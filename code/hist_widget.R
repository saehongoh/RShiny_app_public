suppressMessages(library(lubridate))
suppressMessages(library(data.table))
suppressMessages(library(plotly))
suppressMessages(library(dplyr))
suppressMessages(library(knitr))
linebreaks <- function(n){HTML(strrep(br(), n))}


if(Sys.getenv()[[10]] == "/Users/eoh"){
  work_dir = "/Users/eoh/Documents/R_projects/buythedip/dashboard/"
  # dir = paste0(work_dir,"data_srv")
} else {
  work_dir = "/srv/shiny-server/dashboard/"
  # dir = paste0(work_dir,"data")
}

print(work_dir)
dir = paste0(work_dir,"data_rds")
source(paste0(work_dir,"code/fns.R"))

getDataLean <- function(input, created){
  
  if(grepl("V3|V2", input)){
    
    readRDS(input) %>%
      # select(-V1) %>%
      mutate(coin_id = gsub("r/cc moons","moon",coin_id)) %>%
      select_if(., !grepl("level",colnames(.))) %>%
      mutate(datetime = created) %>%
      # left_join(., cryptodigits, by="coin_id") %>%
      select(datetime, everything()) %>%
      # group_by(coin_id) %>%
      # mutate(mkt_cap = (mkt.cap - mean(mkt.cap))/mean(mkt.cap)) %>%
      # ungroup() %>%
      mutate(z.vol = (current.volume-vol.14d.mean)/vol.sd,
             vol_volatility = abs((vol.sd)/vol.14d.mean*100)) %>%
      mutate(symbol = toupper(symbol)) %>%
      mutate(delta = (current.price - vol.adj.14d.mean)/vol.adj.14d.mean) %>%
      # mutate(volatility = volatility + 3) %>%
      # mutate(vol_volatility = vol_volatility/2) %>%  
      select(coin_id, symbol, vol.adj.14d.mean, datetime, current.price, z.vol, z.score) %>%
      mutate(delta = (current.price - vol.adj.14d.mean)/vol.adj.14d.mean) %>%
      mutate(symbol = toupper(symbol)) %>%
      rename(x=datetime, y=delta, color=symbol)
    
  } else {
    
    # readRDS(hist_files$files[2]) %>%
    readRDS(input) %>%
      # select(-V1) %>%
      mutate(coin_id = gsub("r/cc moons","moon",coin_id)) %>%
      select_if(., !grepl("level",colnames(.))) %>%
      mutate(datetime = created) %>%
      left_join(., cryptodigits, by="coin_id") %>%
      select(datetime, everything()) %>%
      # group_by(coin_id) %>%
      # mutate(mkt_cap = (mkt.cap - mean(mkt.cap))/mean(mkt.cap)) %>%
      # ungroup() %>%
      mutate(z.vol = (current.volume-vol.14d.mean)/vol.sd,
             vol_volatility = abs((vol.sd)/vol.14d.mean*100)) %>%
      mutate(symbol = toupper(symbol)) %>%
      mutate(delta = (current.price - vol.adj.14d.mean)/vol.adj.14d.mean) %>%
      # mutate(volatility = volatility + 3) %>%
      # mutate(vol_volatility = vol_volatility/2) %>%  
      select(coin_id, symbol, vol.adj.14d.mean, datetime, current.price,z.vol, z.score) %>%
      mutate(delta = (current.price - vol.adj.14d.mean)/vol.adj.14d.mean) %>%
      mutate(symbol = toupper(symbol)) %>%
      rename(x=datetime, y=delta, color=symbol)
  }
}

#############################################
########### 2 weeks #########################
#############################################

preload_length = days(14)

#### Import initial data

files <- list.files(path=dir, pattern=".rds",full.names = TRUE,recursive = TRUE)

hist_files = data.frame(files, created = as.POSIXct(do.call(rbind, strsplit(files, "tmp_|.rds"))[,3], tz="UTC")) %>%
  arrange(desc(created)) %>%
  filter(created >= Sys.time() - preload_length)  %>%
  slice(which(row_number() %% 5 == 1))

# print(nrow(hist_files))
# hist_files <- hist_files[3:nrow(hist_files),]
# print("start loading data")

tmp <- do.call(rbind, lapply(1:nrow(hist_files), function(x) getDataLean(input=hist_files$files[x], created = hist_files$created[x])))

tmp <- tmp %>%
  arrange(x) 
cryptos = unique(tmp$color)
palette <- gg_color_hue(length(unique(tmp$color)))

print("done loading data")

m <- list(
  l = 25,
  r = 25,
  b = 0,
  t = 0,
  pad = 0
)

p <- plot_ly(type = 'scatter', mode = 'lines+markers', colors=palette,
             # width = "1500", 
             # autosize=TRUE,
             legendgroup = tmp$color
) %>%
  add_lines(x=tmp$x, y=tmp$z.score, line=list(width=1.5, opacity=0.75),
            color=factor(tmp$color, levels=(unique(tmp$color))),
            opacity=0.75, alpha_stroke = 0.25,
            uirevision = FALSE,
            # width="800px",
            colors=palette) %>%
  layout(margin= m,
         xaxis=list(
           # range=c(xmin,xmax), 
           side="top",
           tickangle=0, gridcolor = "#424242",
           title = list(text='', font=list(size=11))),
         # yaxis=list(title=list("z score (price)",font=list(size=10))),
         yaxis=list(tickformat=",.0", title="z score", titlefont=list(size=11)),
         title = list(text='', font=list(size=14)),
         paper_bgcolor='#1C1C1D',
         hoverlabel=list(font=list(color="white")),
         plot_bgcolor='#1C1C1D',
         selectionrevision= FALSE,
         uirevision = FALSE, dragmode=FALSE,
         legend=list(font=list(size=11),
                     # itle=list(text='<b> Click to filter </b>'),
                     orientation="h",
                     yanchor="top",
                     xanchor="center", 
                     y=-0.02,
                     x=0.5),
         font=list(color="#FFFFFF")) %>%
  config(displaylogo = FALSE, showTips = FALSE,
         modeBarButtonsToRemove = c('toImage','toggleSpikelines','zoomIn2d', 'zoomOut2d', 'lasso2d','select2d',
                                    'hoverClosestCartesian','hoverCompareCartesian'))

# p
# p
filename = paste0(work_dir, "www/hist/zprice.html")
htmlwidgets::saveWidget(as_widget(p), background = "#1C1C1D", title = "BuyTheDips", selfcontained = FALSE, filename)
# htmlwidgets::saveWidget(as_widget(p), paste0(work_dir,"www/widget.html"))


p <- plot_ly(type = 'scatter', mode = 'lines+markers', colors=palette,
             # width = "1500",
             legendgroup = tmp$color
) %>%
  add_lines(x=tmp$x, y=tmp$z.vol, line=list(width=1.5, opacity=0.75),
            color=factor(tmp$color, levels=(unique(tmp$color))),
            opacity=0.75, alpha_stroke = 0.25,
            uirevision = FALSE,
            colors=palette) %>%
  layout(margin= m,
         xaxis=list(
           side="top",
           tickangle=0, gridcolor = "#424242",
           title = list(text='', font=list(size=11))),
         # yaxis=list(title=list("z score (price)",font=list(size=10))),
         yaxis=list(tickformat=",.0", title="z score", titlefont=list(size=11)),
         title = list(text='', font=list(size=14)),
         paper_bgcolor='#1C1C1D',
         hoverlabel=list(font=list(color="white")),
         plot_bgcolor='#1C1C1D',
         selectionrevision= FALSE,
         uirevision = FALSE, dragmode=FALSE,
         legend=list(font=list(size=11),
                     # itle=list(text='<b> Click to filter </b>'),
                     orientation="h",
                     yanchor="top",
                     xanchor="center", 
                     y=-0.02,
                     x=0.5),
         font=list(color="#FFFFFF")) %>%
  config(displaylogo = FALSE, showTips = FALSE,
         modeBarButtonsToRemove = c('toImage','toggleSpikelines','zoomIn2d', 'zoomOut2d', 'lasso2d','select2d',
                                    'hoverClosestCartesian','hoverCompareCartesian'))
# p
filename = paste0(work_dir, "www/hist/zvolume.html")
htmlwidgets::saveWidget(as_widget(p), title = "BuyTheDips", selfcontained = FALSE, filename)

######################
p <- plot_ly(type = 'scatter', mode = 'lines+markers', colors=palette,
             # width = "1500",
             legendgroup = tmp$color
) %>%
  add_lines(x=tmp$x, y=tmp$y, line=list(width=1.5, opacity=0.75),
            color=factor(tmp$color, levels=(unique(tmp$color))),
            opacity=0.75, alpha_stroke = 0.25,
            uirevision = FALSE,
            colors=palette) %>%
  layout(margin= m,
         xaxis=list(
           side="top",
           tickangle=0, gridcolor = "#424242",
           title = list(text='', font=list(size=11))),
         # yaxis=list(title=list("z score (price)",font=list(size=10))),
         yaxis=list(tickformat=",.0%", title="% performance", titlefont=list(size=11)),
         title = list(text='', font=list(size=14)),
         paper_bgcolor='#1C1C1D',
         hoverlabel=list(font=list(color="white")),
         plot_bgcolor='#1C1C1D',
         selectionrevision= FALSE,
         uirevision = FALSE, dragmode=FALSE,
         legend=list(font=list(size=11),
                     # itle=list(text='<b> Click to filter </b>'),
                     orientation="h",
                     yanchor="top",
                     xanchor="center", 
                     y=-0.02,
                     x=0.5),
         font=list(color="#FFFFFF")) %>%
  config(displaylogo = FALSE, showTips = FALSE,
         modeBarButtonsToRemove = c('toImage','toggleSpikelines','zoomIn2d', 'zoomOut2d', 'lasso2d','select2d',
                                    'hoverClosestCartesian','hoverCompareCartesian'))

filename = paste0(work_dir, "www/hist/performance.html")
htmlwidgets::saveWidget(as_widget(p), title = "BuyTheDips", selfcontained = FALSE, filename)


###################### Correlation matrix
# test <- tmp %>%
#   filter(!color %in% c("ATOM","MANA","VET"))
test <- tmp

test2 <- test %>%
  select(color, z.score, x) %>%
  reshape2::dcast(x~color, value.var ="z.score") %>%
  select(-x) %>%
  as.matrix(.)

res <- test2[,unique(test$color)] %>%
  Hmisc::rcorr(.)

flattenCorrMatrix <- function(cormat, pmat) {
  ut <- upper.tri(cormat)
  data.frame(
    row = rownames(cormat)[row(cormat)[ut]],
    column = rownames(cormat)[col(cormat)[ut]],
    cor  =(cormat)[ut],
    p = pmat[ut]
  )
}
res2 <- flattenCorrMatrix(res$r, res$P) %>%
  mutate(hov_text = paste0("<b>",row, " vs. ", column, "</b>\npearson's r: ", round(cor, 3)))

p <- plot_ly(
  x=~factor(res2$row, levels=(unique(res2$row))),
  y=~factor(res2$column, levels=rev(unique(test$color))),
  z=~res2$cor, 
  type="heatmap",
  showscale=FALSE,
  hovertext=~res2$hov_text,
  hoverinfo = "text",
  reversescale=FALSE,
  colorscale="cividis"
) %>%
  layout(showlegend=FALSE,
         xaxis=list(title = list(text='', font=list(size=11))),
         yaxis=list(title = list(text='', font=list(size=11))),
         title = list(text='', font=list(size=12)),
         paper_bgcolor='#1C1C1D',
         hoverlabel=list(font=list(color="white")),
         plot_bgcolor='#1C1C1D',  dragmode = FALSE,
         font=list(color="white")
  )  %>%
  config( displaylogo = FALSE, showTips = FALSE,
          modeBarButtonsToRemove = c('toImage','toggleSpikelines','zoomIn2d', 'zoomOut2d', 'lasso2d','select2d',
                                     'hoverClosestCartesian','hoverCompareCartesian'))

filename = paste0(work_dir, "www/hist/corr_zscore.html")
htmlwidgets::saveWidget(as_widget(p), background = "#1C1C1D", title = "BuyTheDips", selfcontained = FALSE, filename)

###################### Correlation matrix
# test <- tmp %>%
#   filter(!color %in% c("ATOM","MANA","VET"))
test <- tmp

test2 <- test %>%
  select(color, z.vol, x) %>%
  reshape2::dcast(x~color, value.var ="z.vol") %>%
  select(-x) %>%
  as.matrix(.)

res <- test2[,unique(test$color)] %>%
  Hmisc::rcorr(.)

flattenCorrMatrix <- function(cormat, pmat) {
  ut <- upper.tri(cormat)
  data.frame(
    row = rownames(cormat)[row(cormat)[ut]],
    column = rownames(cormat)[col(cormat)[ut]],
    cor  =(cormat)[ut],
    p = pmat[ut]
  )
}
res2 <- flattenCorrMatrix(res$r, res$P) %>%
  mutate(hov_text = paste0("<b>",row, " vs. ", column, "</b>\npearson's r: ", round(cor, 3)))

p <- plot_ly(
  x=~factor(res2$row, levels=(unique(res2$row))),
  y=~factor(res2$column, levels=rev(unique(test$color))),
  z=~res2$cor, 
  type="heatmap",
  showscale=FALSE,
  hovertext=~res2$hov_text,
  hoverinfo = "text",
  reversescale=FALSE,
  colorscale="cividis"
) %>%
  layout(showlegend=FALSE,
         xaxis=list(title = list(text='', font=list(size=11))),
         yaxis=list(title = list(text='', font=list(size=11))),
         title = list(text='', font=list(size=12)),
         paper_bgcolor='#1C1C1D',
         hoverlabel=list(font=list(color="white")),
         plot_bgcolor='#1C1C1D',  dragmode = FALSE,
         font=list(color="white")
  )  %>%
  config( displaylogo = FALSE, showTips = FALSE,
          modeBarButtonsToRemove = c('toImage','toggleSpikelines','zoomIn2d', 'zoomOut2d', 'lasso2d','select2d',
                                     'hoverClosestCartesian','hoverCompareCartesian'))

filename = paste0(work_dir, "www/hist/corr_zvolume.html")
htmlwidgets::saveWidget(as_widget(p), background = "#1C1C1D", title = "BuyTheDips", selfcontained = FALSE, filename)


#############################################
########### 4 weeks #########################
#############################################

# preload_length = days(7*8)

#### Import initial data

files <- list.files(path=dir, pattern=".rds",full.names = TRUE,recursive = TRUE)

slice_factor = round(length(files)/746,0)

hist_files = data.frame(files, created = as.POSIXct(do.call(rbind, strsplit(files, "tmp_|.rds"))[,3], tz="UTC")) %>%
  arrange(desc(created)) %>%
  # filter(created >= Sys.time() - preload_length)  %>%
  slice(which(row_number() %% slice_factor == 1))

# print(nrow(hist_files))
# hist_files <- hist_files[3:nrow(hist_files),]
# print("start loading data")

tmp <- do.call(rbind, lapply(1:nrow(hist_files), function(x) getDataLean(input=hist_files$files[x], created = hist_files$created[x])))

tmp <- tmp %>%
  arrange(x) 

p <- plot_ly(type = 'scatter', mode = 'lines+markers', colors=palette,
             # width = "1500", 
             # autosize=TRUE,
             legendgroup = tmp$color
) %>%
  add_lines(x=tmp$x, y=tmp$z.score, line=list(width=1.5, opacity=0.75),
            color=factor(tmp$color, levels=(unique(tmp$color))),
            opacity=0.75, alpha_stroke = 0.25,
            uirevision = FALSE,
            # width="800px",
            colors=palette) %>%
  layout(margin= m,
         xaxis=list(
           side="top",
           tickangle=0, gridcolor = "#424242",
           title = list(text='', font=list(size=11))),
         # yaxis=list(title=list("z score (price)",font=list(size=10))),
         yaxis=list(tickformat=",.0", title="z score", titlefont=list(size=11)),
         title = list(text='', font=list(size=14)),
         paper_bgcolor='#1C1C1D',
         hoverlabel=list(font=list(color="white")),
         plot_bgcolor='#1C1C1D',
         selectionrevision= FALSE,
         uirevision = FALSE, dragmode=FALSE,
         legend=list(font=list(size=11),
                     # itle=list(text='<b> Click to filter </b>'),
                     orientation="h",
                     yanchor="top",
                     xanchor="center", 
                     y=-0.02,
                     x=0.5),
         font=list(color="#FFFFFF")) %>%
  config(displaylogo = FALSE, showTips = FALSE,
         modeBarButtonsToRemove = c('toImage','toggleSpikelines','zoomIn2d', 'zoomOut2d', 'lasso2d','select2d',
                                    'hoverClosestCartesian','hoverCompareCartesian'))

# p
# p
filename = paste0(work_dir, "www/hist/zprice_max.html")
htmlwidgets::saveWidget(as_widget(p), background = "#1C1C1D", title = "BuyTheDips", selfcontained = FALSE, filename)
# htmlwidgets::saveWidget(as_widget(p), paste0(work_dir,"www/widget.html"))


p <- plot_ly(type = 'scatter', mode = 'lines+markers', colors=palette,
             # width = "1500",
             legendgroup = tmp$color
) %>%
  add_lines(x=tmp$x, y=tmp$z.vol, line=list(width=1.5, opacity=0.75),
            color=factor(tmp$color, levels=(unique(tmp$color))),
            opacity=0.75, alpha_stroke = 0.25,
            uirevision = FALSE,
            colors=palette) %>%
  layout(margin= m,
         xaxis=list(
           side="top",
           tickangle=0, gridcolor = "#424242",
           title = list(text='', font=list(size=11))),
         # yaxis=list(title=list("z score (price)",font=list(size=10))),
         yaxis=list(tickformat=",.0", title="z score", titlefont=list(size=11)),
         title = list(text='', font=list(size=14)),
         paper_bgcolor='#1C1C1D',
         hoverlabel=list(font=list(color="white")),
         plot_bgcolor='#1C1C1D',
         selectionrevision= FALSE,
         uirevision = FALSE, dragmode=FALSE,
         legend=list(font=list(size=11),
                     # itle=list(text='<b> Click to filter </b>'),
                     orientation="h",
                     yanchor="top",
                     xanchor="center", 
                     y=-0.02,
                     x=0.5),
         font=list(color="#FFFFFF")) %>%
  config(displaylogo = FALSE, showTips = FALSE,
         modeBarButtonsToRemove = c('toImage','toggleSpikelines','zoomIn2d', 'zoomOut2d', 'lasso2d','select2d',
                                    'hoverClosestCartesian','hoverCompareCartesian'))
# p
filename = paste0(work_dir, "www/hist/zvolume_max.html")
htmlwidgets::saveWidget(as_widget(p), title = "BuyTheDips", selfcontained = FALSE, filename)

######################
p <- plot_ly(type = 'scatter', mode = 'lines+markers', colors=palette,
             # width = "1500",
             legendgroup = tmp$color
) %>%
  add_lines(x=tmp$x, y=tmp$y, line=list(width=1.5, opacity=0.75),
            color=factor(tmp$color, levels=(unique(tmp$color))),
            opacity=0.75, alpha_stroke = 0.25,
            uirevision = FALSE,
            colors=palette) %>%
  layout(margin= m,
         xaxis=list(
           side="top",
           tickangle=0, gridcolor = "#424242",
           title = list(text='', font=list(size=11))),
         # yaxis=list(title=list("z score (price)",font=list(size=10))),
         yaxis=list(tickformat=",.0%", title="% performance", titlefont=list(size=11)),
         title = list(text='', font=list(size=14)),
         paper_bgcolor='#1C1C1D',
         hoverlabel=list(font=list(color="white")),
         plot_bgcolor='#1C1C1D',
         selectionrevision= FALSE,
         uirevision = FALSE, dragmode=FALSE,
         legend=list(font=list(size=11),
                     # itle=list(text='<b> Click to filter </b>'),
                     orientation="h",
                     yanchor="top",
                     xanchor="center", 
                     y=-0.02,
                     x=0.5),
         font=list(color="#FFFFFF")) %>%
  config(displaylogo = FALSE, showTips = FALSE,
         modeBarButtonsToRemove = c('toImage','toggleSpikelines','zoomIn2d', 'zoomOut2d', 'lasso2d','select2d',
                                    'hoverClosestCartesian','hoverCompareCartesian'))

filename = paste0(work_dir, "www/hist/performance_max.html")
htmlwidgets::saveWidget(as_widget(p), title = "BuyTheDips", selfcontained = FALSE, filename)
