# Master
# clone_name = "m"
if(length(unlist(strsplit(getwd(),"dashboard_"))) > 1){
  clone_name = unlist(strsplit(getwd(),"dashboard_"))[[2]]
} else {
  clone_name = "m"
}
max_filter = 12

suppressMessages(library(shiny))
suppressMessages(library(shinyMobile))
suppressMessages(library(shinyjs))
suppressMessages(library(plotly))
suppressMessages(library(ggplot2))
suppressMessages(library(lubridate))
suppressMessages(library(dplyr))
suppressMessages(library(data.table))
suppressMessages(library(shinyWidgets))
suppressMessages(library(shinycssloaders))
suppressMessages(library(waiter))
suppressMessages(library(viridis))
# suppressMessages(library(DT))
suppressMessages(library(reactable))

#### Directory setup
if(Sys.getenv()[[10]] == "/Users/eoh"){
  work_dir = "/Users/eoh/Documents/R_projects/buythedip/dashboard/"
  #### For historical zscore
  preload_length = days(4)
  refresh_hours = days(4)
  # dir = paste0(work_dir,"data_srv")
} else {
  work_dir = "/srv/shiny-server/dashboard/"
  preload_length = days(2)
  refresh_hours = days(2)
}

dir = paste0(work_dir,"data_rds")
newdir = paste0(work_dir,"newdata")
source(paste0(work_dir,"code/fns.R"))

#### For volatility quantile
volatility_calculation = days(14)

#### Import initial data

getData <- function(input){
  readRDS(input) 
}

getVol <- function(input, filter_cryptos=NULL){
  readRDS(input) %>%
    select(coin_id, vol.14d.mean, vol.sd, volatility) %>%
    mutate(vol_volatility = abs((vol.sd)/vol.14d.mean*100)) %>%
    select(coin_id, volatility, vol_volatility) 
}


getMole <- function(tmp){
  tmp %>%
    mutate(level1.dip = vol.adj.14d.mean-(wtd.sd*1),
           level2.dip = vol.adj.14d.mean-(wtd.sd*2),
           level3.dip = vol.adj.14d.mean-(wtd.sd*3),
           level4.dip = vol.adj.14d.mean-(wtd.sd*4),
           level5.dip = vol.adj.14d.mean-(wtd.sd*5)) %>%
    mutate(level1.spicydip = current.price-(wtd.sd*1),
           level2.spicydip = current.price-(wtd.sd*1.5),
           level3.spicydip = current.price-(wtd.sd*2),
           level4.spicydip = current.price-(wtd.sd*2.5),
           level5.spicydip = current.price-(wtd.sd*3)) %>%
    mutate_if(grepl("price|level",colnames(.)), list(~ifelse(. < 0, 0, .))) %>%
    mutate_if(grepl("price|level",colnames(.)), list(~as.character(ifelse(symbol == "SHIB", paste0("$",signif(., digs)), paste0("$", round(., digs)))))) %>%
    mutate_if(grepl("price|level",colnames(.)), list(~as.character(ifelse(symbol %in% c("BTC","ETH"), format.money(gsub("[$]","",.)), .)))) %>%
    mutate_if(grepl("price|level",colnames(.)), list(~ifelse(.=="$0", "$zero", .))) %>%
    mutate(hov_text_price = paste0(hov_text_z, "\n \nMole's tasty ", paste0("$",toupper(symbol)), " dips!\n",
                                   "Ketchup (z=-1): ", level1.dip," USD\n",
                                   "Hummus (z=-2): ", level2.dip," USD\n",
                                   "Tzatziki (z=-3): ", level3.dip," USD\n",
                                   "Guacamole (z=-4): ", level4.dip," USD\n",
                                   "Fondue (z=-5): ", level5.dip," USD\n"))
}

m <- list(l = 0, r = 0, b = 0, t = 30, pad = 0)
linebreaks <- function(n){HTML(strrep(br(), n))}

plot_hist_zprice <- function(tmp, xmin, xmax, palette_default){
  plot_ly(type = 'scatter', mode = 'lines', colors=palette_default) %>%
    add_trace(x=tmp$datetime, y=tmp$z.score,
              line=list(width=1.25, opacity=0.75),
              color=factor(tmp$symbol, levels=(unique(tmp$symbol))),
              opacity=0.75, alpha_stroke = 0.25,
              # visible="legendonly",
              colors=palette_default) %>%
    layout(margin= m,
           xaxis=list(range=c(xmin,xmax), tickangle=0, gridcolor = "#424242",
                      # uirevision=~tmp$datetime,
                      title = list(text='time (UTC)', font=list(size=11))),
           yaxis=list(tickformat = ".1",
                      tickvals = seq(-5, 5, .5),
                      ticktext = sprintf("%.1f", seq(-5,5,0.5)),
                      title="z score (price)\n ", titlefont=list(size=11)),
           title = list(text='<b>z-score movements in the last 48 hours </b>', font=list(size=11)),
           paper_bgcolor='#1C1C1D',
           hoverlabel=list(font=list(color="white")),
           plot_bgcolor='#1C1C1D',
           # uirevision = "xaxis",
           font=list(color="#FFFFFF")) %>%
    config(displaylogo = FALSE, showTips = FALSE,
           modeBarButtonsToRemove = c('toImage','toggleSpikelines','zoomIn2d', 'zoomOut2d', 'lasso2d','select2d',"autoScale2d",
                                      # 'zoom2d',
                                      'hoverClosestCartesian','hoverCompareCartesian')) 
}

plot_price <- function(tmp, palette_default){
  plot_ly(type="scatter", mode="markers", 
          x = ~tmp$z.score_lim, 
          y = ~factor(tmp$symbol, levels=rev(unique(tmp$symbol))),
          color = ~factor(tmp$symbol, levels=(unique(tmp$symbol))), 
          colors = palette_default, showlegend=F,
          hoverinfo = "text",
          text=~tmp$hov_text_price,
          size=1, alpha = 1) %>%
    add_trace(x = ~tmp$z.score_lim, y = ~factor(tmp$symbol, levels=rev(unique(tmp$symbol))),
              text=~tmp$hov_text_price,
              marker = list(size= ~(tmp$volatility+10), opacity=0.5, text=~tmp$hov_text_price,
                            line = list(color = '#202021', width = 1)),
              showlegend = F) %>%
    layout(margin= m, 
           xaxis=list(range=c(-5,5), showgrid=FALSE, 
                      title = list(text='', font=list(size=11)),
                      tickvals=list(-3.75,-3,-2.1,-2,-1,0,1,2,2.1,3,3.75),
                      tickangle=0, tickfont=list(size=10),
                      ticktext=list(" \nbig dip\n(best deal)","-3"," \ndip\n(good deal)","-2","-1","0\nneutral\n(regular price)",
                                    "1","2"," \npeak\n(premium)","3"," \nbig peak\n(high premium)")),
           yaxis=list(title='', gridcolor = "#424242"),
           shapes=list(vline(0,"#898ba1"),vline(2),vline(3),vline(-2,"red"),vline(-3,"red")),
           title = list(text='<b>z-scores for current price</b>', font=list(size=11)), 
           paper_bgcolor='#1C1C1D',
           hoverlabel=list(font=list(color="white")),
           plot_bgcolor='#1C1C1D',  dragmode = FALSE,
           font=list(color="#FFFFFF"))  %>%
    config( displaylogo = FALSE, showTips = FALSE, 
            modeBarButtonsToRemove = c('toImage','toggleSpikelines','zoomIn2d', 'zoomOut2d', 'lasso2d','select2d',
                                       'pan2d','zoom2d','resetScale2d',"autoScale2d",
                                       'hoverClosestCartesian','hoverCompareCartesian'))
}

plot_volume <- function(tmp, palette_default){
  plot_ly(type="scatter",mode="markers", 
          x=~tmp$z.vol_lim, 
          y=~factor(tmp$symbol, levels=rev(unique(tmp$symbol))),
          color = ~factor(tmp$symbol, levels=(unique(tmp$symbol))), colors = palette_default, showlegend=F,
          hoverinfo = "text",
          text=~tmp$hov_text_vol,
          size=1, alpha = 1) %>%
    add_trace(x = ~tmp$z.vol_lim, y = ~factor(tmp$symbol, levels=rev(unique(tmp$symbol))),
              text=~tmp$hov_text_vol,
              marker = list(size= ~tmp$volatility+10, opacity=0.5, text=~tmp$hov_text_vol,
                            line = list(color = '#202021', width = 1)),
              showlegend = F) %>%
    layout(margin= m, 
           xaxis=list(range=c(-5,5), showgrid=FALSE, 
                      title = list(text='', font=list(size=11)),
                      tickvals=list(-3.75,-3,-2.1,-2,-1,0,1,2,2.1,3,3.75),
                      tickangle=0, tickfont=list(size=10),
                      ticktext=list(" \nvery\nlow volume","-3"," \nlow\nvolume","-2","-1","0\nregular\nvolume",
                                    "1","2"," \nhigh\nvolume","3"," \nvery\nhigh volume")),
           yaxis=list(title='', gridcolor = "#424242"),
           shapes=list(vline(0,"#898ba1"),vline(2),vline(3),vline(-2,"red"),vline(-3,"red")),
           title = list(text='<b>z-scores for current trading volume</b>', font=list(size=11)), 
           paper_bgcolor='#1C1C1D',
           hoverlabel=list(font=list(color="white")),
           plot_bgcolor='#1C1C1D',  dragmode = FALSE,
           font=list(color="#FFFFFF"))  %>%
    config( displaylogo = FALSE, showTips = FALSE, 
            modeBarButtonsToRemove = c('toImage','toggleSpikelines','zoomIn2d', 'zoomOut2d', 'lasso2d','select2d',
                                       'pan2d','zoom2d','resetScale2d',"autoScale2d",
                                       'hoverClosestCartesian','hoverCompareCartesian'))
  
}

plot_scatter <- function(tmp, palette_default){
  plot_ly(type="scatter",mode="markers", 
          x=~tmp$z.score_lim, 
          y=~tmp$z.vol_lim,
          color = ~factor(tmp$symbol, levels=(unique(tmp$symbol))), colors = palette_default, 
          hoverinfo = "text",
          text=~tmp$hov_text_scat,
          size=2, alpha = 0.75) %>%
    add_trace(x = ~tmp$z.score_lim,  y = ~tmp$z.vol_lim, text=~tmp$hov_text_scat, showlegend=F,
              marker = list(size= 20, opacity=0.75, text=~tmp$hov_text_scat, 
                            line = list(color = '#202021', width = 1))) %>%
    layout(margin= m, 
           yaxis=list(range=c(-5,5), showgrid=FALSE, 
                      title = list(text='', font=list(size=11)),
                      tickvals=list(-3.75,-3,-2.1,-2,-1,0,1,2,2.1,3,3.75),
                      tickangle=0, tickfont=list(size=10),
                      ticktext=list(" \nvery low\nvolume","-3"," ","low -2","-1","regular 0",
                                    "1","high 2"," ","3","very high\nvolume")),
           xaxis=list(range=c(-5,5), showgrid=FALSE, 
                      title = list(text='', font=list(size=11)),
                      tickvals=list(-3.75,-3,-2.1,-2,-1,0,1,2,2.1,3,3.75),
                      tickangle=0, tickfont=list(size=10),
                      ticktext=list(" \nbig dip\n(best deal)","-3"," ","-2","-1","0\nneutral\n(regular price)",
                                    "1","2","  ","3"," \nbig peak\n(high premium)")),
           shapes=list(
             vline(3),vline(-3,"red"),
             hline(3),hline(-3,"red"),
             list(type = "rect",line = list(color = "#ffffff", opacity=0.25, dash="dot"),
                  x0 = -2, x1 = 2, y0=-2,y1=2)),
           title = list(text='<b>scatter plot z-volume vs. z-price</b>', font=list(size=11)), 
           paper_bgcolor='#1C1C1D',
           hoverlabel=list(font=list(color="white")),
           plot_bgcolor='#1C1C1D',  dragmode = FALSE,
           font=list(color="#FFFFFF"))  %>%
    config( displaylogo = FALSE, showTips = FALSE,
            modeBarButtonsToRemove = c('toImage','toggleSpikelines','zoomIn2d', 'zoomOut2d', 'lasso2d','select2d',
                                       'pan2d','zoom2d','resetScale2d',"autoScale2d",
                                       'hoverClosestCartesian','hoverCompareCartesian'))
}

plot_vol_price <- function(tmp, palette_default){
  plot_ly(type="bar", 
          x=~factor(tmp$symbol, levels=(unique(tmp$symbol))), 
          y=~tmp$volatility/100,
          color = ~factor(tmp$symbol, levels=(unique(tmp$symbol))), 
          colors = palette_default,
          showlegend=F,
          size=1, alpha = 1) %>%
    layout(margin= m, 
           xaxis=list(title = list(text='', font=list(size=11))),
           yaxis=list(tickformat=",.0%", title='', gridcolor = "#424242"),
           title = list(text='<b>volatility %in% price</b>', font=list(size=11)),
           paper_bgcolor='#1C1C1D',
           hoverlabel=list(font=list(color="white")),
           plot_bgcolor='#1C1C1D',  dragmode = FALSE,
           font=list(color="#FFFFFF"))  %>%
    config( displaylogo = FALSE, showTips = FALSE, 
            modeBarButtonsToRemove = c('toImage','toggleSpikelines','zoomIn2d', 'zoomOut2d', 'lasso2d','select2d',
                                       'pan2d','zoom2d','resetScale2d',"autoScale2d",
                                       'hoverClosestCartesian','hoverCompareCartesian'))
}

plot_vol_volume <- function(tmp, palette_default){
  plot_ly(type="bar", 
          x=~factor(tmp$symbol, levels=(unique(tmp$symbol))), 
          y=~tmp$vol_volatility/100,
          color = ~factor(tmp$symbol, levels=(unique(tmp$symbol))), 
          colors = palette_default,
          showlegend=F,
          size=1, alpha = 1) %>%
    layout(margin= m, 
           xaxis=list(title = list(text='', font=list(size=11))),
           yaxis=list(tickformat=",.0%", title='', gridcolor = "#424242"),
           title = list(text='<b>volatility %in% trading volume</b>', font=list(size=11)),
           paper_bgcolor='#1C1C1D',
           hoverlabel=list(font=list(color="white")),
           plot_bgcolor='#1C1C1D',  dragmode = FALSE,
           font=list(color="#FFFFFF"))  %>%
    config( displaylogo = FALSE, showTips = FALSE, 
            modeBarButtonsToRemove = c('toImage','toggleSpikelines','zoomIn2d', 'zoomOut2d', 'lasso2d','select2d',
                                       'pan2d','zoom2d','resetScale2d',"autoScale2d",
                                       'hoverClosestCartesian','hoverCompareCartesian'))
}

plot_scatter_vol <- function(tmp, palette_default, q_price, q_vol){
  plot_ly(type="scatter",mode="markers", 
          x=~tmp$vol_volatility, 
          y=~tmp$volatility,
          color = ~factor(tmp$symbol, levels=(unique(tmp$symbol))), colors = palette_default, 
          hoverinfo = "text",
          text=~tmp$hov_text_scat_vol,
          size=2, alpha = 0.75) %>%
    add_trace(x = ~tmp$vol_volatility,  y = ~tmp$volatility, text=~tmp$hov_text_scat_vol,
              showlegend=F,
              marker = list(size= 20, opacity=0.75,
                            line = list(color = '#202021', width = 1))) %>%
    layout(margin= m,
           xaxis=list(title='% volatility in trading volume', titlefont=list(size=11)),
           yaxis=list(title='% volatility in price\n', titlefont=list(size=11)),
           shapes=list(
             # hline(q_price[2],"orange"),hline(q_price[4],"orange"),  vline(q_vol[2],"orange"),vline(q_vol[4],"orange")
             list(type = "rect", fillcolor="white", opacity=0.1, line=list(color="#fff",size=0.1),
                  x0 = 0, x1 = ~max(tmp$vol_volatility)+5, y0=q_price[2],y1=q_price[4]),
             list(type = "rect", fillcolor="white", opacity=0.1, line=list(color="#fff",size=0.1),
                  y0 = 0, y1 = ~max(tmp$volatility)+5, x0=q_vol[2],x1=q_vol[4])
           ),
           title = list(text='<b>scatter %in% volatility</b>', font=list(size=11)), 
           paper_bgcolor='#1C1C1D',
           hoverlabel=list(font=list(color="white")),
           plot_bgcolor='#1C1C1D',  dragmode = FALSE,
           font=list(color="#FFFFFF"))  %>%
    config( displaylogo = FALSE, showTips = FALSE,
            modeBarButtonsToRemove = c('toImage','toggleSpikelines','zoomIn2d', 'zoomOut2d', 'lasso2d','select2d',
                                       'pan2d','zoom2d','resetScale2d',"autoScale2d",
                                       'hoverClosestCartesian','hoverCompareCartesian'))
}

plot_performance <- function(tmp, palette_default){
  plot_ly(type="bar", 
          x=~factor(tmp$symbol, levels=(unique(tmp$symbol))), 
          y=~round(tmp$per.diff/100,2),
          color = ~factor(tmp$symbol, levels=(unique(tmp$symbol))), 
          colors = palette_default,
          # hoverinfo = "text",
          # hovertext=~tmp$price_text,
          showlegend=F,
          size=1, alpha = 1) %>%
    layout(margin= m, 
           xaxis=list(title = list(text='', font=list(size=11))),
           yaxis=list(tickformat=",.0%", title='', gridcolor = "#424242"),
           title = list(text='<b>current performance</b>', font=list(size=11)),
           paper_bgcolor='#1C1C1D',
           hoverlabel=list(font=list(color="white")),
           plot_bgcolor='#1C1C1D',  dragmode = FALSE,
           font=list(color="#FFFFFF"))  %>%
    config( displaylogo = FALSE, showTips = FALSE, 
            modeBarButtonsToRemove = c('toImage','toggleSpikelines','zoomIn2d', 'zoomOut2d', 'lasso2d','select2d',
                                       'pan2d','zoom2d','resetScale2d',"autoScale2d",
                                       'hoverClosestCartesian','hoverCompareCartesian'))
}

plot_dominance <- function(tmp, palette_default){
  plot_ly(type="bar",
          x=~factor(tmp$symbol, levels=(unique(tmp$symbol))),
          y=~round(tmp$dom_gain,7),
          color = ~factor(tmp$symbol, levels=(unique(tmp$symbol))),
          colors = palette_default,
          hoverinfo = "text",
          hovertext=~tmp$dom_text,
          showlegend=F,
          size=1, alpha = 1) %>%
    layout(margin= m,
           xaxis=list(title = list(text='', font=list(size=11))),
           # yaxis=list(tickformat=",.0%", title=list(text='delta % dominance\n', font=list(size=11)), gridcolor = "#424242"),
           yaxis=list(tickformat="", title=list(text='fold change against bitcoin\n', font=list(size=11)), gridcolor = "#424242"),
           title = list(text='<b>market cap dominance</b>', font=list(size=11)),
           paper_bgcolor='#1C1C1D',
           hoverlabel=list(font=list(color="white")),
           plot_bgcolor='#1C1C1D',  dragmode = FALSE,
           font=list(color="#FFFFFF"))  %>%
    config( displaylogo = FALSE, showTips = FALSE,
            modeBarButtonsToRemove = c('toImage','toggleSpikelines','zoomIn2d', 'zoomOut2d', 'lasso2d','select2d',
                                       'pan2d','zoom2d','resetScale2d',"autoScale2d",
                                       'hoverClosestCartesian','hoverCompareCartesian'))
}

plot_day_since <- function(days_since, d1, d2, tmp, palette_default){
  plot_ly(
    y=~factor(days_since$symbol, levels=rev(unique(tmp$symbol))),
    x=~days_since$action, 
    z=~days_since$days_since, 
    type="heatmap",
    showscale=FALSE,
    # text = ~days_since$empty_text,
    hovertext=~days_since$hov_text,
    hoverinfo = "text",
    reversescale=TRUE,
    # colorscale = viridis(10),
    height=600
  ) %>%
    add_annotations(x =~days_since$action, y =~days_since$symbol, text="", showarrow = FALSE, ax = 20, ay = -20) %>%
    add_annotations(x =~d1$action, y =~d1$symbol, text=d1$text,
                    showarrow = FALSE, font=list(color="white"), ax = 20, ay = -20) %>%
    add_annotations(x =~d2$action, y =~d2$symbol, text=d2$text, 
                    showarrow = FALSE, font=list(color="black"), ax = 20, ay = -20) %>%
    layout(showlegend=FALSE,
           xaxis=list(side="top", title = list(text='', font=list(size=11))),
           yaxis=list(title = list(text='', font=list(size=11))),
           title = list(text='', font=list(size=11)),
           paper_bgcolor='#1C1C1D',
           hoverlabel=list(font=list(color="white")),
           plot_bgcolor='#1C1C1D',  dragmode = FALSE,
           font=list(color="white")
    )  %>%
    config( displaylogo = FALSE, showTips = FALSE,
            modeBarButtonsToRemove = c('toImage','toggleSpikelines','zoomIn2d', 'zoomOut2d', 'lasso2d','select2d',
                                       'pan2d','zoom2d','resetScale2d',"autoScale2d",
                                       'hoverClosestCartesian','hoverCompareCartesian'))
}


function(input, output, session) {
  # suppress warnings  
  storeWarn<- getOption("warn")
  # options(warn = -1)
  options(warn = -1, expressions = 500000)
  # observe({
  #   f7Notif(title="Welcome to The Dip Index",
  #           text="try the light version @ buythedips.io/lite/",
  #           closeTimeout = 10000000)
  # })
  
  ####################### historical preload
  files <- list.files(path=dir, pattern=".rds",full.names = TRUE,recursive = TRUE)
  
  hist_files = data.frame(files, created = as.POSIXct(do.call(rbind, strsplit(files, "tmp_|.rds"))[,3], tz="UTC")) %>%
    arrange(desc(created)) %>%
    filter(created >= Sys.time() - preload_length)  %>%
    slice(which(row_number() %% 6 == 1))
  
  init_dat <- hist_files$files %>% purrr::map_df(~getData(.)) %>%
    arrange(datetime) 
  
  cryptos = unique(init_dat$symbol)
  palette_default <- gg_color_hue(length(cryptos))
  
  # ####################### quantile calculations
  # tmp_files = data.frame(files, created = as.POSIXct(do.call(rbind, strsplit(files, "tmp_|.rds"))[,3], tz="UTC")) %>%
  #   arrange(desc(created)) %>%
  #   filter(created >= Sys.time() - volatility_calculation) %>%
  #   slice(which(row_number() %% 12 == 1))
  # q_dat <- tmp_files$files %>% purrr::map_df(~getVol(.)) 
  # 
  # q_price <- as.numeric(quantile(q_dat$volatility))
  # q_vol <- as.numeric(quantile(q_dat$vol_volatility))
  # rm(q_dat)
  
  #### Make reactive data
  
  data <- reactivePoll(2000, session,
                       checkFunc = function(){
                         (fileSnapshot(newdir)$info$mtime)
                         # newfile <- paste0(newdir, "/data.rds")
                       },
                       valueFunc = function(){
                         newfile <- paste0(newdir, "/", rownames(fileSnapshot(newdir)$info)[which.max(fileSnapshot(newdir)$info$mtime)])
                         # newfile <- paste0(newdir, "/data.rds")
                         getData(newfile)
                         # filter(symbol %in% previousSelection)
                       }
  )
  
  ######################
  ##### Part 1: Live price update
  ######################
  
  myData <- reactiveValues(datetime = init_dat$datetime,
                           z.score = init_dat$z.score,
                           symbol=init_dat$symbol,
                           xmin = max(init_dat$datetime) - refresh_hours,
                           xmax = max(init_dat$datetime) + minutes(10)
  )
  
  observe({
    diff <- data()
    diff <- diff[c(previousSelection()),]
    myData$datetime <- isolate(c(myData$datetime, diff$datetime))
    myData$symbol <- isolate(c(myData$symbol, diff$symbol))
    myData$z.score <- isolate(c(myData$z.score, diff$z.score))
    myData$xmax <- isolate(max(diff$datetime))  + minutes(10)
    myData$xmin <- isolate(myData$xmax - refresh_hours)
  })
  
  
  #### Tab 1: historical z score
  output$hist_zprice <- renderPlotly({
    
    diff <- data() 
    diff <- diff[c(previousSelection()),]
    
    tmp = data.frame(datetime=isolate(c(myData$datetime)),
                     symbol=isolate(c(myData$symbol)),
                     z.score=isolate(c(myData$z.score)),
                     xmin=isolate(c(myData$xmin)),
                     xmax=isolate(c(myData$xmax))
    )
    
    tmp <- tmp %>% filter(symbol %in% as.character(diff$symbol))
    
    xmax = tmp$xmax
    xmin = tmp$xmin

    plot_hist_zprice(tmp, xmin, xmax, palette_default)
  })
  
  
  ######################
  ##### Part 2
  ######################
  
  output$plot_price <- renderPlotly({
    
    tmp <- data()
    tmp <- tmp[c(previousSelection()),]
    tmp <- getMole(tmp)
    
    output$last_updated1 <- renderText({paste0("",unique(tmp$datetime)," UTC")})
    output$last_updated2 <- renderText({paste0("",unique(tmp$datetime)," UTC")})
    output$last_updated3 <- renderText({paste0("",unique(tmp$datetime)," UTC")})
    output$last_updated5 <- renderText({paste0("",unique(tmp$datetime)," UTC")})
   
    plot_price(tmp, palette_default)
    
  })
  
  
  output$plot_volume <- renderPlotly({
    
    tmp <- data() 
    tmp <- tmp[c(previousSelection()),]
    
    plot_volume(tmp, palette_default)
   
    
  })
  
  output$plot_scatter <- renderPlotly({
    
    tmp <- data() 
    tmp <- tmp[c(previousSelection()),]
    
    plot_scatter(tmp, palette_default)
    
  })
  
  vol_file <- paste0(newdir, "/", rownames(fileSnapshot(newdir)$info)[which.max(fileSnapshot(newdir)$info$mtime)])
  
  output$plot_vol_price <- renderPlotly({
    
    tmp <- readRDS(vol_file)
    plot_vol_price(tmp, palette_default)
    
  })
  
  output$plot_vol_volume <- renderPlotly({
    
    tmp <- readRDS(vol_file)
    plot_vol_volume(tmp, palette_default)
    
  })
  
  # output$plot_scatter_vol <- renderPlotly({
  #   
  #   tmp <- readRDS(vol_file)
  #   plot_ly(type="scatter",mode="markers",
  #           x=~tmp$vol_volatility,
  #           y=~tmp$volatility,
  #           color = ~factor(tmp$symbol, levels=(unique(tmp$symbol))), colors = palette_default,
  #           hoverinfo = "text",
  #           text=~tmp$hov_text_scat_vol,
  #           size=2, alpha = 0.75) %>%
  #     add_trace(x = ~tmp$vol_volatility,  y = ~tmp$volatility, text=~tmp$hov_text_scat_vol,
  #               showlegend=F,
  #               marker = list(size= 20, opacity=0.75,
  #                             line = list(color = '#202021', width = 1))) %>%
  #     layout(
  #       xaxis=list(title='% volatility in trading volume', titlefont=list(size=11)),
  #       yaxis=list(title='% volatility in price\n', titlefont=list(size=11)),
  #       title = list(text='<b>scatter %in% volatility</b>', font=list(size=11)),
  #       paper_bgcolor='#1C1C1D',
  #       hoverlabel=list(font=list(color="white")),
  #       plot_bgcolor='#1C1C1D',  dragmode = FALSE,
  #       font=list(color="#FFFFFF"))  %>%
  #     config( displaylogo = FALSE, showTips = FALSE,
  #             modeBarButtonsToRemove = c('toImage','toggleSpikelines','zoomIn2d', 'zoomOut2d', 'lasso2d','select2d',
  #                                        'pan2d','zoom2d','resetScale2d',"autoScale2d",
  #                                        'hoverClosestCartesian','hoverCompareCartesian'))
  #   
  # })  
  
  output$performance <- renderPlotly({
    
    tmp <- data() 
    tmp <- tmp[c(previousSelection()),]

    plot_performance(tmp, palette_default)
    
  })  
  
  output$dominance <- renderPlotly({
    
    avg_doms <- init_dat %>%
      group_by(coin_id, symbol, digs) %>%
      summarise(avg_dom = mean(dominance, na.rm=TRUE), .groups="drop")
    
    tmp <- data()  %>%
      select(coin_id, symbol, digs, dominance) %>%
      rename(new_dom = dominance) %>%
      left_join(., avg_doms,  by = c("coin_id", "symbol", "digs"))
    
    tmp <- tmp[c(previousSelection()),]
    
    tmp <- tmp %>%
      mutate(dominance_delta = (new_dom - avg_dom)/avg_dom) %>%
      mutate(dominance_delta = ifelse(is.nan(dominance_delta), 0, dominance_delta)) %>%
      mutate(dom_gain = dominance_delta/dominance_delta[1]) %>%
      mutate(dom_text = paste0("<b>", symbol, "</b> |\n<b>current dominance (%)</b>: ",
                               round(new_dom, 2), "\n<b>average dominance (%)</b>: ",
                               round(avg_dom,2), "\n<b>delta (%)</b>: ",
                               round(dominance_delta,3), "\n<b>fold change against bitcoin: </b>: ",
                               round(dom_gain,2), " pts"))
   
    plot_dominance(tmp, palette_default)
    
  })  
  
  output$days_since <- renderPlotly({
    
    days_since <- readRDS(paste0(work_dir, "days_since.rds")) %>% distinct()
    ### Load here
    # just_here_to_invoke_refresh <- data()
    # file <- paste0(dir, "/", rownames(fileSnapshot(dir)$info)[which.max(fileSnapshot(dir)$info$mtime)])
    tmp <- data()
    days_since$dip <- as.numeric(difftime(as.character(Sys.time()), as.character(days_since$dip)), unit="days")
    days_since$moon <- as.numeric(difftime(as.character(Sys.time()), as.character(days_since$moon)), unit="days")
    
    days_since <- days_since %>%
      reshape2::melt(., id = "symbol") %>%
      mutate(value = round(value, 2)) %>%
      rename(action = variable, days_since = value)  %>%
      mutate(text = ifelse(is.na(days_since), "Not since tracking", paste0(as.character(days_since)," days"))) %>%
      mutate(hov_text = paste0(symbol, " | ", action," | ",text)) %>%
      mutate(empty_text ="")
    cutoff <- mean(days_since$days_since, na.rm=TRUE)
    d1 <- days_since %>% filter(is.na(days_since) | days_since > cutoff)
    d2 <- days_since %>% filter(!(is.na(days_since) | days_since > cutoff))
    
    # days_since <- droplevels(days_since)
    # plot_day_since <- function(days_since, d1, d2, tmp, palette_default){
    # plot_ly(
    #   y=~factor(days_since$symbol, levels=rev(unique(tmp$symbol))),
    #   x=~days_since$action, 
    #   z=~days_since$days_since, 
    #   type="heatmap",
    #   showscale=FALSE,
    #   # text = ~days_since$empty_text,
    #   hovertext=~days_since$hov_text,
    #   hoverinfo = "text",
    #   reversescale=TRUE,
    #   # colorscale = viridis(10),
    #   height=600
    # ) %>%
    #   add_annotations(x =~days_since$action, y =~days_since$symbol, text="", showarrow = FALSE, ax = 20, ay = -20) %>%
    #   add_annotations(x =~d1$action, y =~d1$symbol, text=d1$text,
    #                   showarrow = FALSE, font=list(color="white"), ax = 20, ay = -20) %>%
    #   add_annotations(x =~d2$action, y =~d2$symbol, text=d2$text, 
    #                   showarrow = FALSE, font=list(color="black"), ax = 20, ay = -20) %>%
    #   layout(showlegend=FALSE,
    #          xaxis=list(side="top", title = list(text='', font=list(size=11))),
    #          yaxis=list(title = list(text='', font=list(size=11))),
    #          title = list(text='', font=list(size=11)),
    #          paper_bgcolor='#1C1C1D',
    #          hoverlabel=list(font=list(color="white")),
    #          plot_bgcolor='#1C1C1D',  dragmode = FALSE,
    #          font=list(color="white")
    #   )  %>%
    #   config( displaylogo = FALSE, showTips = FALSE,
    #           modeBarButtonsToRemove = c('toImage','toggleSpikelines','zoomIn2d', 'zoomOut2d', 'lasso2d','select2d',
    #                                      'hoverClosestCartesian','hoverCompareCartesian'))
    # }
    plot_day_since(days_since, d1, d2, tmp, palette_default)
    
  })
  
  
  from <- c("USD", "USD", "USD", "USD", "USD", "USD", "USD")
  to <- c("USD", "CAD", "EUR", "GBP", "INR", "MYR", "TRY")
  FX <- quantmod::getQuote(paste0(from, to, "=X")) %>%
    select(Last) %>%
    mutate(from = from, to = to)
  
  observeEvent(input$mole_buylimits, {
    # output$buy_text <- renderText({"calculating... please wait..."})
    
    buywhat <- input$buy_what
    currency <- input$buy_currency
    current_units = as.numeric(input$buy_owned)
    invested_fiat = as.numeric(input$buy_invested)
    total_budget = as.numeric(input$buy_howmuch)
    buy_strat = input$buy_strategy
    
    buylimits <- function(tmp, input_limit, total_budget, current_units, invested_fiat, ratiox){
      
      tmp %>%
        mutate(z_scores = input_limit) %>%
        mutate(budget_total = total_budget) %>%
        mutate(budget_ratio = ratiox) %>%
        mutate(budget = budget_total*budget_ratio) %>%
        mutate(limit_buy =  current.price - (wtd.sd*z_scores)) %>%
        mutate(z_scores = -z_scores) %>%
        mutate(new_units = budget/limit_buy) %>%
        mutate(new_total = new_units + current_units) %>%
        mutate(new_fiat = budget + invested_fiat) %>%
        mutate(current_breakeven = invested_fiat/current_units) %>%
        mutate(breakeven_new = new_fiat/new_total) %>%
        mutate(breakeven_delta = current_breakeven - breakeven_new) %>%
        mutate(breakeven_percent = breakeven_delta/current_breakeven) %>%
        select(symbol, z_scores, limit_buy, budget_ratio, budget, new_units, current_breakeven, breakeven_new, breakeven_delta, breakeven_percent) %>%
        mutate(breakeven_new = ifelse(is.na(current_breakeven), NA, breakeven_new),
               new_units = ifelse(is.na(current_breakeven), NA, new_units),
               breakeven_delta = ifelse(is.na(current_breakeven), NA, breakeven_delta),
               breakeven_percent = ifelse(is.na(current_breakeven), NA, breakeven_percent))
    }
    
    tmp <- data() 
    
    tmp <- tmp %>%
      select(symbol, current.price, wtd.sd) %>%
      mutate(current.price = current.price*FX[FX$to == currency,]$Last,
             wtd.sd = wtd.sd*FX[FX$to == currency,]$Last)
    
    limitpoints <- seq(0, 5, 0.5)
    strategies <- data.frame(aggressive = c(5, 25, 25, 25,15, 5, 0, 0, 0, 0, 0)/100,
                             unbiased = c(0, rep(1/(length(limitpoints)-1), length(limitpoints)-1)),
                             bell = c(5, 10, 15, 20,20, 15, 10, 5, 0, 0, 0)/100,
                             laidback = c(0, 0, 5, 10, 15, 20, 20,15,10, 5, 0)/100,
                             significant = c(0, 0, 0, 0, 25, 25, 25, 15, 10, 0, 0)/100)
    
    buy_ratios <- strategies[,grepl(buy_strat, colnames(strategies))]
    
    tmp2 <- do.call(rbind, lapply(1:length(limitpoints), function(x) buylimits(tmp, input_limit= limitpoints[x], total_budget = total_budget,
                                                                               current_units= current_units, invested_fiat = invested_fiat,
                                                                               ratiox = buy_ratios[x]))) %>%
      filter(symbol == buywhat) %>%
      left_join(., cryptodigits, by = "symbol")
    
    tmp2$limit_buy <- paste0("$", formatC(as.numeric(tmp2$limit_buy), format="f", digits=unique(tmp2$digs), big.mark=","))
    if(!is.na(unique(tmp2$current_breakeven))){
      tmp2$current_breakeven <- paste0("$", formatC(as.numeric(tmp2$current_breakeven), format="f", digits=unique(tmp2$digs), big.mark=","))
      tmp2$breakeven_new <- paste0("$", formatC(as.numeric(tmp2$breakeven_new), format="f", digits=unique(tmp2$digs), big.mark=","))
      tmp2$breakeven_delta <- paste0("$", formatC(as.numeric(tmp2$breakeven_delta), format="f", digits=unique(tmp2$digs), big.mark=","))
    }
    
    todatatable <- tmp2 %>%
      select(-coin_id,-digs)
    
    output$buylimits <- renderReactable({
      reactable(todatatable,
                borderless = TRUE,
                bordered = FALSE, 
                striped = FALSE,      
                onClick = "select",
                pagination = FALSE, highlight = TRUE, 
                theme = reactableTheme(
                  rowStyle = "height: 10px;",
                  rowSelectedStyle = list(backgroundColor = "#1c1c1d", boxShadow = "inset 2px 0 0 0 #007AFF"),
                  backgroundColor = "#1c1c1d",
                ), 
                columns = list(z_scores = colDef(style = function(value) {list(fontWeight = "bold")}),
                               limit_buy = colDef(style = function(value) {list(fontWeight = "bold", color = "red")})
                ),
                defaultColDef = colDef(
                  header = function(value) gsub(".", " ", value, fixed = TRUE),
                  cell = function(value) format(value, nsmall = 1),
                  align = "center",
                  minWidth = 70,
                  headerStyle = list(background = "#1c1c1d")
                ))
    })
    
  })
  
  observeEvent(input$mole_selllimits, {
    # output$sell_text <- renderText({"calculating... please wait..."})
    
    sellwhat <- input$sell_what
    currency <- input$sell_currency
    sell_total = as.numeric(input$sell_howmany)
    current_units = as.numeric(input$sell_owned)
    invested_fiat = as.numeric(input$sell_invested)
    
    selllimits <- function(tmp, input_limit, current_units, invested_fiat, sell_total, ratiox){
      
      tmp %>%
        mutate(z_scores = input_limit) %>%
        mutate(sell_total = sell_total) %>%
        mutate(sell_ratio = ratiox) %>%
        mutate(sell_units = sell_total*sell_ratio) %>%
        mutate(limit_sell =  current.price + (wtd.sd*z_scores)) %>%
        mutate(fiat = sell_units*limit_sell) %>%
        mutate(new_total = current_units - sell_units) %>%
        mutate(new_fiat = invested_fiat - fiat) %>%
        mutate(breakeven_new = new_fiat/new_total) %>%
        mutate(current_breakeven =invested_fiat/current_units) %>%
        mutate(breakeven_delta = breakeven_new - current_breakeven) %>%
        mutate(breakeven_percent = breakeven_delta/current_breakeven) %>%
        select(symbol, z_scores, limit_sell, sell_ratio, sell_units, fiat, current_breakeven, breakeven_new, breakeven_delta, breakeven_percent) %>%
        mutate(breakeven_new = ifelse(is.na(current_breakeven), NA, breakeven_new),
               fiat = ifelse(is.na(current_breakeven), NA, fiat),
               breakeven_delta = ifelse(is.na(current_breakeven), NA, breakeven_delta),
               breakeven_percent = ifelse(is.na(current_breakeven), NA, breakeven_percent))
    }
    
    tmp <- data()
    
    tmp <- tmp %>%
      select(symbol, current.price, wtd.sd) %>%
      mutate(current.price = current.price*FX[FX$to == currency,]$Last,
             wtd.sd = wtd.sd*FX[FX$to == currency,]$Last)
    
    
    limitpoints <- seq(0, 5, 0.5)
    strategies <- data.frame(aggressive = c(5, 25, 25, 25,15, 5, 0, 0, 0, 0, 0)/100,
                             unbiased = c(0, rep(1/(length(limitpoints)-1), length(limitpoints)-1)),
                             bell = c(5, 10, 15, 20,20, 15, 10, 5, 0, 0, 0)/100,
                             laidback = c(0, 0, 5, 10, 15, 20, 20,15,10, 5, 0)/100,
                             significant = c(0, 0, 0, 0, 25, 25, 25, 15, 10, 0, 0)/100)
    
    sellratios <- strategies[,grepl(input$sell_strategy, colnames(strategies))]
    
    tmp2 <- do.call(rbind, lapply(1:length(limitpoints), function(x) selllimits(tmp, input_limit= limitpoints[x], current_units, invested_fiat,
                                                                                sell_total = sell_total, ratiox = sellratios[x]))) %>%
      arrange(limit_sell) %>%
      filter(symbol == sellwhat) %>%
      left_join(., cryptodigits, by = "symbol")
    
    tmp2$limit_sell <- paste0("$", formatC(as.numeric(tmp2$limit_sell), format="f", digits=unique(tmp2$digs), big.mark=","))
    if(!is.na(unique(tmp2$current_breakeven))){
      tmp2$current_breakeven <- paste0("$", formatC(as.numeric(tmp2$current_breakeven), format="f", digits=unique(tmp2$digs), big.mark=","))
      tmp2$breakeven_new <- paste0("$", formatC(as.numeric(tmp2$breakeven_new), format="f", digits=unique(tmp2$digs), big.mark=","))
      tmp2$breakeven_delta <- paste0("$", formatC(as.numeric(tmp2$breakeven_delta), format="f", digits=unique(tmp2$digs), big.mark=","))
    }
    
    todatatable <- tmp2 %>%
      select(-coin_id,-digs)
    
    output$selllimits <- renderReactable({
      reactable(todatatable,
                borderless = TRUE,
                bordered = FALSE, 
                striped = FALSE,      
                onClick = "select",
                pagination = FALSE, highlight = TRUE, 
                theme = reactableTheme(
                  rowStyle = "height: 10px;",
                  rowSelectedStyle = list(backgroundColor = "#1c1c1d", boxShadow = "inset 2px 0 0 0 #007AFF"),
                  backgroundColor = "#1c1c1d",
                ), 
                columns = list(z_scores = colDef(style = function(value) {list(fontWeight = "bold")}),
                               limit_sell = colDef(style = function(value) {list(fontWeight = "bold", color = "red")})
                ),
                defaultColDef = colDef(
                  header = function(value) gsub(".", " ", value, fixed = TRUE),
                  cell = function(value) format(value, nsmall = 1),
                  align = "center",
                  minWidth = 70,
                  headerStyle = list(background = "#1c1c1d")
                ))
    })
   
  })
  
  output$user_token1 <- renderText({paste0(session$token,"_",clone_name)})

  # output$timeout <- renderText({input$timeOut})
  observeEvent(input$timeOut, {
    f7Notif(
      closeTimeout = 10000000,
      title = "idle timeout",
      text=paste("closed app due to", input$timeOut, "of inactivity -", Sys.time())
    )
    session$close()
  })
  
  ###################################
  ##### Historical UIs
  
  output$historical_zprice <- renderUI({
    
    if(input$zprice_duration == FALSE){
      htmltools::tags$iframe(src="https://buythedips.io/hist/zprice.html", height=450, width="100%", frameBorder=0)
    } else {
      htmltools::tags$iframe(src="https://buythedips.io/hist/zprice_max.html", height=450, width="100%", frameBorder=0)
    }
  })
  
  output$historical_zvolume <- renderUI({
    
    if(input$zvolume_duration == FALSE){
      htmltools::tags$iframe(src="https://buythedips.io/hist/zvolume.html", height=450, width="100%", frameBorder=0)
    } else {
      htmltools::tags$iframe(src="https://buythedips.io/hist/zvolume_max.html", height=450, width="100%", frameBorder=0)
    }
  })
  
  output$historical_performance <- renderUI({
    
    if(input$performance_duration == FALSE){
      htmltools::tags$iframe(src="https://buythedips.io/hist/performance.html", height=450, width="100%", frameBorder=0)
    } else {
      htmltools::tags$iframe(src="https://buythedips.io/hist/performance_max.html", height=450, width="100%", frameBorder=0)
    }
  })
  
  ###################################
  ##### Filter table
  
  output$reactable <- renderReactable({
      
    # files <- data.frame(file = paste0(dir, "/", rownames(fileSnapshot(dir)$info)), created=as.POSIXct(fileSnapshot(dir)$info$mtime)) %>%
    # arrange(desc(created))
    # 
    # nav_table = readRDS(files$file[1])
    nav_table = data() 
    
    nav_table <- nav_table %>%
        rename(USD = current.price) %>%
        mutate(rank = 1:n()) %>%
        select(symbol, USD, z.score, digs, rank) %>%
      rename(z_scores = z.score)
      
    selections <- previousSelection() 
    # selections <- c(1:15)
    nav_table$USD <- sapply(1:nrow(nav_table), function(x) paste0("$", formatC(as.numeric(nav_table$USD[x]), format="f", digits=nav_table$digs[x], big.mark=",")))
    # nav_table$z_scores <- sprintf("%.3f",nav_table$z_scores)
    nav_table$z_scores <- round(nav_table$z_scores,3)
    
    reactable(nav_table %>% select(-digs), 
              selection = "multiple", 
              defaultSelected = selections,
              borderless = TRUE,
              bordered = FALSE, 
              striped = FALSE,      
              onClick = "select",
              pagination = FALSE, highlight = TRUE, 
              theme = reactableTheme(
                rowStyle = "height: 10px;",
                rowSelectedStyle = list(backgroundColor = "#1c1c1d", boxShadow = "inset 2px 0 0 0 #007AFF"),
                backgroundColor = "#1c1c1d",
              ), 
              columns = list(
                symbol = colDef(width = 75, style=list( fontWeight = "bold")),
                rank = colDef(width = 60),
                USD = colDef(width = 120)
              ),
              defaultColDef = colDef(
                header = function(value) gsub(".", " ", value, fixed = TRUE),
                cell = function(value) format(value, nsmall = 1),
                align = "center",
                minWidth = 70,
                headerStyle = list(background = "#1c1c1d")
              )
    )
      
    })
 
  
  ###################################
  ##### Filtering cryptos
  
  previousSelection <- reactiveVal(c(1:max_filter))
  
  # output$table_state <- renderPrint({
  #   state <- req(getReactableState("table"))
  #   print(state)
  # })
  
  observeEvent(input$filterGO, {
    state <- getReactableState("reactable", "selected")
    # print(state)
    # output$state_selected <- renderText({state})
    # print(state)
    # print(previousSelection())
    if(length(state) > max_filter){
      output$filter_message <- renderText({paste0("Sorry, the selection limit is ", max_filter)})
    } else if(length(state) != length(previousSelection())){
      previousSelection(c(state))
      output$filter_message <- renderText({paste0("")})
    } else if(!all(state %in% previousSelection())){
      previousSelection(c(state))
      output$filter_message <- renderText({paste0("")})
    } else {
      output$filter_message <- renderText({paste0("no changes were requested")})
    }
  })
  
  observeEvent(input$fb1, {
    previousSelection(c(1:max_filter))
  })
  
  observeEvent(input$fb2, {
    index <- data() %>%
      mutate(rank = 1:n()) %>%
      select(symbol, rank)
    previousSelection(c(which(index$symbol %in% c("BTC","XRP","DOGE", "LTC"))))
  })
  
  observeEvent(input$fb3, {
    index <- data() %>%
      mutate(rank = 1:n()) %>%
      select(symbol, rank)
    previousSelection(c(which(index$symbol %in% c("ETH","SOL","ADA", "LUNA", "DOT", "AVAX", "ATOM", "FTM", "VET", "ONE", "ALGO"))))
  })
  
  observeEvent(input$fb4, {
    index <- data() %>%
      mutate(rank = 1:n()) %>%
      select(symbol, rank)
    previousSelection(c(which(index$symbol %in% c("BNB","CRO","LRC"))))
  })
  
  observeEvent(input$fb5, {
    index <- data() %>%
      mutate(rank = 1:n()) %>%
      select(symbol, rank)
    previousSelection(c(which(index$symbol %in% c("DOGE","SHIB","MOON"))))
  })
  
  observeEvent(input$fb6, {
    index <- data() %>%
      mutate(rank = 1:n()) %>%
      select(symbol, rank)
    previousSelection(c(which(index$symbol %in% c("LINK","GRT"))))
  })
  
  
  ###################################
  ##### Order book stuff
  bookz_dir= paste0(work_dir, "data_orderbook_z/")
  
  bookz_data <- reactivePoll(2000, session,
                             checkFunc = function(){
                               (fileSnapshot(bookz_dir)$info$mtime)
                               # newfile <- paste0(newdir, "/data.rds")
                             },
                             valueFunc = function(){
                               book_z <- paste0(bookz_dir, "/", rownames(fileSnapshot(bookz_dir)$info)[which.max(fileSnapshot(bookz_dir)$info$mtime)])
                               # newfile <- paste0(newdir, "/data.rds")
                               readRDS(book_z)
                               # filter(symbol %in% previousSelection)
                             }
  )
  
  book_dir= paste0(work_dir, "data_orderbook/")
  
  bookraw_data <- reactivePoll(2000, session,
                               checkFunc = function(){
                                 (fileSnapshot(book_dir)$info$mtime)
                                 # newfile <- paste0(newdir, "/data.rds")
                               },
                               valueFunc = function(){
                                 book <- paste0(book_dir, "/", rownames(fileSnapshot(book_dir)$info)[which.max(fileSnapshot(book_dir)$info$mtime)])
                                 # newfile <- paste0(newdir, "/data.rds")
                                 readRDS(book)
                                 # filter(symbol %in% previousSelection)
                               }
  )
  
  book_files <- list.files(path=book_dir, pattern=".rds",full.names = TRUE,recursive = TRUE)
  book_files = data.frame(book_files, created = as.POSIXct(do.call(rbind, strsplit(book_files, "lims_|.rds"))[,2], tz="UTC")) %>%
    arrange(desc(created)) %>%
    filter(created >= max(created) - lubridate::days(14)) %>%
    slice(which(row_number() %% 8 == 1))
  
  book <- book_files$book_files %>% purrr::map_df(~readRDS(.)) %>%
    mutate(units = dollars/z_lims) %>%
    arrange(datetime)
  
  
  bookz_files <- list.files(path=bookz_dir, pattern=".rds",full.names = TRUE,recursive = TRUE)
  bookz_files = data.frame(bookz_files, created = as.POSIXct(do.call(rbind, strsplit(bookz_files, "lims_|.rds"))[,2], tz="UTC")) %>%
    arrange(desc(created)) %>%
    filter(created >= max(created) - lubridate::days(14)) %>%
    slice(which(row_number() %% 8 == 1))
  
  bookz <- bookz_files$bookz_files %>% purrr::map_df(~readRDS(.)) %>%
    arrange(datetime)
  
  output$orderbook_table <- renderReactable({
    
    tmp <- bookz_data()
    order_sel <- orderbook_selection()
    # output$orderbook_update <- renderText({unique(tmp$datetime)})
    
    tmp <- tmp %>%
      group_by(symbol) %>%
      mutate(ratio = units_mean[order=="bids"]/units_mean[order=="asks"]) %>%
      select(symbol, ratio, order, z_book_units) %>%
      mutate(order = paste0("z_",order)) %>%
      reshape2::dcast(symbol+ratio~order, value.var = "z_book_units")
    
    tmp <- tmp[match(unique(book$symbol), tmp$symbol),]
    
    
    tmp$z_asks <- round(tmp$z_asks,3)
    tmp$z_bids <- round(tmp$z_bids,3)
    tmp$ratio <- round(tmp$ratio,3)
    
    reactable(tmp %>% rename(`bid:ask ratio`= ratio),
              selection = "single",
              defaultSelected = c(which(tmp$symbol == order_sel)),
              borderless = TRUE,
              bordered = FALSE,
              striped = FALSE,
              onClick = "select",
              pagination = FALSE, highlight = TRUE,
              theme = reactableTheme(
                rowStyle = "height: 10px;",
                rowSelectedStyle = list(backgroundColor = "#1c1c1d", boxShadow = "inset 2px 0 0 0 #007AFF"),
                backgroundColor = "#1c1c1d",
              ),
              columns = list(
                symbol = colDef(width = 75, style=list( fontWeight = "bold"))
                # rank = colDef(width = 60),
                # USD = colDef(width = 120)
              ),
              defaultColDef = colDef(
                header = function(value) gsub(".", " ", value, fixed = TRUE),
                cell = function(value) format(value, nsmall = 1),
                align = "center",
                minWidth = 70,
                headerStyle = list(background = "#1c1c1d")
              )
    )
    
  })
  
  output$orderbook_zsummary <- renderPlotly({
    
    tmp <- bookz_data() %>%
      group_by(symbol) %>%
      tidyr::nest()
    
    output$orderbook_update <- renderText({paste0("",unique(tmp$datetime)," UTC")})
    
    tmp <- tmp[match(unique(book$symbol), tmp$symbol),] %>%
      tidyr::unnest(col=c(data))
    
    # tmp %>%
    #   plot_ly(., type="scatter", mode="markers",
    #           x = ~round(z_book_units,2),
    #           y = ~factor(symbol, levels=rev(unique(symbol))),
    #           color = ~factor(order, levels=(unique(order))),
    #           # colors = c(viridis(10)[5], viridis(10)[8]),
    #           colors = c(gg_color_hue(8)[8],viridis(10)[9]),
    #           marker = list(size= 15, opacity=0.75)
    #   ) %>%
    #   layout(
    #     xaxis=list(range=c(-5,5), showgrid=FALSE,
    #                title = list(text='', font=list(size=11)),
    #                tickvals=list(-3.75,-3,-2.1,-2,-1,0,1,2,2.1,3,3.75),
    #                tickangle=0, tickfont=list(size=10),
    #                ticktext=list(" \nvery low\n","-3"," \nlow\n","-2","-1","0\nneutral\n",
    #                              "1","2"," \nhigh\n","3"," \nvery high\n")),
    #     yaxis=list(title='', gridcolor = "#424242"),
    #     shapes=list(vline(0,"#898ba1"),vline(2),vline(3),vline(-2,"red"),vline(-3,"red")),
    #     title = list(text='<b>z-scores for orderbook bids and asks</b>', font=list(size=11)),
    #     paper_bgcolor='#1C1C1D',
    #     hoverlabel=list(font=list(color="white")),
    #     plot_bgcolor='#1C1C1D',  dragmode = FALSE,
    #     legend=list(font=list(size=11),
    #                 # itle=list(text='<b> Click to filter </b>'),
    #                 orientation="h",
    #                 yanchor="top",
    #                 xanchor="right",
    #                 y=1.02,
    #                 x=1),
    #     font=list(color="#FFFFFF"))  %>%
    #   config( displaylogo = FALSE, showTips = FALSE,
    #           modeBarButtonsToRemove = c('toImage','toggleSpikelines','zoomIn2d', 'zoomOut2d', 'lasso2d','select2d',
    #                                      'pan2d','zoom2d','resetScale2d',"autoScale2d",
    #                                      'hoverClosestCartesian','hoverCompareCartesian'))
    ob_colors <- gg_color_hue(length(unique(tmp$symbol)))
    tmp %>%
      mutate(short_text = ifelse(order == "bids","B","A")) %>%
      plot_ly(., x = ~round(z_book_units,2),
              y = ~factor(symbol, levels=rev(unique(symbol))),
              color = ~factor(symbol, levels=(unique(symbol))),
              colors =  ob_colors,
              text=~short_text
      ) %>%
      add_lines(x = ~round(z_book_units,2),
                y = ~factor(symbol, levels=rev(unique(symbol))),
                showlegend=F) %>%
      add_markers(
        x = ~round(z_book_units,2),
        y = ~factor(symbol, levels=rev(unique(symbol))),
        showlegend=F,
        marker = list(color = '#1c1c1d', size = 20, line = list(colors = ob_colors, width = 2 )) ) %>%
      add_text(showlegend=F, textfont=list(size = 14) ) %>%
      layout(
        xaxis=list(range=c(-5,5), showgrid=FALSE, 
                   title = list(text='', font=list(size=11)),
                   tickvals=list(-3.75,-3,-2.1,-2,-1,0,1,2,2.1,3,3.75),
                   tickangle=0, tickfont=list(size=10),
                   ticktext=list(" \nvery low\n","-3"," \nlow\n","-2","-1","0\nneutral\n",
                                 "1","2"," \nhigh\n","3"," \nvery high\n")),
        yaxis=list(title='', gridcolor = "#424242"),
        shapes=list(vline(0,"#898ba1"),vline(2),vline(3),vline(-2,"red"),vline(-3,"red")),
        title = list(text='<b>z-score for orderbook ASKS (A) and BIDS (B)</b>', font=list(size=11)), 
        paper_bgcolor='#1C1C1D',
        hoverlabel=list(font=list(color="white")),
        plot_bgcolor='#1C1C1D',  dragmode = FALSE,
        font=list(color="#FFFFFF"))  %>%
      config( displaylogo = FALSE, showTips = FALSE, 
              modeBarButtonsToRemove = c('toImage','toggleSpikelines','zoomIn2d', 'zoomOut2d', 'lasso2d','select2d',
                                         'pan2d','zoom2d','resetScale2d',"autoScale2d",
                                         'hoverClosestCartesian','hoverCompareCartesian'))
  })
  
  orderbook_selection <- reactiveVal(c("BTC"))
  
  observeEvent(input$filterOB, {
    stateOB <- unique(book$symbol)[getReactableState("orderbook_table", "selected")]
    if(stateOB != orderbook_selection()){
      orderbook_selection(stateOB)
    } else {
      print("its the same")
    }
  })
  
  output$spread <- renderPlotly({
    # book_dir= paste0(work_dir, "data_orderbook/")
    # book_now = readRDS(paste0(book_dir, "/", rownames(fileSnapshot(book_dir)$info)[which.max(fileSnapshot(book_dir)$info$mtime)]))
    
    book_now <- bookraw_data()
    # key = data()$symbol
    # unique(book_now$symbol)[state_orderbook]
    order_sel <- orderbook_selection()
    
    test <- book_now %>%
      filter(symbol == order_sel) %>%
      mutate(z_score_bins = (ceiling(z_score_bins*2)/2)) %>%
      group_by(order, z_score_bins) %>%
      summarise(dollars = sum(dollars), .groups="drop") %>%
      ungroup() %>%
      mutate(order2 = paste0(order,": ", z_score_bins)) %>%
      reshape2::dcast(order2~order, value.var = "dollars")
    
    test %>%
      ungroup() %>%
      mutate(order=sapply(1:nrow(test), function(x) unlist(strsplit(test$order2[x],":"))[1])) %>%
      mutate(z_lim=sapply(1:nrow(test), function(x) as.numeric(unlist(strsplit(test$order2[x],":"))[2]))) %>%
      arrange(z_lim) %>%
      # mutate(order2 = factor(order2, levels=c('asks: 5', 'asks: 4.5', 'asks: 4', 'asks: 3.5', 'asks: 3', 'asks: 2.5', 'asks: 2', 'asks: 1.5', 'asks: 1', 'asks: 0.5', 'asks: 0', 'bids: 0', 'bids: -0.5', 'bids: -1', 'bids: -1.5', 'bids: -2', 'bids: -2.5', 'bids: -3', 'bids: -3.5', 'bids: -4', 'bids: -4.5', 'bids: -5'))) %>%
      group_by(order) %>%
      do(p=
           plot_ly(., type = "bar",
                   showlegend=F,
                   orientation="h") %>%
           add_bars(name = "asks", marker=list(color = rev(viridis(11))),
                    y=~z_lim, x=~asks) %>%
           add_bars(y=~z_lim,
                    x=~bids, name="bids", marker=list(color = viridis(11))
           ) %>%
           layout(barmode="group",
                  xaxis=list(tickprefix="$", tickformat=",.2f", gridcolor = "#424242", tickfont=list(size=10), title = list(text='', font=list(size=8))),
                  yaxis=list(tickformat=",.", title='z-score (price) levels', 
                             tick0=0, dtick=1,
                             gridcolor = "#1c1c1d"),
                  title = list(text=paste0('<b>', order_sel,
                                           ': current orderbook</b>'), font=list(size=11)),
                  paper_bgcolor='#1C1C1D',
                  hoverlabel=list(font=list(color="white")),
                  plot_bgcolor='#1C1C1D',  dragmode = FALSE,
                  font=list(color="#FFFFFF"))  %>%
           config( displaylogo = FALSE, showTips = FALSE,
                   modeBarButtonsToRemove = c('toImage','toggleSpikelines','zoomIn2d', 'zoomOut2d', 'lasso2d','select2d',
                                              'pan2d','zoom2d','resetScale2d',"autoScale2d",
                                              'hoverClosestCartesian','hoverCompareCartesian'))
      ) %>%
      subplot(nrows = 2, shareX = TRUE, shareY = FALSE, titleY = TRUE)
    
  })
  
  output$orderbook_historical <- renderPlotly({
    
    # state_orderbook <- req(getReactableState("orderbook_table", "selected"))
    order_sel <- orderbook_selection()
    
    book %>%
      mutate(units = dollars/z_lims) %>%
      filter(symbol == order_sel) %>%
      group_by(datetime, symbol, order) %>%
      summarise(dollars = sum(dollars),
                units = sum(units), .groups ="drop") %>%
      # mutate(z_score_bins = "total") %>%
      plot_ly(type="scatter",mode="lines",
              x=~datetime, y=~units, color=~order,
              colors = c(gg_color_hue(8)[8],viridis(10)[9])
      ) %>%
      layout(barmode="group",
             xaxis=list(tickfont=list(size=10), title = list(text='', font=list(size=8))),
             yaxis=list(
               # tickprefix="$", tickformat=",.2f",
               title='total unts trading', font=list(size=12), gridcolor = "#1c1c1d"),
             title = list(text=paste0('<b>',
                                      order_sel,
                                      ': 14d orderbook</b>'), font=list(size=11)),
             paper_bgcolor='#1C1C1D',
             hoverlabel=list(font=list(color="white")),
             plot_bgcolor='#1C1C1D',  dragmode = FALSE,
             legend=list(font=list(size=11),
                         # itle=list(text='<b> Click to filter </b>'),
                         orientation="h",
                         yanchor="top",
                         xanchor="right",
                         y=1.1,
                         x=1),
             font=list(color="#FFFFFF"))  %>%
      config( displaylogo = FALSE, showTips = FALSE,
              modeBarButtonsToRemove = c('toImage','toggleSpikelines','zoomIn2d', 'zoomOut2d', 'lasso2d','select2d',
                                         'pan2d','zoom2d','resetScale2d',"autoScale2d",
                                         'hoverClosestCartesian','hoverCompareCartesian'))
  })
  
  output$bookz_historical <- renderPlotly({
    
    # state_orderbook <- req(getReactableState("orderbook_table", "selected"))
    order_sel <- orderbook_selection()
    
    ratio = bookz %>%
      group_by(datetime, symbol) %>%
      mutate(z_book_units = units_now[which(order == "bids")]/units_now[which(order == "asks")]) %>%
      select(datetime, symbol, z_book_units) %>%
      mutate(order = "ratio")
    
    bookz %>%
      select(datetime, symbol, order, z_book_units) %>%
      rbind(., ratio) %>%
      # filter(symbol == "BTC") %>%
      filter(symbol == order_sel) %>%
      plot_ly(type="scatter",mode="lines",
              x=~datetime, y=~z_book_units, color=~order,
              colors = c(gg_color_hue(8)[8], viridis(10)[9], viridis(10)[4])
      ) %>%
      layout(barmode="group",
             xaxis=list(tickfont=list(size=10), title = list(text='', font=list(size=8))),
             yaxis=list(
               # tickprefix="$", tickformat=",.2f",
               title='z-scores', font=list(size=12), gridcolor = "#1c1c1d"),
             title = list(text=paste0('<b> ',
                                      order_sel,
                                      ': z-scores for bids and asks</b>'), font=list(size=11)),
             paper_bgcolor='#1C1C1D',
             hoverlabel=list(font=list(color="white")),
             plot_bgcolor='#1C1C1D',  dragmode = FALSE,
             legend=list(font=list(size=11),
                         # itle=list(text='<b> Click to filter </b>'),
                         orientation="h",
                         yanchor="top",
                         xanchor="right",
                         y=1.1,
                         x=1),
             font=list(color="#FFFFFF"))  %>%
      config( displaylogo = FALSE, showTips = FALSE,
              modeBarButtonsToRemove = c('toImage','toggleSpikelines','zoomIn2d', 'zoomOut2d', 'lasso2d','select2d',
                                         'pan2d','zoom2d','resetScale2d',"autoScale2d",
                                         'hoverClosestCartesian','hoverCompareCartesian'))
  })
  
  output$orderbook_byz <- renderPlotly({
    
    # state_orderbook <- req(getReactableState("orderbook_table", "selected"))
    order_sel <- orderbook_selection()
    
    palette_bins <- c((RColorBrewer::brewer.pal(8, "Reds")[2:8]), 
                      rev(RColorBrewer::brewer.pal(8, "Greens")[2:8]))
    
    plot_dat <- book %>%
      # filter(symbol == "BTC") %>%
      filter(symbol == order_sel) %>%
      mutate(units = dollars/z_lims) %>%
      mutate(z_label = as.character(round(z_score_bins,0))) %>%
      group_by(datetime, symbol, order, z_label) %>%
      summarise(dollars = sum(dollars), units = sum(units), .groups ="drop") %>%
      mutate(z_label = paste0(order, ": ", z_label))
    
    label_order <- c(paste0("asks: ", rev(seq(0,5,1))), paste0("bids: ", rev(seq(-5,0,1))))
    
    plot_dat %>%
      plot_ly(type="scatter",mode="lines",
              x=~datetime, 
              y=~units, 
              color=~factor(z_label, levels=label_order),
              colors = palette_bins,
              # alpha=0.75, 
              line = list(width= 2, opacity=0.5, alpha=0.5)
      ) %>%
      layout(xaxis=list(title = list(text='', font=list(size=11))),
             yaxis=list(
               # tickprefix = "$", tickformat=',.2f', 
               title=list(text='total units trading\n ', font=list(size=12)), gridcolor = "#1c1c1d"),
             title = list(text=paste0('<b>', 
                                      order_sel,
                                      ': orderbook by dip and moon levels</b>'), font=list(size=11)),
             paper_bgcolor='#1C1C1D',
             hoverlabel=list(font=list(color="white")),
             plot_bgcolor='#1C1C1D',  dragmode = FALSE,
             font=list(color="#FFFFFF"))  %>%
      config( displaylogo = FALSE, showTips = FALSE,
              modeBarButtonsToRemove = c('toImage','toggleSpikelines','zoomIn2d', 'zoomOut2d', 'lasso2d','select2d',
                                         'pan2d','zoom2d','resetScale2d',"autoScale2d",
                                         'hoverClosestCartesian','hoverCompareCartesian'))
  })
  
  ################## Voting stuff
  
  observeEvent(input$vote_go,{
    
    if(input$vote_for == "select coin"){
      output$vote_message <- renderText({"please select a coin you'd like added to twitter updates."})
    } else if (input$vote_against == "select coin"){
      output$vote_message <- renderText({"please select a coin you'd like removed from twitter updates."})
    } else {

      if(!file.exists(paste0(work_dir, "twitter_votes.txt"))){
        
        votes <- data.frame(token = session$token, 
                            time = Sys.time(), 
                            toadd = input$vote_for,
                            toremove = input$vote_against)
        write.table(x = votes, 
                    file = paste0(work_dir, "twitter_votes.txt"),
                    append = TRUE, row.names = FALSE, col.names = FALSE, sep = "\t")
        output$vote_message <- renderText({"thank you for voting!"})
        
      } else {
        
        votes <- fread(paste0(work_dir, "twitter_votes.txt"))
        
        if(session$token %in% votes$V1){
          
          output$vote_message <- renderText({"sorry you've already voted in this session."})
          
        } else {
          
          votes <- data.frame(token = session$token, 
                              time = Sys.time(), 
                              toadd = input$vote_for,
                              toremove = input$vote_against)
          write.table(x = votes, file = paste0(work_dir, "twitter_votes.txt"),
                      append = TRUE, row.names = FALSE, col.names = FALSE, sep = "\t") 
          output$vote_message <- renderText({"thank you for voting!"})
        }
      } 
    }
    
  })
  
  ###################################
  ##### Mole clicks
  
  users_data <- data.frame(token = session$token, start = Sys.time())
  
  observeEvent(input$mole_buylimits, {
    users_data$click <- Sys.time()
    users_data$action <- "buy"
    users_data$click_buy_coin <- as.character(input$buy_what)
    users_data$click_buy_lims <- as.numeric(input$mole_buylimits)
    write.table(x = users_data, file = paste0(work_dir, "user_clicks.txt"),
                append = TRUE, row.names = FALSE, col.names = FALSE, sep = "\t")
  })
  
  observeEvent(input$mole_selllimits, {
    users_data$click <- Sys.time()
    users_data$action <- "sell"
    users_data$click_sell_coin <- as.character(input$sell_what)
    users_data$click_sell_lims <- as.numeric(input$mole_selllimits)
    write.table(x = users_data, file = paste0(work_dir, "user_clicks.txt"),
                append = TRUE, row.names = FALSE, col.names = FALSE, sep = "\t")
  })
  
  ###################################
  ### Connected users
  if(!file.exists(paste0(work_dir, "admin.txt"))){
    # users_connected <- data.frame(token = session$token, start = Sys.time(), count = 1, worker="master", proxy=1)
    users_connected <- data.frame(token = session$token, start = Sys.time(), 
                                  worker=clone_name,
                                  action="enter",
                                  n = 1)
    write.table(x = users_connected, file = paste0(work_dir, "admin.txt"), append = FALSE, row.names = FALSE, col.names = FALSE, sep = "\t")
  } else {
    users_connected <- fread(paste0(work_dir, "admin.txt"))
    users_connected <- data.frame(V1 = session$token, V2= Sys.time(),
                                  V3 = clone_name,
                                  V4 = "enter",
                                  V5 = users_connected$V5[nrow(users_connected)] + 1 ) 
    write.table(x = users_connected, file = paste0(work_dir, "admin.txt"), append = TRUE, row.names = FALSE, col.names = FALSE, sep = "\t") 
  }
  
  onSessionEnded(function() {
    users_connected <- fread(paste0(work_dir, "admin.txt"))
    users_connected <- data.frame(V1 = session$token, V2= Sys.time(),
                                  V3 = clone_name,
                                  V4 = "exit",
                                  V5 = users_connected$V5[nrow(users_connected)] - 1)
    write.table(x = users_connected, file = paste0(work_dir, "admin.txt"),
                append = TRUE, row.names = FALSE, col.names = FALSE, sep = "\t")
    session$close()
  })
  
  waiter_hide()
  
}
