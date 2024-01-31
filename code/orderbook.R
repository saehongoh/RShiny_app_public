library(jsonlite)
library(glue)
require(dplyr)

work_dir = "/srv/shiny-server/dashboard/"

tmp <- readRDS(paste0(work_dir, "newdata/data.rds")) %>%
  filter(!(symbol %in% c("BNB","XRP","LUNA","FTM","VET","ONE","MOON")))
res <- vector('list', nrow(tmp))

for(i in 1:nrow(tmp)){
  
  tmp2 <- tmp %>%
    filter(symbol == tmp$symbol[i])
  z_score_bins <- seq(-5, 5, 0.1)
  lims <- data.frame(symbol=tmp2$symbol, z_score_bins, z_lims = tmp2$current.price+(tmp2$wtd.sd*z_score_bins))
  
  # print(tmp2$symbol)
  
  coinbase_pair <- paste0(tmp2$symbol, "-USD")
  coinbase_url = glue("https://api.pro.coinbase.com/products/{coinbase_pair}/book?level=3")
  dat_coinbase <- fromJSON(coinbase_url)
  dat_coinbase <- data.frame(cbind("coinbase", rbind(cbind("bids", dat_coinbase$bids), cbind("asks", dat_coinbase$asks))))[,1:4]
  colnames(dat_coinbase) <- c("exchange","order","price","units")
  
  # binance_pair <- paste0(tmp2$symbol, "USDT")
  # binance_url = glue("https://api.binance.com/api/v3/depth?limit=5000&symbol={binance_pair}")
  # dat_binance <- fromJSON(binance_url)
  # dat_binance <- data.frame(cbind("binance", rbind(cbind("bids", dat_binance$bids), cbind("asks", dat_binance$asks))))
  # colnames(dat_binance) <- c("exchange","order","price","units")
  
  # export <- rbind(dat_binance, dat_coinbase)
  # saveRDS(export, "dashboard/data_orderbook/test.rds", compress = FALSE)
  
  export <- dat_coinbase
  
  export2 <- export %>%
    mutate(price = as.numeric(price), units = as.numeric(units)) %>%
    filter(price >= min(lims$z_lims) & price <= max(lims$z_lims))
  
  export2$z_score_bins <- sapply(1:nrow(export2), function(x) lims$z_score_bins[findInterval(as.numeric(export2$price)[x], lims$z_lims)])
  
  res[[i]] <- export2 %>%
    # filter(exchange == "coinbase") %>%
    mutate(dollars = price*units) %>%
    mutate(z_score_bins = ifelse(order == "asks" & z_score_bins < 0, 0, z_score_bins)) %>%
    group_by(order, z_score_bins) %>%
    dplyr::summarise(dollars = sum(dollars), .groups="drop") %>%
    arrange(z_score_bins) %>%
    left_join(., lims, by = "z_score_bins")
  
}

res <- do.call(rbind, res) 
res$datetime <- unique(tmp$datetime)
saveRDS(res, paste0(work_dir, "data_orderbook/lims_", unique(tmp$datetime),".rds"), compress = FALSE)

########### make z_book
# work_dir = "/Users/eoh/Documents/R_projects/buythedip/dashboard/"

book_dir= paste0(work_dir, "data_orderbook/")
book_files <- list.files(path=book_dir, pattern=".rds",full.names = TRUE,recursive = TRUE)
book_files = data.frame(book_files, created = as.POSIXct(do.call(rbind, strsplit(book_files, "lims_|.rds"))[,2], tz="UTC")) %>%
  arrange(desc(created)) %>%
  filter(created >= max(created) - lubridate::days(14))
# slice(which(row_number() %% 2 == 1))

book <- book_files$book_files %>% purrr::map_df(~readRDS(.)) %>%
  mutate(units = dollars/z_lims) %>%
  arrange(datetime) 

book_now = readRDS(paste0(book_dir, "/", rownames(fileSnapshot(book_dir)$info)[which.max(fileSnapshot(book_dir)$info$mtime)])) %>%
  mutate(units = dollars/z_lims) %>%
  filter(datetime == max(datetime)) %>%
  group_by(datetime, order, symbol) %>%
  summarise(units_now = sum(units),  .groups="drop")

book_now <- book %>%
  group_by(datetime, symbol, order) %>%
  summarise(units = sum(units), .groups="drop") %>%
  arrange(symbol, datetime) %>%
  group_by(symbol, order) %>%
  summarise(units_mean = mean(units, na.rm=TRUE), units_sd = sd(units, na.rm=TRUE),
            .groups="drop") %>%
  left_join(., book_now, by = c("symbol", "order")) %>%
  # mutate(z_book_dollar = (dollar_now - dollar_mean)/dollar_sd) %>%
  mutate(z_book_units = (units_now - units_mean)/units_sd)

saveRDS(book_now, paste0(work_dir, "data_orderbook_z/zlims_", unique(book_now$datetime),".rds"), compress = FALSE)

# book_now %>%
#   ggplot(aes(x=symbol, y=z_book, col=symbol)) +
#   geom_point(show.legend = F) +
#   facet_wrap(~order) +
#   coord_flip() +
#   theme_bw()


