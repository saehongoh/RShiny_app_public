# Backend

print(Sys.time())
print("initiating script")
suppressMessages(library(dplyr))
suppressMessages(library(geckor))
suppressMessages(library(Hmisc))
suppressMessages(library(lubridate))
suppressMessages(library(data.table))
suppressMessages(library(coinmarketcapr))
# work_dir = "/Users/eoh/Documents/R_projects/buythedip/dashboard/"
work_dir = "/srv/shiny-server/dashboard/"
source(paste0(work_dir,"code/fns.R"))

###############################
###### FETCH COIN PRICES ######
###############################

### New CMC API for shiny app
### 333 calls per day, 10,000 calls a month
### CMC current price
cmc_cryptos = c("BTC","ETH","ADA","BNB","XRP","DOGE","SOL","DOT","MATIC","SHIB",
                "xMOON","FTM","GRT","LTC","LRC","ONE","AVAX","CRO","LINK","LUNA",
                "ATOM","VET","MANA")

cmc_cryptos_set2 = c("AXS","ALGO","AAVE","AMP","QNT"
                     # "AXS","XTZ","AAVE","GALA","QNT",
                     # "MKR","ZEC","ENJ","AMP", "BAT"
                     )

cmc_cryptos <- c(cmc_cryptos, cmc_cryptos_set2)

coinmarketcapr::setup(api="cef055c6-5f45-42a0-8c48-a5f603411679")
coins <- coinmarketcapr::get_crypto_listings('USD', percent_change_1h=T)

price <- coins %>%
  filter(symbol %in% cmc_cryptos) %>%
  mutate(coin_id_cmc = slug, current.price = USD_price, dominance = USD_market_cap_dominance) %>%
  # rename(coin_id_cmc = coin_id) %>%
  select(symbol, coin_id_cmc, current.price, dominance) 

# coin_db <- readRDS(paste0("dashboard/", "code/gecko_coin_db.rds")) %>%
coin_db <- readRDS(paste0(work_dir, "code/gecko_coin_db.rds")) %>%
  mutate(symbol = toupper(symbol)) %>%
  rename(coin_id_gecko = coin_id)

to_omit <- paste0("peg|golden|oec|fitmin|one|wrapped|unicorn|universe|bat-finance|genesis-mana")
to_fetch <- merge(price, coin_db, by ="symbol") %>%
  filter(!(grepl(to_omit,coin_id_gecko))) %>%
  rename(coin_id = coin_id_gecko)

to_fetch <- rbind(to_fetch, 
                  data.frame(symbol ="MOON", coin_id_cmc = NA, current.price = 0, dominance = 0, coin_id = "moon", name = "reddit moons"))
### Need bash wrapper for cronjob
### Coingecko 14d prices

cryptos = unique(to_fetch$coin_id)

coin_fetcher <- function(coin, d){
  # print(coin)
  ex <- geckor_v3(coin_id = coin, vs_currency = "usd", days = d, max_attempts = 2) %>% 
        mutate(d=d) %>% 
        mutate(n=rev(1:n()))
      # Sys.sleep(.5)
      return(ex)
  }

# system.time(
tmp <- do.call(rbind, lapply(1:length(cryptos), function(x) coin_fetcher(cryptos[x], d=14)))
# )
# tmp <- do.call(rbind, lapply(1:length(cryptos_new), function(x) coin_fetcher(cryptos_new[x], d=14)))

if(!exists("tmp")){
  tmp <- do.call(rbind, lapply(1:length(cryptos), function(x) coin_fetcher(cryptos[x], d=14)))
} else {print("passed")}

### Merge two databases

to_fetch <- to_fetch[,c("coin_id","current.price","dominance")]

if(to_fetch[to_fetch$coin_id == "bitcoin",]$current.price < 1000000){
  
  tmp <- merge(tmp, to_fetch, by="coin_id") %>%
    arrange(-n) %>%
    group_by(coin_id) %>%
    mutate(current.price = ifelse(grepl("binancecoin|moon", coin_id), price[which.max(n)], current.price))
  
} else {
  
  tmp <- tmp %>% 
    group_by(coin_id) %>% 
    mutate(current.price = price[which.max(timestamp)]) %>%
    mutate(dominance = 0)
  
}

### Get current time
timenow = round(Sys.time(), units = "mins")
# isitanhour <- minute(timenow) %in% 0:5

### Massage data

export <- tmp %>%
  group_by(coin_id) %>%
  mutate(current.volume = total_volume[which.max(timestamp)]) %>%
  group_by(coin_id, current.price, current.volume, dominance, d) %>%
  mutate(volume_correction = total_volume/max(total_volume)) %>%
  mutate(vol_adj_price = price*volume_correction) %>%
  summarise(mkt.cap = market_cap[which.max(timestamp)], 
            vol.adj.14d.mean = Hmisc::wtd.mean(x=price, w=volume_correction, na.rm=TRUE), 
            wtd.sd = sqrt(Hmisc::wtd.var(price, w=volume_correction, na.rm=TRUE)), 
            vol.14d.mean=Hmisc::wtd.mean(x=total_volume, na.rm=TRUE), 
            vol.sd = sqrt(Hmisc::wtd.var(total_volume, na.rm=TRUE)), 
            .groups = 'drop') %>%
  arrange(-mkt.cap) %>%
  mutate(per.diff = (current.price-vol.adj.14d.mean)/vol.adj.14d.mean*100, 
         abs.diff = abs(per.diff),
         volatility = abs((wtd.sd)/vol.adj.14d.mean*100), 
         z.score = (current.price-vol.adj.14d.mean)/wtd.sd) %>%
  select(-d) %>%
  mutate(coin_id = gsub("r/cc moons","moon",coin_id)) %>%
  select_if(., !grepl("level",colnames(.))) %>%
  # mutate(datetime = as.POSIXct(unlist(strsplit(input, "tmp_|.rds"))[[3]], tz="UTC")) %>%
  mutate(datetime = as.POSIXct(timenow)) %>%
  left_join(., cryptodigits, by="coin_id") %>%
  select(datetime, everything()) %>%
  mutate(z.score_lim = ifelse(z.score > 4.5, 4.5, z.score)) %>%
  mutate(z.score_lim = ifelse(z.score_lim < -4.5, -4.5, z.score_lim)) %>%
  mutate(z.vol = (current.volume-vol.14d.mean)/vol.sd,
         vol_volatility = abs((vol.sd)/vol.14d.mean*100)) %>%
  mutate(z.vol_lim = ifelse(z.vol > 4.5, 4.5, z.vol)) %>%
  mutate(z.vol_lim = ifelse(z.vol_lim < -4.5, -4.5, z.vol_lim)) %>%
  mutate(symbol = toupper(symbol)) %>%
  mutate(hov_text_z = paste0("<br><b>",symbol,"</b> | z-price: ",round(z.score, 2),
                             "\nprice volatility: ", round(volatility,2), "%")) %>%
  mutate(hov_text_vol = paste0("<br><b>",symbol,"</b> | z-volume: ",round(z.vol, 2),
                               "\nvolume volatility: ", round(vol_volatility,2), "%")) %>%
  mutate(hov_text_scat = paste0("<br><b>",symbol,"</b>\nz-price: ",round(z.score, 2),"\n",
                                "z-volume: ",round(z.vol, 2))) %>%
  mutate(hov_text_scat_vol = paste0("<br><b>",symbol,"</b>\nprice volatility: ",round(volatility, 1),"%\n",
                                    "volume volatility: ",round(vol_volatility, 1),"%"))

### Save data
# write.csv(export, paste0(work_dir, "data/tmp_",timenow,".csv"))
# old_list <- export %>%
#   filter(symbol %in% cmc_cryptos)
# new_list <- export 

saveRDS(export, paste0(work_dir, "data_rds/V3tmp_",timenow,".rds"), compress = FALSE)
# saveRDS(new_list, paste0(work_dir, "data_new/V3_",timenow,".rds"), compress = FALSE)
saveRDS(export, paste0(work_dir, "newdata/data.rds"), compress = FALSE)
# saveRDS(export, paste0("dashboard/", "data_rds/V2tmp_",timenow,".rds"), compress = FALSE)
# export <- readRDS("dashboard/tmp_2021-12-09 20:16:00.rds")

