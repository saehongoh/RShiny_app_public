suppressMessages(library(dplyr))
suppressMessages(library(geckor))
#### Directory setup
if(Sys.getenv()[[10]] == "/Users/eoh"){
  work_dir = "/Users/eoh/Documents/R_projects/buythedip/dashboard/"
  # dir = paste0(work_dir,"data_srv")
} else {
  work_dir = "/srv/shiny-server/dashboard/"
  # dir = paste0(work_dir,"data")
}

source(paste0(work_dir,"code/fns.R"))

for(i in 1:nrow(cryptodigits)){
  print(i)
  export <- geckor_v3(coin_id = cryptodigits$coin_id[i], vs_currency = "usd", interval = "daily", days = 1)
  export <- rbind(export, readRDS(paste0(work_dir,"data_hist/",cryptodigits$symbol[i],".rds")))
  saveRDS(export, paste0(work_dir,"data_hist/",cryptodigits$symbol[i],".rds"))
}
