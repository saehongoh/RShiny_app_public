require(dplyr)

getData <- function(input){
    readRDS(input) 
}

#### Directory setup
if(Sys.getenv()[[10]] == "/Users/eoh"){
  work_dir = "/Users/eoh/Documents/R_projects/buythedip/dashboard/"
} else {
  work_dir = "/srv/shiny-server/dashboard/"
}

dir = paste0(work_dir,"data_rds")
source(paste0(work_dir,"code/fns.R"))

file <- paste0(dir, "/", rownames(fileSnapshot(dir)$info)[which.max(fileSnapshot(dir)$info$mtime)])

### Load here
tmp <- getData(file)
days_since <- readRDS(paste0(work_dir, "days_since.rds")) %>% distinct()

newitems <- tmp[!(tmp$symbol %in% days_since$symbol),]$symbol

if(length(newitems) > 0){
  days_since <- rbind(days_since, data.frame(symbol=newitems, dip = NA, moon =NA))
}

newday <- tmp %>%
  group_by(symbol) %>%
  mutate(action = ifelse(z.score <= -2, "dip", ifelse(z.score >= 2, "moon", "normal"))) %>%
  select(datetime, symbol, action) %>%
  group_by(symbol, action) %>%
  summarise(last = max(datetime), .groups="drop") %>%
  filter(action != "normal")

if(nrow(newday) > 0){
  
  for(i in 1:nrow(newday)){
    colu = ifelse(newday$action[i] == "moon", 3, 2)
    days_since[days_since$symbol == newday$symbol[i],colu] <- as.character(newday$last[i]) 
  }
  
}

### Save here
saveRDS(days_since, paste0(work_dir, "days_since.rds"))

if(!file.exists(paste0(work_dir, "dipmoon_log.txt"))){
  write.table(x = newday, file = paste0(work_dir, "dipmoon_log.txt"), append = FALSE, row.names = FALSE, col.names = FALSE, sep = "\t")
} else {
  write.table(x = newday, file = paste0(work_dir, "dipmoon_log.txt"), append = TRUE, row.names = FALSE, col.names = FALSE, sep = "\t") 
}
