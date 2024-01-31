###### Restart shiny
require(dplyr)
users_connected <- rbind(data.frame(token = "reset", 
                                    time = Sys.time(),
                                    clone="reset",
                                    action="reset",
                                    total=0) 
)

write.table(x = users_connected, file = paste0("/srv/shiny-server/dashboard/admin.txt"), append = TRUE, row.names = FALSE, col.names = FALSE, sep = "\t")
