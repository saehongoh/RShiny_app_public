#!/bin/bash
# */5 * * * * /srv/shiny-server/dashboard/code/backend.sh
echo "$(date)" >> /srv/shiny-server/dashboard/code/crontab_log.txt
/usr/local/bin/Rscript  /srv/shiny-server/dashboard/code/backend.R
/usr/local/bin/Rscript  /srv/shiny-server/dashboard/code/days_since.R
/usr/local/bin/Rscript  /srv/shiny-server/dashboard/code/orderbook.R

# echo 'd *' | mail -N
# rm -r /Users/eoh/.rtweet_token*
