#!/bin/bash
# 0 0 * * * /srv/shiny-server/dashboard/code/dailyprice.sh
echo "$(date)" >> /srv/shiny-server/dashboard/code/dailyprice_log.txt
/usr/local/bin/Rscript  /srv/shiny-server/dashboard/code/dailyprice.R

# echo 'd *' | mail -N
# rm -r /Users/eoh/.rtweet_token*