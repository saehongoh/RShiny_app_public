#!/bin/bash
# */5 * * * * /srv/shiny-server/dashboard/code/backend.sh
sudo sync; echo 3 > /proc/sys/vm/drop_caches
echo "$(date)" >> /srv/shiny-server/dashboard/code/historical_log.txt
/usr/local/bin/Rscript  /srv/shiny-server/dashboard/code/hist_widget.R
cp -r /srv/shiny-server/dashboard/www/hist/* /srv/shiny-server/dashboard_s1/www/hist/
cp -r /srv/shiny-server/dashboard/www/hist/* /srv/shiny-server/dashboard_s2/www/hist/
cp -r /srv/shiny-server/dashboard/www/hist/* /srv/shiny-server/dashboard_s3/www/hist/
cp -r /srv/shiny-server/dashboard/www/hist/* /srv/shiny-server/dashboard_s4/www/hist/
# echo 'd *' | mail -N
# rm -r /Users/eoh/.rtweet_token*
