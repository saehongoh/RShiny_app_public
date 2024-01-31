# Master

suppressMessages(library(shiny))
# suppressMessages(library(shinydashboard))
# suppressMessages(library(shinydashboardPlus))
suppressMessages(library(shinyjs))
suppressMessages(library(shinycssloaders))
suppressMessages(library(shinyMobile))
suppressMessages(library(shinyWidgets))
suppressMessages(library(plotly))
suppressMessages(library(waiter))
suppressMessages(library(reactable))
# library(shinyBS)
options(java.parameters = "-Xss2560k")

if(Sys.getenv()[[10]] == "/Users/eoh"){
  work_dir = "/Users/eoh/Documents/R_projects/buythedip/dashboard/"
  # dir = paste0(work_dir,"data_srv")
} else if(Sys.getenv()[[9]] == "/Users/admin"){
  work_dir = "/Volumes/2TB/gitRepos/buythedip/dashboard/"
} else {
  work_dir = "/srv/shiny-server/dashboard/"
  # dir = paste0(work_dir,"data")
}

dir = paste0(work_dir,"data_rds")
source(paste0(work_dir,"code/fns.R"))
# source(paste0(work_dir,"code/google_ads.R"))
source(paste0(work_dir,"code/f7mods.R"))
voting_options <- readRDS(paste0(work_dir,"code/voting_options.rds"))

linebreaks <- function(n){HTML(strrep(br(), n))}

options(spinner.color="#b0b0b0", spinner.color.background="#ffffff", spinner.size=1)
spinner <- tagList(
  spin_solar(),
  span("", style="color:white; line-height:50px; font-size: 12px")
)

timeoutSeconds <- 3000

inactivity <- sprintf("function idleTimer() {
var t = setTimeout(logout, %s);
window.onmousemove = resetTimer; // catches mouse movements
window.onmousedown = resetTimer; // catches mouse movements

function logout() {
Shiny.setInputValue('timeOut', '%ss')
}

function resetTimer() {
clearTimeout(t);
t = setTimeout(logout, %s);  // time is in milliseconds (1000 is 1 second)
}
}
idleTimer();", timeoutSeconds*1000, timeoutSeconds, timeoutSeconds*1000)

dipper_cryptos = c("bitcoin","ethereum","cardano","binancecoin","ripple","dogecoin","solana","polkadot","matic-network","vechain","moon","terra-luna","crypto-com","litecoin","harmony")


ui <- f7Page_mod(
  tags$head(tags$link(rel = "shortcut icon", href = "favicon.ico")),
  includeCSS(paste0(work_dir, "www/styles_v2.css")),
  tags$head(includeHTML(paste0(work_dir, "www/header_elements.html"))),
  useShinyjs(),
  extendShinyjs(text = inactivity, functions = 'idleTimer'),
  use_waiter(),
  waiter_show_on_load(spinner,  color = "#1C1C1D"),
  title = "The Dip Index",
  # position = "fixed-top",
  iosTranslucentBars = FALSE,
  # hideOnPageScroll = FALSE,
  f7TabLayout(
    panels = tagList(
      f7Panel(title = "v2.3.0", side = "left", theme = "dark",resizable = TRUE, effect="cover",
              # hideOnPageScroll = TRUE,
              f7PanelMenu(id="menu",
                          f7PanelItem(tabName = "dashboard", title= "dashboard", icon = icon("tachometer-alt"), active = TRUE),
                          f7PanelItem(tabName = "vote", title= "vote", icon = icon("vote-yea")),
                          # f7Link(label = "@bitcoindips", icon=icon("hashtag"), href="https://www.twitter.com/bitcoindips"),
                          # linebreaks(1),
                          f7PanelItem(tabName = "readme", title= "readme::dashboard", icon = icon("book-open")),
                          f7PanelItem(tabName = "twitter", title= "readme::twitter", icon = icon("hashtag")),
                          f7PanelItem(tabName = "versions", title= "versions", icon = icon("code-branch")),
                          f7PanelItem(tabName = "donate", title= "donate", icon = icon("hands-helping"))
                          )
      ),
      f7Panel(title = "Settings", side = "right", theme = "dark", effect = "cover", resizable = TRUE, 
              f7Row(
              wellPanel("Stay up to date by following" , a("The Dip Index @bitcoindips", href="https://twitter.com/bitcoindips",target="_blank")," on Twitter.",
                        linebreaks(2)
                        # HTML("If you like to support The Dip Index, <b>please click on some ads</b> whenever you see them! Thank you!"), linebreaks(1)
              )
              )
      )
    ),
    navbar = f7Navbar_mod( 
      title = img(src='thedipindex_dashboard.png', height="100%", width="100%", align = "center"),
      hairline = FALSE,
      shadow = TRUE,
      leftPanel = TRUE,
      rightPanel = TRUE
    ),
    toolbar = f7Toolbar(
      position = "bottom",scrollable = FALSE
    ),
    f7Items(
      f7Item(tabName = "vote",
      tags$head(tags$style(HTML('.content-wrapper {color: #ffffff;}'))),
      f7Row(
            f7Col(
              f7Card(
                h4("Vote on which coins should be added to the twitter updates."),
                "1. The choices are limited to top 100 cryptos according to coinmarketcap. I can't get enough data for coins outside top 100 to run the calculations.", linebreaks(2),
                "2. You can actually vote multiple times over unique sessions. To implement a vote limit, I would have to grab an identifying information
                   from you, whether it'd be email or IP through a third party, which I decided not to do. So please vote in moderation.", linebreaks(2),
                "3. Voting will close on the first of every month (2pm EST) and changes will be implemented within 24-48 hours. The results will be posted on the twitter page.", linebreaks(2),
                "4. To avoid overcrowding, a coin will be removed at the same time.", linebreaks(2),
                f7Picker(
                  inputId = "vote_for",
                  label = "Add to twitter updates (sorted by market cap)",
                  # type = "popup",
                  placeholder = NULL,
                  rotateEffect = FALSE,
                  scrollToInput = TRUE,
                  # value = NULL,
                  # closeOnSelect = TRUE,
                  choices = c("select coin", voting_options$options)
                ),
                linebreaks(3),
                f7Picker(
                  inputId = "vote_against",
                  label = "Remove from twitter updates",
                  # type = "popup",
                  placeholder = NULL,
                  scrollToInput = TRUE,
                  rotateEffect = FALSE,
                  # value = NULL,
                  # closeOnSelect = TRUE,
                  choices = c("select coin", toupper(dipper_cryptos))
                ),
                linebreaks(3),
                actionBttn(
                  inputId = "vote_go",
                  label = "vote",
                  style="bordered",
                  color="success",
                  block=FALSE,
                  size="sm"
                )
              ),
              f7Card(
                textOutput("vote_message")
              )
            )
      )
      ),
      f7Item(tabName = "readme",
             tags$head(tags$style(HTML('.content-wrapper {color: #ffffff;}'))),
             f7Row(
               f7Col(htmltools::tags$iframe(src=base64enc::dataURI(file = paste0(work_dir, "www/about/readme.html"), mime = "text/html"), seamless="seamless",
                                            # htmltools::tags$iframe(src="https://buythedips.io/about/aboutMole.html", seamless="seamless", 
                                            height=6500, width="100%", frameBorder=0)
               )
             )
      ),
      f7Item(tabName = "twitter",
             tags$head(tags$style(HTML('.content-wrapper {color: #ffffff;}'))),
             f7Row(
               f7Col(htmltools::tags$iframe(src=base64enc::dataURI(file = paste0(work_dir, "www/about/aboutMole.html"), mime = "text/html"), seamless="seamless",
                                            # htmltools::tags$iframe(src="https://buythedips.io/about/aboutMole.html", seamless="seamless", 
                                            height=6500, width="100%", frameBorder=0)
               )
             )
      ),
      f7Item(tabName = "versions",
             tags$head(tags$style(HTML('.content-wrapper {color: #ffffff;}'))),
             f7Row(
               f7Col(htmltools::tags$iframe(src=base64enc::dataURI(file = paste0(work_dir, "www/about/versions.html"), mime = "text/html"), seamless="seamless",
                                            # htmltools::tags$iframe(src="https://buythedips.io/about/aboutMole.html", seamless="seamless", 
                                            height=6000, width="100%", frameBorder=0)
               )
             )
      ),
      f7Item(tabName = "donate",
             tags$head(tags$style(HTML('.content-wrapper {color: #ffffff;}'))),
             f7Row(
               f7Col(htmltools::tags$iframe(src=base64enc::dataURI(file = paste0(work_dir, "www/about/donate.html"), mime = "text/html"), seamless="seamless",
                                            # htmltools::tags$iframe(src="https://buythedips.io/about/aboutMole.html", seamless="seamless", 
                                            height=6000, width="100%", frameBorder=0)
               )
             )
      ),
      f7Item(tabName = "dashboard",
             style="position:relative; height: 100%; overflow-y: hidden;",
             f7Tabs(
               animated = TRUE,
               #swipeable = TRUE,
               f7Tab(
                     tabName = "z-scores",
                     icon = f7Icon("bell"),
                     active = TRUE,
                     # splitLayout(cellWidths = c("78%", "21%"),style="position:relative; top:-60px",
                     f7Row(style="position:relative; top:-85px;",
                           f7Col(
                             f7Segment(container=c("segment"), 
                                       strong=FALSE,
                                       # shawdow=FALSE,
                                       f7Button(inputId= "fb1", color = NULL, size ="small", label = "top", outline = FALSE, fill = FALSE),
                                       f7Button(inputId= "fb2", color = NULL, size ="small", label = "currency", outline = FALSE, fill = FALSE),
                                       f7Button(inputId= "fb3", color = NULL, size ="small", label = "smart contract platform", outline = FALSE, fill = FALSE),
                                       f7Button(inputId= "fb4", color = NULL, size ="small", label = "exchange token", outline = FALSE, fill = FALSE),
                                       f7Button(inputId= "fb5", color = NULL, size ="small", label = "memecoin", outline = FALSE, fill = FALSE),
                                       f7Button(inputId= "fb6", color = NULL, size ="small", label = "web3", outline = FALSE, fill = FALSE)
                             )
                           )
                     ),
                     f7Row(style="position:relative; top:-115px",
                           f7Col(
                             f7Card(
                               reactableOutput("reactable", height="770px"),
                               linebreaks(1),
                               actionBttn(
                                 inputId = "filterGO",
                                 label =  HTML('filter::cryptos<div title="Did the filter table not load for you?&#013;&#013;Please write down your user_token and send it to me on Twitter (@bitcoindips) or on Reddit (u/ahjustsea)."><i id="help_table" class="f7-icons">question_diamond</i></div>'),
                                 style ="simple",
                                 size ="sm",
                                 color="primary"
                               ),
                               # HTML('<div title="Did the filter table not load for you?&#013;&#013;Please write down your user_token and send it to me on Twitter (@bitcoindips) or on Reddit (u/ahjustsea)."><i id="help_table" class="f7-icons">question_diamond</i></div>'),
                               textOutput("filter_message", inline=TRUE)
                               # HTML('<div title="Did the filter table not load for you?&#013;&#013;Please write down your user_token and send it to me on Twitter (@bitcoindips) or on Reddit (u/ahjustsea)."><i id="help_table" class="f7-icons">question_diamond</i></div>')
                             )
                           ),
                           f7Col(
                             f7Card(style="height:400px;",
                                    plotlyOutput("hist_zprice") 
                                    %>% withSpinner(hide.ui = FALSE),
                                    HTML('<div title="z-scores of cryptocurrency prices in the last 48 hours.&#013;&#013;Please see z-scores graph(s) for more info.&#013;&#013;Historical z-price can be found on historicals tab"><i id="help_icon" class="f7-icons">question_diamond</i></div>')
                             ),
                             f7Card(style="height:400px;",
                                    plotlyOutput("performance") 
                                    %>% withSpinner(hide.ui = FALSE),
                                    HTML('<div title="Performance (expressed as percentage) is the difference between the current price and the two-week volume-adjusted mean price."><i id="help_icon" class="f7-icons">question_diamond</i></div>')
                             )
                           ),
                           f7Col(
                             f7Card(style="height:400px;",
                                    plotlyOutput("plot_price") 
                                    %>% withSpinner(hide.ui = FALSE),
                                    HTML('<div title="z-scores of cryptocurrency prices are measured in standard deviations from the mean prices.&#013;&#013;The halo diamater corresponds to current volatility in price (see volatility tab).&#013;&#013;Mean cryptocurrency prices are calculated from two weeks worth of data fetched in 5 minute intervals (4032 data points) from 2 data sources (coingecko and coinmarketcap)"><i id="help_icon" class="f7-icons">question_diamond</i></div>')
                             ),
                             f7Card(style="height:400px;",
                                    plotlyOutput("plot_volume") 
                                    %>% withSpinner(hide.ui = FALSE),
                                    HTML('<div title="z-scores of cryptocurrency trading volumes are measured in standard deviations from the mean trading volume.&#013;&#013;The halo diamater corresponds to current volatility in trading volume (see volatility tab).&#013;&#013;Mean trading volumes are calculated from two weeks worth of data fetched in 5 minute intervals (4032 data points) from 2 data sources (coingecko and coinmarketcap)"><i id="help_icon" class="f7-icons">question_diamond</i></div>')
                             )
                           ),
                           f7Col(
                             f7Card(style="height:400px;",
                                    plotlyOutput("plot_scatter") 
                                    %>% withSpinner(hide.ui = FALSE),
                                    HTML('<div title="Scatter plot of two z-scores (price and volume).&#013;&#013;More info can be found in the readme"><i id="help_icon" class="f7-icons">question_diamond</i></div>')
                             ),
                             f7Card(style="height:400px;",
                                    plotlyOutput("dominance") 
                                    %>% withSpinner(hide.ui = FALSE),
                                    HTML('<div title="Changes in cryptocurrency market cap normalized against bitcoin (i.e., bitcoin will always be 1).&#013;&#013;Greater than 1 means that its gaining ground on bitcoin.&#013;&#013;Less than 1 means that its losing ground on bitcoin."><i id="help_icon" class="f7-icons">question_diamond</i></div>')
                             )
                           )
                     ),
                     f7Row(style="position:relative; top:-115px;  padding-bottom: 50px;",
                           f7Col(
                             f7Card(
                               f7Link("The Dip Index %>% lite", href="https://buythedips.io/lite/"), "version is more suitable for mobile.", linebreaks(1),
                               "Stay up to date by following" , f7Link("The Dip Index @bitcoindips", href="https://twitter.com/bitcoindips")," on Twitter.",
                               # linebreaks(2),
                               "Don't forget to vote for twitter updates!",
                               linebreaks(1)
                             )
                           ),
                           f7Col(
                             f7Card(
                               "Dashboard updates every 5 minutes.", linebreaks(1),
                               "Last updated: ", htmlOutput("last_updated1", inline=TRUE), ".",
                               " User token: ", textOutput("user_token1", inline=TRUE)
                             )
                           )
                     )
               ),
               f7Tab(
                 tabName = "orderbook",
                 icon = f7Icon("layers_alt"),
                 active = FALSE,
                 # f7Row(style="position:relative; top:-60px",
                 #   htmltools::tags$iframe(src="http://127.0.0.1:4290", height=1850, width="100%", frameBorder=0)
                 # ),
                 f7Row(style="position:relative; top:-60px",
                       f7Col(
                         f7Card(plotlyOutput("orderbook_zsummary", height="635px") %>% withSpinner(hide.ui = FALSE))
                       ),
                       f7Col(
                         f7Card(reactableOutput("orderbook_table", height="600px")),
                         actionBttn(
                           inputId = "filterOB",
                           label =  HTML('filter::cryptos<div title="Did the filter table not load for you?&#013;&#013;Please write down your user_token and send it to me on Twitter (@bitcoindips) or on Reddit (u/ahjustsea)."><i id="help_table" class="f7-icons">question_diamond</i></div>'),
                           style ="simple",
                           size ="sm",
                           color="primary"
                         )
                       )
                 ),
                 f7Row(style="position:relative; top:-60px",
                   f7Col(
                     f7Card(plotlyOutput("bookz_historical") %>% withSpinner(hide.ui = FALSE))
                   ),
                   f7Col(
                     f7Card(plotlyOutput("orderbook_historical") %>% withSpinner(hide.ui = FALSE))
                   ),
                   f7Col(
                     f7Card(plotlyOutput("spread") %>% withSpinner(hide.ui = FALSE))
                   )
                 ),
                 f7Row(style="position:relative; top:-60px",
                       f7Col(
                         f7Card(plotlyOutput("orderbook_byz", height="600px") %>% withSpinner(hide.ui = FALSE))
                       )
                 ),
                 f7Row(
                   style="position:relative; top:-60px; padding-bottom: 50px;",
                   f7Col(
                     f7Card(
                       "Orderbook data is in beta. Data quality should improve as the application gathers more data.", linebreaks(1),
                       "Updates every 5 minutes. Last updated: ",
                       htmlOutput("orderbook_update", inline=TRUE),
                       "."
                       # " User token: ", textOutput("user_token2", inline=TRUE)
                     )
                   )
                 )
               ),
               f7Tab(
                 tabName = "historicals",
                 icon = f7Icon("waveform_path_ecg"),
                 active = FALSE,
                 f7Row( style="position:relative; top:-60px", 
                         f7Col(
                           f7Card(HTML(" z-score for price | "), 
                                  f7Toggle(
                                    inputId = "zprice_duration",
                                    label = "max duration"
                                  )
                                  ),
                         uiOutput("historical_zprice")
                         )
                 ), 
                 linebreaks(2),
                 f7Row(  style="position:relative; top:-60px", 
                         f7Col(
                           f7Card(HTML(" How well has price movements correlated between coins in the last two weeks? |<a href='https://buythedips.io/hist/corr_zscore.html' target='_blank'> full screen </a>| ")),
                           htmltools::tags$iframe(src="https://buythedips.io/hist/corr_zscore.html", height=450, width="100%", frameBorder=0)
                         )
                 ), 
                 linebreaks(2),
                 f7Row(  style="position:relative; top:-60px", 
                         f7Col(
                           f7Card(HTML(" z-score for volume | "), 
                                  f7Toggle(
                                    inputId = "zvolume_duration",
                                    label = "max duration"
                                  )
                           ),
                           uiOutput("historical_zvolume")
                         )
                 ), 
                 linebreaks(2),
                 f7Row(  style="position:relative; top:-60px", 
                         f7Col(
                           f7Card(HTML(" How well has trading volumes correlated between coins in the last two weeks? |<a href='https://buythedips.io/hist/corr_zvolume.html' target='_blank'> full screen </a>| ")),
                           htmltools::tags$iframe(src="https://buythedips.io/hist/corr_zvolume.html", height=450, width="100%", frameBorder=0)
                         )
                 ), 
                 linebreaks(2),
                 f7Row(  style="position:relative; top:-60px", 
                         f7Col(
                           f7Card(HTML(" performance | "), 
                                  f7Toggle(
                                    inputId = "performance_duration",
                                    label = "max duration"
                                  )
                           ),
                           uiOutput("historical_performance")
                         )
                 ), 
                 linebreaks(4),
                 f7Row(
                   style="position:relative; top:-60px",
                   f7Col(
                     f7Card(
                       "Historical charts update once every hour.", linebreaks(1)
                     )
                   )
                 )
               ),
               f7Tab(
                 tabName = "days",
                 icon = f7Icon("hourglass"),
                 active = FALSE,
                 f7Row(style="position:relative; top:-60px",
                       f7Col(
                         plotlyOutput("days_since") 
                         %>% withSpinner(hide.ui = FALSE)
                       )
                 ),
                 f7Row(style="position:relative; top:150px",
                       f7Col(
                         f7Card(
                           plotlyOutput("plot_vol_price") 
                           %>% withSpinner(hide.ui = FALSE)
                         )
                       ),
                       f7Col(
                         f7Card(
                           plotlyOutput("plot_vol_volume") 
                           %>% withSpinner(hide.ui = FALSE)
                         )
                       )
                 ),
                 f7Row(
                   style="position:relative; top: 150px; padding-bottom: 50px;",
                   f7Col(
                     f7Card(
                       "Dashboard updates every 5 minutes.", linebreaks(1),
                       "Last updated: ", htmlOutput("last_updated3", inline=TRUE), "."
                       # " User token: ", textOutput("user_token3", inline=TRUE)
                     )
                   )
                 )
               ),
               f7Tab(
                 tabName = "limits",
                 icon = f7Icon("money_dollar_circle"),
                 active = FALSE,
                 # uiOutput("limits_ui"),
                 f7Row(style="position:relative; top:-60px; background-color: #000000;", 
                       f7Card(
                         HTML("This page is designed to help you setup your limit buys and sells to see how it would affect your breakeven scores. Many of my early twitter followers will know this feature as Mole, Connoisseur of Flavourful Dips! Mole is hoping that this feature will prevent panic sells and FOMO buys. Mole suggests that you just set these limits and go enjoy life!"),
                         linebreaks(1)
                       ),
                 ),
                 f7Row(style="position:relative; top:-60px; background-color: #000000;", 
                       f7Col(style="background-color: #000000;",
                             f7Card(style="background-color: #000000;",
                                    h3("Mole's personalized buy limits")
                             )
                       )
                 ),
                 f7Row(# style = "position:relative; top: -10px; left:0px; padding-left:25px; padding-right:25px; ", 
                   # f7Col(style="width:300px; padding: 10px; text-align: center; margin: auto;",
                   style="position:relative; top:-60px; background-color: #1c1c1d;",
                   f7Col(
                     style="position:relative; top:-20px;",
                     f7Card(
                       style="padding-top:0px;",
                       f7Picker(inputId = "buy_what",
                                placeholder = cryptodigits$symbol[1],
                                label = "pick crypto",
                                rotateEffect = FALSE,
                                choices = cryptodigits$symbol,
                       )
                     ),
                     f7Card(
                       style="padding-top:0px;",
                       f7Picker(inputId = "buy_currency",
                                placeholder = "USD",
                                rotateEffect = FALSE,
                                label = "pick currency",
                                choices = c("USD", "CAD", "EUR", "GBP", "INR", "MYR", "TRY")
                       )
                     )),
                   f7Col(style="align:center;",
                         f7Card(
                           f7Text(inputId = "buy_howmuch",label = "what's your budget?", 
                                  value = c(100))
                         ),
                         f7Card(
                           sliderTextInput(
                             inputId = "buy_strategy",
                             label = "pick a buy strategy:", 
                             grid = TRUE, 
                             width="100%", 
                             force_edges = TRUE,
                             choices = c("unbiased", "significant", 
                                         "laidback", "bell", "aggressive")
                           )
                         )
                   ),
                   f7Col(
                     f7Card(
                       f7Text(inputId = "buy_owned",label = "owned units (optional)", 
                              value=c(0))),
                     f7Card(
                       f7Text(inputId = "buy_invested",label = "total invested (optional)", 
                              value=c(0)))
                   )
                 ),
                 f7Row(style="position:relative; top:-60px; background-color: #000000;",
                       f7Col(style="background-color: #000000;",
                             f7Card(style="background-color: #000000;",
                                    f7Button(inputId="mole_buylimits", label = "get limit buys",
                                             color="green",
                                             size="small" 
                                    )
                                    # textOutput("buy_text")
                             )
                       )
                 ),
                 f7Row(style="position:relative; top:-60px; background-color: #000000;",
                       f7Col(style="background-color: #000000;",
                             f7Card(style="background-color: #000000;",
                                    reactableOutput("buylimits")
                             )
                       )
                 ), 
                 ####### Sell Limit
                 f7Row(style="position:relative; top:-60px; background-color: #000000;", 
                       f7Col(style="background-color: #000000;",
                             f7Card(style="background-color: #000000;",
                                    h3("Mole's personalized sell limits")
                             )
                       )
                 ),
                 f7Row(# style = "position:relative; top: -10px; left:0px; padding-left:25px; padding-right:25px; ", 
                   # f7Col(style="width:300px; padding: 10px; text-align: center; margin: auto;",
                   style="position:relative; top:-60px; background-color: #1c1c1d;",
                   f7Col(
                     style="position:relative; top:-20px;",
                     f7Card(
                       style="padding-top:0px;",
                       f7Picker(inputId = "sell_what",
                                placeholder = cryptodigits$symbol[1],
                                label = "pick crypto",
                                rotateEffect = FALSE,
                                choices = cryptodigits$symbol,
                       )
                     ),
                     f7Card(
                       style="padding-top:0px;",
                       f7Picker(inputId = "sell_currency",
                                placeholder = "USD",
                                label = "pick currency",
                                rotateEffect = FALSE,
                                choices = c("USD", "CAD", "EUR", "GBP", "INR", "MYR", "TRY")
                       )
                     )),
                   f7Col(style="align:center;",
                         f7Card(
                           f7Text(inputId = "sell_howmany",label = "how many are you looking to sell?", 
                                  value = c(100))
                         ),
                         f7Card(
                           sliderTextInput(
                             inputId = "sell_strategy",
                             label = "pick a sell strategy:", 
                             grid = TRUE, 
                             width="100%", 
                             force_edges = TRUE,
                             choices = c("unbiased", "significant", 
                                         "laidback", "bell", "aggressive")
                           )
                         )
                   ),
                   f7Col(
                     f7Card(
                       f7Text(inputId = "sell_owned",label = "owned units (optional)", 
                              value=c(0))),
                     f7Card(
                       f7Text(inputId = "sell_invested",label = "total invested (optional)", 
                              value=c(0)))
                   )
                 ),
                 f7Row(style="position:relative; top:-60px; background-color: #000000;",
                       f7Col(style="background-color: #000000;",
                             f7Card(style="background-color: #000000;",
                                    f7Button(inputId="mole_selllimits", label = "get limit sells",
                                             color="red",
                                             size="small" 
                                    )
                             )
                       )
                 ),
                 f7Row(style="position:relative; top:-60px; background-color: #000000;",
                       f7Col(style="background-color: #000000;",
                             f7Card(style="background-color: #000000;",
                                    reactableOutput("selllimits")
                             )
                       )
                 ),
                 f7Row(
                   style="position:relative; top:-60px",
                   f7Col(
                     f7Card(
                       "Limits updates every 5 minutes with dashboard data.", linebreaks(1),
                       "Last updated: ", htmlOutput("last_updated5", inline=TRUE), "."
                     )
                   )
                 )
               )
             )
      )
    )
  )
)
