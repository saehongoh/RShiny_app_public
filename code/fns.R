
##############################
######### SHINY STUFF ########
##############################

cryptodigits = data.frame(coin_id = c('bitcoin', 'ethereum', 'binancecoin', 'cardano', 'solana', 
                                      'polkadot', 'ripple', 'shiba-inu', 'dogecoin', 'litecoin', 
                                      'matic-network', 'cosmos', 'the-graph',"harmony", 'moon',
                                      "loopring","avalanche-2", "crypto-com-chain","chainlink","terra-luna",
                                      'fantom', 'vechain','decentraland',
                                      "uniswap","algorand","bitcoin-cash","stellar","filecoin",
                                      "axie-infinity","tezos","aave","gala","quant-network",
                                      "maker","zcash","enjincoin","amp-token","basic-attention-token"),
                          symbol = c('BTC', 'ETH', 'BNB', 'ADA', 'SOL',
                                     'DOT', 'XRP', 'SHIB', 'DOGE', 'LTC', 
                                     'MATIC', 'ATOM', 'GRT', "ONE", 'MOON',
                                     'LRC', "AVAX","CRO","LINK","LUNA",
                                     'FTM','VET','MANA',
                                     "UNI","ALGO","BCH","XLM","FIL",
                                     "AXS","XTZ","AAVE","GALA","QNT",
                                     "MKR","ZEC","ENJ","AMP", "BAT"),
                          digs =  c(2,2,2,5,2,
                                    4,4,8,4,4,
                                    4,3,4,4,4,
                                    4,3,4,2,2,
                                    2,4,4,
                                    3,3,3,5,3,
                                    3,3,3,5,3,
                                    2,3,3,6,6))

vline <- function(x = 0, color = "green") {
  list(
    type = "line",
    y0 = 0,
    y1 = 1,
    yref = "paper",
    x0 = x,
    x1 = x,
    line = list(color = color, alpha=0.5, dash="dot")
  )
}

hline <- function(y = 0, color = "green") {
  list(
    type = "line",
    x0 = 0,
    x1 = 1,
    xref = "paper",
    y0 = y,
    y1 = y,
    line = list(color = color, alpha=0.5, dash="dot")
  )
}

gg_color_hue <- function(n) {
  hues = seq(15, 375, length = n + 1)
  hcl(h = hues, l = 65, c = 100)[1:n]
}

##############################
######### GECKOR STUFF #######
##############################

validate_arguments <- function(arg_max_attempts = NULL,
                               arg_coin_ids = NULL,
                               arg_vs_currencies = NULL,
                               arg_exchange_id = NULL,
                               arg_include_market_cap = NULL,
                               arg_include_24h_vol = NULL,
                               arg_include_24h_change = NULL,
                               arg_date = NULL) {
  if (!is.null(arg_max_attempts)) {
    if (!(is.numeric(arg_max_attempts) & arg_max_attempts > 0)) {
      rlang::abort("`max_attempts` must be a positive number")
    }
  }
  
  if (!is.null(arg_coin_ids)) {
    if (!is.character(arg_coin_ids)) {
      rlang::abort("Coin IDs must be of class character")
    }
  }
  
  if (!is.null(arg_vs_currencies)) {
    if (!is.character(arg_vs_currencies)) {
      rlang::abort("`vs_currencies` must be a character vector")
    }
    
    supported_vs_currencies <- supported_currencies()
    
    if (!all(arg_vs_currencies %in% supported_vs_currencies)) {
      rlang::abort(c(
        "The following base currencies are not currently supported:",
        arg_vs_currencies[!arg_vs_currencies %in% supported_vs_currencies]
      ))
    }
  }
  
  if (!is.null(arg_include_market_cap)) {
    if (!is.logical(arg_include_market_cap)) {
      rlang::abort("`include_market_cap` must be boolean")
    }
  }
  
  if (!is.null(arg_exchange_id)) {
    if (!is.character(arg_exchange_id) | length(arg_exchange_id) != 1) {
      rlang::abort("`exchange_id` must be a single character value")
    }
  }
  
  if (!is.null(arg_include_24h_vol)) {
    if (!is.logical(arg_include_24h_vol)) {
      rlang::abort("`include_24h_vol` must be boolean")
    }
  }
  
  if (!is.null(arg_include_24h_change)) {
    if (!is.logical(arg_include_24h_change)) {
      rlang::abort("`include_24h_change` must be boolean")
    }
  }
  
  if (!is.null(arg_date)) {
    if (class(arg_date) != "Date") {
      rlang::abort("All dates must be of class Date")
    }
  }
}

build_get_request <- function(base_url = "https://api.coingecko.com/api/v3",
                              path,
                              query_parameters) {
  if (!is.character(base_url)) {
    rlang::abort("`base_url` must be a character value")
  }
  
  if (!is.character(path) & !is.null(path)) {
    rlang::abort("`path` must be a character vector or NULL")
  }
  
  if (!is.list(query_parameters) & !is.null(query_parameters)) {
    rlang::abort("`query_parameters` must be a named list or NULL")
  }
  
  
  url <- httr::modify_url(base_url, path = path)
  url <- httr::parse_url(url)
  url$query <- query_parameters
  url <- httr::build_url(url)
  
  return(url)
}

api_request <- function(url, max_attempts = 3) {
  if (!is.character(url)) {
    rlang::abort("`url` must be a character value")
  }
  
  validate_arguments(arg_max_attempts = max_attempts)
  
  ua <- httr::user_agent(
    sprintf(
      "geckor/%s (R client for the CoinGecko API; https://github.com/next-game-solutions/geckor)",
      utils::packageVersion("geckor")
    )
  )
  
  for (attempt in seq_len(max_attempts)) {
    r <- try(httr::GET(url, ua), silent = FALSE)
    
    if (class(r) == "try-error" || httr::http_error(r)) {
      delay <- stats::runif(n = 1, min = attempt, max = 2^attempt)
      message(
        "\nAPI request failed. Retrying after ",
        round(delay, 2), " seconds..."
      )
      Sys.sleep(delay)
    } else {
      break
    }
  }
  
  if (httr::http_error(r)) {
    parsed <- jsonlite::fromJSON(
      httr::content(r, "text"),
      simplifyVector = FALSE
    )
    
    stop(
      sprintf(
        "API request failed [status code %s]. \n%s",
        httr::status_code(r),
        parsed$error
      ),
      call. = FALSE
    )
  }
  
  if (httr::http_type(r) != "application/json") {
    rlang::abort("Returned data are not JSON-formatted")
  }
  
  parsed <- jsonlite::fromJSON(
    httr::content(r, "text"),
    simplifyVector = FALSE
  )
  
  if (length(parsed) == 0) {
    return(NULL)
  }
  
  return(parsed)
}

geckor_v3 <- function (coin_id, vs_currency = "usd", days, interval = NULL, 
                       max_attempts = 3) 
{
  if (length(coin_id) > 1L) {
    rlang::abort("Only one `coin_id` is allowed")
  }
  if (length(vs_currency) > 1L) {
    rlang::abort("Only one `vs_currency` is allowed")
  }
  if (length(days) > 1L) {
    rlang::abort("Only one `days` value is allowed")
  }
  # validate_arguments(arg_coin_ids = coin_id, arg_vs_currencies = vs_currency, 
  #     arg_max_attempts = max_attempts)
  if (is.na(days) | is.na(suppressWarnings(as.numeric(days))) && 
      days != "max") {
    rlang::abort("`days` only accepts coercible-to-numeric values or a character value \"max\"")
  }
  if (!is.null(interval) && interval != "daily") {
    rlang::abort("`interval` must be equal to NULL or \"daily\"")
  }
  query_params <- list(vs_currency = vs_currency, days = days, 
                       interval = interval)
  url <- build_get_request(base_url = "https://api.coingecko.com/api/v3", 
                           path = c("api", "v3", "coins", coin_id, "market_chart"), 
                           query_parameters = query_params)
  r <- api_request(url = url, max_attempts = max_attempts)
  if (length(r$prices) == 0) {
    message("No data found. Check if the query parameters are specified correctly")
    return(NULL)
  }
  prices <- lapply(r$prices, function(x) {
    if (is.null(x[[1]])) {
      x[[1]] <- NA
    }
    if (is.null(x[[2]])) {
      x[[2]] <- NA
    }
    tibble::tibble(timestamp = as.POSIXct(x[[1]]/1000, origin = as.Date("1970-01-01"), 
                                          tz = "UTC", format = "%Y-%m-%d %H:%M:%S"), coin_id = coin_id, 
                   vs_currency = vs_currency, price = x[[2]])
  }) %>% dplyr::bind_rows()
  market_caps <- lapply(r$market_caps, function(x) {
    if (is.null(x[[1]])) {
      x[[1]] <- NA
    }
    if (is.null(x[[2]])) {
      x[[2]] <- NA
    }
    tibble::tibble(timestamp = as.POSIXct(x[[1]]/1000, origin = as.Date("1970-01-01"), 
                                          tz = "UTC", format = "%Y-%m-%d %H:%M:%S"), market_cap = x[[2]])
  }) %>% dplyr::bind_rows()
  total_volumes <- lapply(r$total_volumes, function(x) {
    if (is.null(x[[1]])) {
      x[[1]] <- NA
    }
    if (is.null(x[[2]])) {
      x[[2]] <- NA
    }
    tibble::tibble(timestamp = as.POSIXct(x[[1]]/1000, origin = as.Date("1970-01-01"), 
                                          tz = "UTC", format = "%Y-%m-%d %H:%M:%S"), total_volume = x[[2]])
  }) %>% dplyr::bind_rows()
  result <- dplyr::full_join(prices, total_volumes, by = "timestamp")
  result <- dplyr::full_join(result, market_caps, by = "timestamp") %>% 
    dplyr::arrange(dplyr::desc(.data$timestamp))
  return(result)
}

format.money  <- function(x, ...) {
  paste0("$", formatC(as.numeric(x), format="f", digits=2, big.mark=","))
}

readdat <- function(x){
  fread(x) %>%
    mutate(datetime = as.POSIXct(unlist(strsplit(x, "tmp_|.csv"))[2])) %>%
    select(-V1)
}