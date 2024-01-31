f7Page_mod <- function(
  ...,
  title = NULL,
  preloader = FALSE,
  loading_duration = 3,
  # default options
  options = list(
    theme = c("ios", "md", "auto", "aurora"),
    dark = TRUE,
    filled = FALSE,
    color = "#007aff",
    touch = list(
      tapHold = TRUE,
      tapHoldDelay = 750,
      iosTouchRipple = FALSE
    ),
    iosTranslucentBars = FALSE,
    navbar = list(
      iosCenterTitle = TRUE,
      hideOnPageScroll = FALSE
    ),
    toolbar = list(
      hideOnPageScroll = FALSE
    ),
    pullToRefresh = FALSE
  ),
  allowPWA = FALSE
) {
  
  # fallback to auto
  if (length(options$theme) > 1) options$theme <- "auto"
  
  if (!is.null(options$theme) && !is.null(options$filled) && !is.null(options$color)) {
    if (options$theme == "dark" && options$filled == TRUE &&
        (options$color == "white" || options$color == "#fff")) {
      stop("Wrong theme combination: navbar color cannot be white in a dark theme!")
    }
  }
  
  if (!is.null(options$pullToRefresh)) {
    dataPTR <- tolower(options$pullToRefresh)
    options$pullToRefresh <- NULL
  } else {
    dataPTR <- NULL
  }
  
  # configuration tag to be passed to JS
  configTag <- shiny::tags$script(
    type = "application/json",
    `data-for` = "app",
    jsonlite::toJSON(
      x = options,
      auto_unbox = TRUE,
      json_verbatim = TRUE
    )
  )
  
  bodyTag <- shiny::tags$body(
    `data-pwa` = tolower(allowPWA),
    `data-ptr`= dataPTR,
    # preloader
    onLoad = if (preloader) {
      duration <- loading_duration * 1000
      paste0(
        "$(function() {
          // Preloader
          app.dialog.preloader();
          setTimeout(function () {
           app.dialog.close();
           }, ", duration, ");
        });
        "
      )
    },
    shiny::tags$div(
      id = "app",
      ...
    ),
    configTag
  )
  
  pwaDeps <- if (allowPWA) {
    c("pwa", "pwacompat")
  } else {
    NULL
  }
  
  shiny::tagList(
    # Head
    shiny::tags$head(
      shiny::tags$meta(charset = "utf-8"),
      shiny::tags$meta(
        name = "viewport",
        content = "
          width=device-width,
          initial-scale=1,
          maximum-scale=1,
          minimum-scale=1,
          user-scalable=no,
          viewport-fit=cover"
      ),
      shiny::tags$title(title)
    ),
    # Body
    add_dependencies(
      deps = c(
        "framework7",
        "shinyMobile",
        pwaDeps
      ),
      bodyTag
    )
  )
}


f7Navbar_mod <- function(..., subNavbar = NULL, title = NULL, subtitle = NULL, hairline = TRUE,
                         shadow = TRUE,  bigger = FALSE, transparent = FALSE, leftPanel = FALSE,
                         rightPanel = FALSE) {
  
  navbarClass <- "navbar"
  # bigger and transparent work together
  if (bigger) {
    if (transparent) {
      navbarClass <- paste0(navbarClass, " navbar-large navbar-large-transparent")
    } else {
      navbarClass <- paste0(navbarClass, " navbar-large")
    }
  }
  if (!hairline) navbarClass <- paste0(navbarClass, " no-hairline")
  if (!shadow) navbarClass <- paste0(navbarClass, " no-shadow")
  
  leftNav <- if (leftPanel) {
    shiny::tags$div(
      class = "left",
      shiny::tags$a(
        class = "link icon-only panel-open",
        `data-panel` = "left",
        # shiny::tags$i(class = "f7-icons ios-only", "bars"),
        # shiny::tags$i(class = "icon material-icons md-only", "menu")
        f7Icon("bars")
      )
    )
  }
  
  rightNav <- if (rightPanel) {
    shiny::tags$div(
      class = "right",
      shiny::tags$a(
        class = "link icon-only panel-open",
        `data-panel` = "right",
        # shiny::tags$i(class = "f7-icons ios-only", "bars"),
        # shiny::tags$i(class = "icon material-icons md-only", "menu")
        f7Icon("gear_alt")
      )
    )
  }
  
  innerCl <- "navbar-inner sliding"
  if (bigger) innerCl <- paste0(innerCl, " navbar-inner-large")
  
  shiny::tags$div(
    class = navbarClass,
    shiny::tags$div(class = "navbar-bg"),
    shiny::tags$div(
      class = innerCl,
      leftNav,
      if (bigger) {
        shiny::tagList(
          shiny::tags$div(
            class = "title",
            title,
            # add style to prevent title from
            # being black. Bug in Framework7?
            style = "color: white;"
          ),
          rightNav,
          shiny::tags$div(
            class = "title-large",
            shiny::tags$div(class = "title-large-text", title)
          )
        )
      } else {
        shiny::tagList(
          shiny::tags$div(
            class = "title",
            title,
            if (!is.null(subtitle)) shiny::tags$span(class = "subtitle", subtitle)
          ),
          rightNav
        )
      },
      ...,
      subNavbar
    )
  )
}
