#' Themes
#'
#' Add a custom theme or apply a pre-built one.
#'
#' @inheritParams e_bar
#' @param theme Theme, A json string or a see below.
#' @param name Name of theme.
#'
#' @section Functions:
#' \itemize{
#'  \item{\code{e_theme} - Use a default theme by name.}
#'  \item{\code{e_theme_custom} - Use a custom theme.}
#'  \item{\code{e_theme_register} - Register a theme globally in shiny or R markdown.}
#' }
#'
#' @details The function \code{e_theme_register} can be used to register the theme globally
#' in R markdown or shiny (UI). This is useful because 1) the \code{e_theme_custom} registers the
#' theme every time and is more computationally expensive.
#'
#' @section Theme names:
#' \itemize{
#'   \item{\code{default}}
#'   \item{\code{dark}}
#'   \item{\code{vintage}}
#'   \item{\code{westeros}}
#'   \item{\code{essos}}
#'   \item{\code{wonderland}}
#'   \item{\code{walden}}
#'   \item{\code{chalk}}
#'   \item{\code{infographic}}
#'   \item{\code{macarons}}
#'   \item{\code{roma}}
#'   \item{\code{shine}}
#'   \item{\code{purple-passion}}
#'   \item{\code{halloween}}
#'   \item{\code{auritus}}
#'   \item{\code{azul}}
#'   \item{\code{bee-insipired}}
#'   \item{\code{blue}}
#'   \item{\code{caravan}}
#'   \item{\code{carp}}
#'   \item{\code{cool}}
#'   \item{\code{dark-blue}}
#'   \item{\code{dark-bold}}
#'   \item{\code{dark-digerati}}
#'   \item{\code{dark-fresh-cut}}
#'   \item{\code{dark-mushroom}}
#'   \item{\code{eduardo}}
#'   \item{\code{forest}}
#'   \item{\code{fresh-cut}}
#'   \item{\code{fruit}}
#'   \item{\code{gray}}
#'   \item{\code{green}}
#'   \item{\code{helianthus}}
#'   \item{\code{inspired}}
#'   \item{\code{jazz}}
#'   \item{\code{london}}
#'   \item{\code{macarons}}
#'   \item{\code{macarons2}}
#'   \item{\code{mint}}
#'   \item{\code{red}}
#'   \item{\code{red-velvet}}
#'   \item{\code{royal}}
#'   \item{\code{sakura}}
#'   \item{\code{tech-blue}}
#' }
#'
#' @examples
#' mtcars %>%
#'   e_charts(mpg) %>%
#'   e_line(disp) %>%
#'   e_area(hp) %>%
#'   e_x_axis(min = 10) -> p
#'
#' p %>% e_theme("chalk")
#' p %>% e_theme_custom('{"color":["#ff715e","#ffaf51"]}')
#' @seealso \href{https://echarts.apache.org/en/theme-builder.html}{create your own theme}.
#'
#' @rdname theme
#' @export
e_theme <- function(e, name) {
  if (missing(name)) {
    stop("must pass name", call. = FALSE)
  }

  # add name to register
  e$x$theme <- name

  # add dependency
  path <- system.file("htmlwidgets/lib/echarts-4.8.0/themes", package = "echarts4r")
  dep <- htmltools::htmlDependency(
    name = name,
    version = "1.0.0",
    src = c(file = path),
    script = paste0(name, ".js")
  )

  e$dependencies <- append(e$dependencies, list(dep))
  e
}

#' @rdname theme
#' @export
e_theme_custom <- function(e, theme, name = "custom") {
  if (missing(theme)) {
    stop("must pass `theme`", call. = FALSE)
  }

  if (grepl("\\.json", theme)) {
    theme <- readLines(theme) # read the file
    theme <- paste0(theme, collapse = "") # collapse to a string
  }

  e$x$theme2 <- TRUE
  e$x$customTheme <- theme
  e$x$theme <- "custom"
  e$x$theme_name <- name
  e
}

#' @rdname theme
#' @export
e_theme_register <- function(theme, name = "custom") {
  if (missing(theme)) {
    stop("must pass `theme`", call. = FALSE)
  }

  if (grepl("\\.json", theme)) {
    theme <- readLines(theme) # read the file
    theme <- paste0(theme, collapse = "") # collapse to a string
  }

  shiny::singleton(
    shiny::tags$head(
      shiny::tags$script(
        paste0("window.echarts.registerTheme('", name, "', ", theme, ");")
      )
    )
  )
}
