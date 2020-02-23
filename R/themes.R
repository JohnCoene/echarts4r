#' Themes
#' 
#' Add a custom theme or apply a pre-built one.
#' 
#' @inheritParams e_bar
#' @param theme Theme, see below.
#' @param name Name of theme.
#' 
#' @section Functions:
#' \itemize{
#'  \item{\code{e_theme} - Use a default theme by name.}
#'  \item{\code{e_theme_custom} - Use a custom theme.}
#'  \item{\code{e_register_theme} - Register a theme globally in shiny or R markdown.}
#' }
#' 
#' @details The function \code{e_register_theme} can be used to register the theme globally
#' in R markdown or shiny. This is useful because 1) the \code{e_theme_custom} registers the 
#' theme every time and is more computationally expensive.
#' 
#' @section Themes: 
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
#' 
#' @seealso \href{http://echarts.baidu.com/theme-builder/}{create your own theme}.
#' 
#' @rdname theme
#' @export
e_theme <- function(e, name){
  
  if(missing(name))
    stop("must pass name", call. = FALSE)
  
  e$x$theme <- name
  e
}

#' @rdname theme
#' @export
e_theme_custom <- function(e, theme, name = "custom"){
  if(missing(theme))
    stop("must pass `theme`", call. = FALSE)

  e$x$theme2 <- TRUE
  e$x$customTheme <- theme
  e$x$theme <- "custom"
  e$x$theme_name <- name
  e
}

#' @rdname theme
#' @export
e_register_theme <- function(theme, name = "custom"){
  if(missing(theme))
    stop("must pass `theme`", call. = FALSE)

  shiny::singleton(
    shiny::tags$head(
      shiny::tags$script(
        paste0("window.echarts.registerTheme('", name, "', ", theme,");")
      )
    )
  )
}