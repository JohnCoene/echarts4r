#' Themes
#' 
#' Add a custom theme or apply a pre-built one.
#' 
#' @inheritParams e_bar
#' @param theme Theme, see below.
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
e_theme <- function(e, theme){
  
  if(missing(theme))
    stop("must pass theme", call. = FALSE)
  
  e$x$theme <- tolower(theme)
  e
}

#' @rdname theme
#' @export
e_theme_custom <- function(e, theme){
  e$x$theme2 <- TRUE
  e$x$customTheme <- theme
  e$x$theme <- "custom"
  e
}