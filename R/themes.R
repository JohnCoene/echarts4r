#' Themes
#' 
#' Add a theme.
#' 
#' @inheritParams e_bar
#' @param theme Theme, see below.
#' 
#' @section Themes: 
#' \itemize{
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
  
  themes <- c("dark", "vintage", "westeros", "essos", "wonderland", 
              "walden", "chalk", "infographic", "macarons", "roma",
              "shine", "purple-passion", "halloween", "wef", "weforum")
  
  if(!tolower(theme) %in% themes)
    stop("Incorrect theme", call. = FALSE)
  
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