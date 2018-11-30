#' Color
#' 
#' Customise chart and background colors.
#' 
#' @inheritParams e_bar
#' @param color Vector of colors.
#' @param background Background color.
#' 
#' @examples 
#' mtcars %>% 
#'   e_charts(drat) %>% 
#'   e_line(mpg) %>% 
#'   e_area(qsec) %>% 
#'   e_color(
#'     c("red", "blue"),
#'     "#d3d3d3"
#'   )
#'   
#' @seealso \code{\link{e_theme}}, 
#' \href{Official color documentation}{https://ecomfe.github.io/echarts-doc/public/en/option.html#color}, 
#' \href{Official background documentation}{https://ecomfe.github.io/echarts-doc/public/en/option.html#backgroundColor}
#' 
#' @export
e_color <- function(e, color = NULL, background = NULL){
  
  if(!e$x$tl){
    if(!is.null(color)) e$x$opts$color <- color
    if(!is.null(background)) e$x$opts$backgroundColor <- background
  } else {
    if(!is.null(color)) e$x$opts$baseOption$color <- color
    if(!is.null(background)) e$x$opts$baseOption$backgroundColor <- background
  }
  
  e
}