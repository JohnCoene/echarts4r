#' Geo 3D
#' 
#' Create a 3d geo plot.
#' 
#' @inheritParams e_bar
#' @param color Color.
#' @param type Map type.
#' 
#' @examples 
#' choropleth <- data.frame(
#'   countries = c("France", "Brazil", "China", "Russia", "Canada", "India", "United States",
#'                 "Argentina", "Australia"),
#'   height = runif(9, 1, 5),
#'   color = c("#F7FBFF", "#DEEBF7", "#C6DBEF", "#9ECAE1", "#6BAED6", "#4292C6", 
#'             "#2171B5", "#08519C", "#08306B")
#' )
#' 
#' choropleth %>% 
#'   e_charts(countries) %>% 
#'   e_geo_3d(height, color)
#' 
#' @rdname geo3d
#' @export
e_geo_3d <- function(e, serie, color, type = "world", ...){
  if(missing(e))
    stop("must pass e", call. = FALSE)
  
  e$x$opts$xAxis <- NULL # remove
  e$x$opts$yAxis <- NULL # remove
  
  series <- list(
    map = type,
    ...
  )
  
  if(!missing(serie) && !missing(color))
    series$regions <- .build_height(e, deparse(substitute(serie)), deparse(substitute(color)))
  
  e$x$opts$geo3D <- series
  
  e
}
