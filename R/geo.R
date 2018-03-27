#' Geo 3D
#' 
#' Initialise geo 3D.
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
#' @seealso \code{\link{e_country_names}}
#' 
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
  
  if(!missing(color) && !missing(serie))
    series$regions <- .build_height(e, deparse(substitute(serie)), deparse(substitute(color)))
  else if(missing(color) && !missing(serie))
    series$regions <- .build_height(e, deparse(substitute(serie)))
  
  e$x$opts$geo3D <- series
  
  e
}

#' Geo
#' 
#' Initialise geo.
#' 
#' @inheritParams e_bar
#' @param map Map type.
#' 
#' @examples 
#' flights <- read.csv(
#'   paste0("https://raw.githubusercontent.com/plotly/datasets/",
#'          "master/2011_february_aa_flight_paths.csv")
#' )
#' 
#' flights %>% 
#'   e_charts() %>% 
#'   e_geo() %>% 
#'   e_lines(
#'     start_lon, 
#'     start_lat, 
#'     end_lon, 
#'     end_lat,
#'     name = "flights",
#'     lineStyle = list(normal = list(curveness = 0.3))
#'    )
#' 
#' @export
e_geo <- function(e, map = "world", ...){
  
  opts <- list(
    map = map,
    ...
  )
  
  e$x$opts$geo <- opts
  e
}