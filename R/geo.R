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
#'   e_geo3d(height, color)
#'   
#' flights <- read.csv(
#'   paste0("https://raw.githubusercontent.com/plotly/datasets/",
#'          "master/2011_february_aa_flight_paths.csv")
#' )
#' 
#' flights %>% 
#'   e_charts() %>% 
#'   e_geo3d_lines(
#'     start_lon, 
#'     start_lat, 
#'     end_lon, 
#'     end_lat,
#'     name = "flights",
#'   ) 
#' 
#' @rdname geo3d
#' @export
e_geo3d <- function(e, serie, color, type = "world", ...){
  if(missing(e))
    stop("must pass e", call. = FALSE)
  
  if(missing(serie) || missing(color))
    stop("must pass serie and color", call. = FALSE)
  
  e$x$opts$xAxis <- NULL # remove
  e$x$opts$yAxis <- NULL # remove
  
  # build JSON data
  data <- .build_height(e, deparse(substitute(serie)), deparse(substitute(color)))
  
  serie <- list(
    map = type,
    regions = data,
    ...
  )
  
  e$x$opts$geo3D <- serie
  
  e
}

#' @rdname geo3d
#' @export
e_geo3d_lines <- function(e, source.lon, source.lat, target.lon, target.lat, type = "world", name = NULL, ...){
  if(missing(e))
    stop("must pass e", call. = FALSE)
  
  if(missing(source.lat) || missing(source.lon) || missing(target.lat) || missing(target.lon))
    stop("missing coordinates", call. = FALSE)
  
  if(is.null(name)) # defaults to column name
    name <- deparse(substitute(serie))
  
  e$x$opts$xAxis <- NULL # remove
  e$x$opts$yAxis <- NULL # remove
  
  # build JSON data
  data <- .map_lines(
    e, 
    deparse(substitute(source.lon)), 
    deparse(substitute(source.lat)), 
    deparse(substitute(target.lon)), 
    deparse(substitute(target.lat))
  )
  
  if(!length(e$x$opts$geo))
    e$x$opts$geo3D <- list(map = type)
  
  serie <- list(
    name = name,
    type = "lines3D",
    coordinateSystem = "geo3D",
    data = data,
    ...
  )
  
  e$x$opts$series <- append(e$x$opts$series, list(serie))
  
  e
}