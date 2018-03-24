#' Choropleth
#' 
#' Draw maps.
#' 
#' @inheritParams e_bar
#' @param serie Values to plot.
#' @param type Map type.
#' @param source.lon,source.lat,target.lat,target.lon Coordinates.
#' 
#' @examples 
#' \dontrun{
#' choropleth <- data.frame(
#'   countries = c("France", "Brazil", "China", "Russia", "Canada", "India", "United States",
#'                 "Argentina", "Australia"),
#'   values = round(runif(9, 10, 25))
#' )
#' 
#' choropleth %>% 
#'   e_charts(countries) %>% 
#'   e_map_3d(values, shading = "lambert") %>% 
#'   e_visual_map(min = 10, max = 30)
#'   
#' choropleth %>% 
#'   e_charts(countries) %>% 
#'   e_choropleth(values) %>% 
#'   e_visual_map(min = 10, max = 25)
#'   
#' flights <- read.csv(
#'   paste0("https://raw.githubusercontent.com/plotly/datasets/",
#'          "master/2011_february_aa_flight_paths.csv")
#' )
#' 
#' flights %>% 
#'   e_charts() %>% 
#'   e_map_lines(
#'     start_lon, 
#'     start_lat, 
#'     end_lon, 
#'     end_lat,
#'     name = "flights",
#'     lineStyle = list(normal = list(curveness = 0.3))
#'    )
#' }
#' 
#' @rdname map
#' @export
e_choropleth <- function(e, serie, type = "world", name = NULL, ...){
  
  if(missing(e))
    stop("must pass e", call. = FALSE)
  
  if(missing(serie))
    stop("must pass serie", call. = FALSE)
  
  if(is.null(name)) # defaults to column name
    name <- deparse(substitute(serie))
  
  e$x$opts$xAxis <- NULL # remove
  e$x$opts$yAxis <- NULL # remove
  
  # build JSON data
  data <- .build_pie(e, deparse(substitute(serie)))
  
  serie <- list(
    name = name,
    type = "map",
    mapType = type,
    data = data,
    ...
  )
  
  e$x$opts$series <- append(e$x$opts$series, list(serie))
  
  e
}

#' @rdname map
#' @export
e_map_3d <- function(e, serie, type = "world", name = NULL, ...){
  if(missing(e))
    stop("must pass e", call. = FALSE)
  
  if(missing(serie))
    stop("must pass serie", call. = FALSE)
  
  if(is.null(name)) # defaults to column name
    name <- deparse(substitute(serie))
  
  e$x$opts$xAxis <- NULL # remove
  e$x$opts$yAxis <- NULL # remove
  
  # build JSON data
  data <- .build_pie(e, deparse(substitute(serie)))
  
  serie <- list(
    name = name,
    type = "map3D",
    map = type,
    data = data,
    ...
  )
  
  e$x$opts$series <- append(e$x$opts$series, list(serie))
  
  e
}

#' @rdname map
#' @export
e_map_lines <- function(e, source.lon, source.lat, target.lon, target.lat, type = "world", name = NULL, ...){
  if(missing(e))
    stop("must pass e", call. = FALSE)
  
  if(missing(source.lat) || missing(source.lon) || missing(target.lat) || missing(target.lon))
    stop("missing coordinates", call. = FALSE)
  
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
    e$x$opts$geo <- list(map = type)
  
  serie <- list(
    name = name,
    type = "lines",
    coordinateSystem = "geo",
    data = data,
    ...
  )
  
  e$x$opts$series <- append(e$x$opts$series, list(serie))
  
  e
}