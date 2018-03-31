#' Choropleth
#' 
#' Draw maps.
#' 
#' @inheritParams e_bar
#' @param serie Values to plot.
#' @param map Map type.
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
#'   e_map(values) %>% 
#'   e_visual_map(min = 10, max = 25)
#' 
#' choropleth %>% 
#'   e_charts(countries) %>% 
#'   e_map_3d(values, shading = "lambert") %>% 
#'   e_visual_map(min = 10, max = 30)
#' }
#' 
#' @seealso \code{\link{e_country_names}}
#' 
#' @rdname map
#' @export
e_map <- function(e, serie, map = "world", name = NULL, ...){
  
  if(missing(e))
    stop("must pass e", call. = FALSE)
  
  if(missing(serie))
    stop("must pass serie", call. = FALSE)
  
  if(is.null(name)) # defaults to column name
    name <- deparse(substitute(serie))
  
  e$x$opts$xAxis <- NULL # remove
  e$x$opts$yAxis <- NULL # remove
  
  app <- list(
    name = name,
    type = "map",
    map = map,
    ...
  )
  
  if(!missing(serie))
    app$data <- .build_data(e, e$x$mapping$x, deparse(substitute(serie)), names = c("name", "value"))
  
  e$x$opts$series <- append(e$x$opts$series, list(app))
  
  e
}

#' @rdname map
#' @export
e_map_3d <- function(e, serie, map = "world", name = NULL, ...){
  if(missing(e))
    stop("must pass e", call. = FALSE)
  
  if(missing(serie))
    stop("must pass serie", call. = FALSE)
  
  if(is.null(name)) # defaults to column name
    name <- deparse(substitute(serie))
  
  e$x$opts$xAxis <- NULL # remove
  e$x$opts$yAxis <- NULL # remove
  
  # build JSON data
  data <- .build_data(e, e$x$mapping$x, deparse(substitute(serie)), names = c("name", "value"))
  
  app <- list(
    name = name,
    type = "map3D",
    map = map,
    ...
  )
  
  if(!missing(app))
    app$data <- .build_data(e, e$x$mapping$x, deparse(substitute(serie)), names = c("name", "value"))
  
  e$x$opts$series <- append(e$x$opts$series, list(app))
  
  e
}

#' Register map
#' 
#' Register a \href{geojson}{http://geojson.org/} map.
#' 
#' @param e An \code{echarts4r} object as returned by \code{\link{e_charts}}.
#' @param name Name of map, to use in \code{\link{e_map}}
#' @param json \href{Geojson}{http://geojson.org/}.
#' 
#' @examples 
#' \dontrun{
#' json <- jsonlite::read_json("http://www.echartsjs.com/gallery/data/asset/geo/USA.json")
#'
#' USArrests %>%
#'   dplyr::mutate(states = row.names(.)) %>%
#'   e_charts(states) %>%
#'   e_map_register("USA", json) %>%
#'   e_map(Murder, map = "USA") %>% 
#'   e_visual_map(min = 0, max = 18)
#' }
#' 
#' @export
e_map_register <- function(e, name, json){
  
  e$x$registerMap <- TRUE
  e$x$mapName <- name
  e$x$geoJSON <- json
  e
}
