#' Choropleth
#' 
#' Draw maps.
#' 
#' @inheritParams e_bar
#' @param serie Values to plot.
#' @param map Map type.
#' @param coord.system Coordinate system to use, one of \code{cartesian3D}, \code{geo3D}, \code{globe}.
#' @param rm.x,rm.y Whether to remove x and y axis, defaults to \code{TRUE}.
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
#' @seealso \code{\link{e_country_names}}, 
#' \href{Additional map arguments}{https://ecomfe.github.io/echarts-doc/public/en/option.html#series-map}, 
#' \href{Additional map 3D arguments}{http://echarts.baidu.com/option-gl.html#series-map3D}
#' 
#' @rdname map
#' @export
e_map <- function(e, serie, map = "world", name = NULL, rm.x = TRUE, rm.y = TRUE, ...){
  
  if(missing(e))
    stop("must pass e", call. = FALSE)
  
  if(missing(serie))
    stop("must pass serie", call. = FALSE)
  
  e_map_(e, deparse(substitute(serie)), map, name, rm.x, rm.y, ...)
}

#' @rdname map
#' @export
e_map_ <- function(e, serie, map = "world", name = NULL, rm.x = TRUE, rm.y = TRUE, ...){
  
  if(missing(e))
    stop("must pass e", call. = FALSE)
  
  if(missing(serie))
    stop("must pass serie", call. = FALSE)
  
  if(is.null(name)) # defaults to column name
    name <- serie
  
  e <- .rm_axis(e, rm.x, "x")
  e <- .rm_axis(e, rm.y, "y")
  
  app <- list(
    name = name,
    type = "map",
    map = map,
    ...
  )
  
  if(!missing(serie)){
    data <- .build_data(e, serie)
    data <- .add_bind(e, data, e$x$mapping$x)
    app$data <- data
  }
  
  e$x$opts$series <- append(e$x$opts$series, list(app))
  
  e
}

#' @rdname map
#' @export
e_map_3d <- function(e, serie, map = "world", name = NULL, coord.system = NULL, rm.x = TRUE, rm.y = TRUE, ...){
  if(missing(e))
    stop("must pass e", call. = FALSE)
  
  if(missing(serie))
    stop("must pass serie", call. = FALSE)
  
  e_map_3d_(e, deparse(substitute(serie)), map, name, coord.system, rm.x, rm.y, ...)
}

#' @rdname map
#' @export
e_map_3d_ <- function(e, serie, map = "world", name = NULL, coord.system = NULL, rm.x = TRUE, rm.y = TRUE, ...){
  if(missing(e))
    stop("must pass e", call. = FALSE)
  
  if(missing(serie))
    stop("must pass serie", call. = FALSE)
  
  if(is.null(name)) # defaults to column name
    name <- serie
  
  e <- .rm_axis(e, rm.x, "x")
  e <- .rm_axis(e, rm.y, "y")
  
  # build JSON data
  data <- .build_data(e, serie)
  data <- .add_bind(e, data, e$x$mapping$x)
  
  app <- list(
    name = name,
    type = "map3D",
    map = map,
    ...
  )
  
  if(!is.null(coord.system))
    app$coordinateSystem <- coord.system
  
  if(!missing(serie)){
    data <- .build_data(e, serie)
    data <- .add_bind(e, data, e$x$mapping$x)
    app$data <- data
  }
  
  e$x$opts$series <- append(e$x$opts$series, list(app))
  
  e
}

#' Register map
#' 
#' Register a \href{geojson}{http://geojson.org/} map.
#' 
#' @param e An \code{echarts4r} object as returned by \code{\link{e_charts}}.
#' @param name Name of map, to use in \code{\link{e_map}}.
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

#' Mapbox
#' 
#' Use mapbox.
#' 
#' @inheritParams e_bar
#' @param token Your mapbox token from \href{https://www.mapbox.com/}{mapbox}.
#' @param ... Any option.
#' 
#' @examples 
#' \dontrun{
#' url <- paste0("https://ecomfe.github.io/echarts-examples/",
#'               "public/data-gl/asset/data/population.json")
#' data <- jsonlite::fromJSON(url)
#' data <- as.data.frame(data)
#' names(data) <- c("lon", "lat", "value")
#' 
#' data %>% 
#'   e_charts(lon) %>% 
#'   e_mapbox(
#'     token = "YOUR_MAPBOX_TOKEN",
#'     style = "mapbox://styles/mapbox/dark-v9"
#'   ) %>% 
#'   e_bar_3d(lat, value, coord.system = "mapbox") %>% 
#'   e_visual_map()
#' }
#' 
#' @note Mapbox may not work properly in the RSudio console.
#' 
#' @seealso \href{http://www.echartsjs.com/option-gl.html#mapbox3D.style}{Official documentation},
#' \href{https://www.mapbox.com/mapbox-gl-js/api/}{mapbox documentation}
#' 
#' @name mapbox
#' @export
e_mapbox <- function(e, token, ...){
  
  if(missing(token))
    stop("missing token", call. = FALSE)
  
  e$x$opts$mapbox <- list(...)
  e$x$mapboxToken <- token
  e
}
