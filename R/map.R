#' Choropleth
#' 
#' Draw maps.
#' 
#' @inheritParams e_bar
#' @param serie Values to plot.
#' @param map Map type.
#' @param coord_system Coordinate system to use, one of \code{cartesian3D}, \code{geo3D}, \code{globe}.
#' @param rm_x,rm_y Whether to remove x and y axis, defaults to \code{TRUE}.
#' @param id,value,height Columns corresponding to registered map.
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
#'         
#' # custom
#' buildings <- jsonlite::read_json(
#'   paste0(
#'     "https://ecomfe.github.io/echarts-examples/",
#'     "public/data-gl/asset/data/buildings.json"
#'   )
#' )
#' 
#' heights <- purrr::map(buildings$features, "properties") %>% 
#'   purrr::map("height") %>% 
#'   unlist()
#'   
#' names <- purrr::map(buildings$features, "properties") %>% 
#'   purrr::map("name") %>% 
#'   unlist()
#'   
#' data <- dplyr::tibble(
#'   name = names,
#'   value = round(runif(length(names), 0, 1), 6),
#'   height = heights / 10
#' )
#' 
#' data %>% 
#'   e_charts() %>% 
#'   e_map_register("buildings", buildings) %>%
#'   e_map_3d_custom(name, value, height) %>% 
#'   e_visual_map(
#'     show = FALSE,
#'     min = 0.4,
#'     max = 1
#'   ) 
#'   
#' # timeline
#' choropleth <- data.frame(
#'     countries = rep(choropleth$countries, 3)
#'   ) %>% 
#'   dplyr::mutate(
#'     grp = c(
#'       rep(2016, nrow(choropleth)),
#'       rep(2017, nrow(choropleth)),
#'       rep(2018, nrow(choropleth))
#'     ),
#'     values = runif(27, 1, 10)
#'   )
#'   
#' choropleth %>% 
#'   group_by(grp) %>% 
#'   e_charts(countries, timeline = TRUE) %>% 
#'   e_map(values) %>% 
#'   e_visual_map(min = 1, max = 10)
#'   
#' choropleth %>% 
#'   group_by(grp) %>% 
#'   e_charts(countries, timeline = TRUE) %>% 
#'   e_map_3d(values) %>% 
#'   e_visual_map(min = 1, max = 10)
#' }
#' 
#' @seealso \code{\link{e_country_names}}, 
#' \href{https://echarts.apache.org/en/option.html#series-map}{Additional map arguments}, 
#' \href{http://echarts.baidu.com/option-gl.html#series-map3D}{Additional map 3D arguments}
#' 
#' @rdname map
#' @export
e_map <- function(e, serie, map = "world", name = NULL, rm_x = TRUE, rm_y = TRUE, ...){
  
  if(missing(e))
    stop("must pass e", call. = FALSE)
  
  if(!missing(serie))
    sr <- deparse(substitute(serie))
  else
    sr <- NULL
  
  e_map_(e, sr, map, name, rm_x, rm_y, ...)
}

#' @rdname map
#' @export
e_map_ <- function(e, serie = NULL, map = "world", name = NULL, rm_x = TRUE, rm_y = TRUE, ...){
  
  if(missing(e))
    stop("must pass e", call. = FALSE)
  
  e <- .rm_axis(e, rm_x, "x")
  e <- .rm_axis(e, rm_y, "y")
  
  for(i in 1:length(e$x$data)){
    
    app <- list(
      type = "map",
      map = map,
      name = name,
      ...
    )
    
    if(!is.null(serie)){
      data <- .build_data2(e$x$data[[i]], serie)
      data <- .add_bind2(e, data, e$x$mapping$x, i = i)
      app_data <- list(data = data)
    } else {
      app_data <- list()
    }
    
    if(!e$x$tl){
      
      if(is.null(name) && !is.null(serie))
        app$name <- serie
      
      app <- append(app, app_data)
      
      e$x$opts$series <- append(e$x$opts$series, list(app))
    } else
      e$x$opts$options[[i]]$series <- append(e$x$opts$options[[i]]$series, list(app_data))
    
  }
  
  if(isTRUE(e$x$tl))
    e$x$opts$baseOption$series <- append(e$x$opts$baseOption$series, list(app))


  if(map == "world"){
    # add dependency
    path <- system.file("htmlwidgets/lib/echarts-4.8.0", package = "echarts4r")
    dep <- htmltools::htmlDependency(
      name = "echarts-world",
      version = "1.0.0",
      src = c(file = path),
      script = "world.js"
    )

    e$dependencies <- append(e$dependencies, list(dep))
  }
  
  e
}

#' @rdname map
#' @export
e_map_3d <- function(e, serie, map = "world", name = NULL, coord_system = NULL, rm_x = TRUE, rm_y = TRUE, ...){
  if(missing(e))
    stop("must pass e", call. = FALSE)
  
  if(!missing(serie))
    sr <- deparse(substitute(serie))
  else
    sr <- NULL
  
  e_map_3d_(e = e, serie = sr, map, name, coord_system, rm_x, rm_y, ...)
}

#' @rdname map
#' @export
e_map_3d_ <- function(e, serie = NULL, map = "world", name = NULL, coord_system = NULL, rm_x = TRUE, rm_y = TRUE, ...){
  
  if(missing(e))
    stop("must pass e", call. = FALSE)
  
  e <- .rm_axis(e, rm_x, "x")
  e <- .rm_axis(e, rm_y, "y")
  
  for(i in 1:length(e$x$data)){
    
    if(!is.null(serie)){
      data <- .build_data2(e$x$data[[i]], serie)
      data <- .add_bind2(e, data, e$x$mapping$x, i = i)
      dat <- data
    }
    
    app <- list(
      type = "map3D",
      map = map,
      coordinateSystem = coord_system,
      name = name,
      ...
    )
    
    app_data <- list(data = data)
    
    if(!e$x$tl){
      
      if(is.null(name) && !is.null(serie))
        app$name <- serie
      
      app <- append(app, app_data)
      
      e$x$opts$series <- append(e$x$opts$series, list(app))
      
    } else {
      e$x$opts$options[[i]]$series <- append(e$x$opts$options[[i]]$series, list(app_data))
    }
    
  }
  
  if(isTRUE(e$x$tl))
    e$x$opts$baseOption$series <- append(e$x$opts$baseOption$series, list(app))

  if(map == "world"){
    # add dependency
    path <- system.file("htmlwidgets/lib/echarts-4.8.0", package = "echarts4r")
    dep <- htmltools::htmlDependency(
      name = "echarts-world",
      version = "1.0.0",
      src = c(file = path),
      script = "world.js"
    )

    e$dependencies <- append(e$dependencies, list(dep))
  }

  # add dependency
  path <- system.file("htmlwidgets/lib/echarts-4.8.0", package = "echarts4r")
  dep <- htmltools::htmlDependency(
    name = "echarts-gl",
    version = "1.1.2",
    src = c(file = path),
    script = "echarts-gl.min.js"
  )

  e$dependencies <- append(e$dependencies, list(dep)) 
  
  e
}

#' @rdname map
#' @export
e_map_3d_custom <- function(e, id, value, height, map = NULL, name = NULL, rm_x = TRUE, rm_y = TRUE, ...){
  
  if(missing(e))
    stop("must pass e", call. = FALSE)
  
  if(missing(id) || missing(value) || missing(height))
    stop("must pass id, value, and height", call. = FALSE)
  
  if(is.null(map) && length(e$x$registerMap[[1]]$mapName))
    map <- unlist(e$x$registerMap[[1]]$mapName)
  else
    stop("not map registered, see e_map_register", call. = FALSE)
  
  e$x$renderer <- "webgl"
  
  e <- .rm_axis(e, rm_x, "x")
  e <- .rm_axis(e, rm_y, "y")
  
  app <- list(
    type = "map3D",
    map = map,
    ...
  )
  
  if(!is.null(name))
    app$name <- name
  
  name_quo <- dplyr::enquo(id)
  value_quo <- dplyr::enquo(value)
  height_quo <- dplyr::enquo(height)
  
  data <- e$x$data[[1]] %>% 
    dplyr::select(
      name = !!name_quo,
      value = !!value_quo,
      height = !!height_quo
    ) %>% 
    apply(1, as.list)
  
  app$data <- data
  
  e$x$opts$series <- append(e$x$opts$series, list(app))
  
  e
}

#' Register map
#' 
#' Register a \href{http://geojson.org/}{geojson} map.
#' 
#' @param e An \code{echarts4r} object as returned by \code{\link{e_charts}}.
#' @param name Name of map, to use in \code{\link{e_map}}.
#' @param json \href{http://geojson.org/}{Geojson}.
#' @param async Whether to read the file asynchronously.
#' @param session A valid Shiny session.
#' 
#' @details \code{e_map_register_p} is not truly a proxy as it does not require
#' a chart to function. While the function \code{e_map_register_ui} is meant to
#' register the map globally in the Shiny UI, not that then \code{json} must be accessible
#' from the UI (generally www folder).
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
#'   e_visual_map(Murder)
#' }
#' 
#' @rdname e_map_register
#' @export
e_map_register <- function(e, name, json) UseMethod("e_map_register")

#' @export
#' @method e_map_register echarts4r
e_map_register.echarts4r <- function(e, name, json){
  
  if(!length(e$x$registerMap))
    e$x$registerMap <- list()
  
  opts <- list(
    mapName = name,
    geoJSON = json
  )
  
  e$x$registerMap <- append(e$x$registerMap, list(opts))
  e
}

#' @rdname e_map_register
#' @export
e_map_register_p <- function(name, json, async = FALSE, session = shiny::getDefaultReactiveDomain()){
  
  opts <- list(
    mapAsync = async,
    mapName = name,
    geoJSON = json
  )
  
  session$sendCustomMessage("e_register_map", opts)
  invisible()
}

#' @rdname e_map_register
#' @export
e_map_register_ui <- function(name, json, async = FALSE){
  async <- paste0(tolower(async))
  script <- paste0("
    $.ajax({ 
        url: '", json, "', 
        dataType: 'json', 
        async: ", async, ",
        success: function(map){ 
          echarts.registerMap('", name, "', map);
        } 
      });"
    )
  shiny::tags$script(
    script
  )
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
#'   e_bar_3d(lat, value, coord_system = "mapbox") %>% 
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
  
  e$x$mapboxToken <- token
  
  if(!e$x$tl)
    e$x$opts$mapbox <- list(...)
  else
    e$x$opts$baseOption$mapbox <- list(...)

  # add dependency
  path <- system.file("htmlwidgets/lib/mapbox", package = "echarts4r")
  dep <- htmltools::htmlDependency(
    name = "echarts-mapbox",
    version = "0.38.0",
    src = c(file = path),
    script = "mapbox-gl.js",
    stylesheet = "mapbox-gl.css"
  )

  e$dependencies <- append(e$dependencies, list(dep))
  
  e
}
