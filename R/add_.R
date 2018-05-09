#' @rdname e_bar
#' @export
e_bar_ <- function(e, serie, bind = NULL, name = NULL, legend = TRUE, y.index = 0, x.index = 0, ...){
  if(missing(e))
    stop("must pass e", call. = FALSE)
  
  if(missing(serie))
    stop("must pass serie", call. = FALSE)
  
  if(is.null(name)) # defaults to column name
    name <- serie
  
  if(y.index != 0)
    e <- .set_y_axis(e, serie, y.index)
  
  if(x.index != 0)
    e <- .set_x_axis(e, x.index)
  
  # build JSON data
  .build_data(e, e$x$mapping$x, serie) -> vector
  
  if(!is.null(bind))
    vector <- .add_bind(e, vector, bind)
  
  serie <- list(
    name = name,
    type = "bar",
    data = vector,
    yAxisIndex = y.index,
    xAxisIndex = x.index,
    ...
  )
  
  if(isTRUE(legend))
    e$x$opts$legend$data <- append(e$x$opts$legend$data, list(name))
  
  e$x$opts$series <- append(e$x$opts$series, list(serie))
  e
}

#' @rdname e_line
#' @export
e_line_ <- function(e, serie, bind = NULL, name = NULL, legend = TRUE, y.index = 0, x.index = 0, 
                    coord.system = "cartesian2d", ...){
  if(missing(e))
    stop("must pass e", call. = FALSE)
  
  if(missing(serie))
    stop("must pass serie", call. = FALSE)
  
  if(is.null(name)) # defaults to column name
    name <- serie
  
  # build JSON data
  .build_data(e, e$x$mapping$x, serie) -> vector
  
  if(!is.null(bind))
    vector <- .add_bind(e, vector, bind)
  
  l <- list(
    name = name,
    type = "line",
    data = vector,
    coordinateSystem = coord.system,
    ...
  )
  
  if(coord.system == "cartesian2d"){
    if(y.index != 0)
      e <- .set_y_axis(e, serie, y.index)
    
    if(x.index != 0)
      e <- .set_x_axis(e, x.index)
    
    l$yAxisIndex <- y.index
    l$xAxisIndex <- x.index
  }
  
  if(isTRUE(legend))
    e$x$opts$legend$data <- append(e$x$opts$legend$data, list(name))
  
  e$x$opts$series <- append(e$x$opts$series, list(l))
  e
}

#' @rdname e_area
#' @export
e_area_ <- function(e, serie, bind = NULL, name = NULL, legend = TRUE, y.index = 0, x.index = 0, ...){
  if(missing(e))
    stop("must pass e", call. = FALSE)
  
  if(missing(serie))
    stop("must pass serie", call. = FALSE)
  
  if(is.null(name)) # defaults to column name
    name <- serie
  
  if(y.index != 0)
    e <- .set_y_axis(e, serie, y.index)
  
  if(x.index != 0)
    e <- .set_x_axis(e, x.index)
  
  # build JSON data
  .build_data(e, e$x$mapping$x, serie) -> vector
  
  if(!is.null(bind))
    vector <- .add_bind(e, vector, bind)
  
  serie <- list(
    name = name,
    type = "line",
    data = vector,
    yAxisIndex = y.index,
    xAxisIndex = x.index,
    areaStyle = list(normal = list()),
    ...
  )
  
  if(isTRUE(legend))
    e$x$opts$legend$data <- append(e$x$opts$legend$data, list(name))
  
  e$x$opts$series <- append(e$x$opts$series, list(serie))
  e
}

#' @rdname e_step
#' @export
e_step_ <- function(e, serie, bind = NULL, step = c("start", "middle", "end"), fill = FALSE, 
                   name = NULL, legend = TRUE, y.index = 0, x.index = 0, ...){
  if(missing(e))
    stop("must pass e", call. = FALSE)
  
  if(missing(serie))
    stop("must pass serie", call. = FALSE)
  
  if(!step[1] %in% c("start", "middle", "end"))
    stop("wrong step", call. = FALSE)
  
  if(is.null(name)) # defaults to column name
    name <- serie
  
  if(y.index != 0)
    e <- .set_y_axis(e, serie, y.index)
  
  if(x.index != 0)
    e <- .set_x_axis(e, x.index)
  
  # build JSON data
  .build_data(e, e$x$mapping$x, serie) -> vector
  
  if(!is.null(bind))
    vector <- .add_bind(e, vector, bind)
  
  serie <- list(
    name = name,
    type = "line",
    data = vector,
    yAxisIndex = y.index,
    xAxisIndex = x.index,
    step = step[1],
    ...
  )
  
  if(isTRUE(fill)) serie$areaStyle <- list(normal = list())
  
  if(isTRUE(legend))
    e$x$opts$legend$data <- append(e$x$opts$legend$data, list(name))
  
  e$x$opts$series <- append(e$x$opts$series, list(serie))
  e
}

#' @rdname scatter
#' @export
e_scatter_ <- function(e, serie, size = NULL, bind = NULL, scale = "* 1", name = NULL, 
                      coord.system = "cartesian2d", legend = TRUE, y.index = 0, 
                      x.index = 0, rm.x = TRUE, rm.y = TRUE, ...){
  
  if(missing(serie))
    stop("must pass serie", call. = FALSE)
  
  if(is.null(name)) # defaults to column name
    name <- serie
  
  if(y.index != 0)
    e <- .set_y_axis(e, serie, y.index)
  
  if(x.index != 0)
    e <- .set_x_axis(e, x.index)
  
  if(!is.null(size))
    xy <- .build_data(e, e$x$mapping$x, serie, size)
  else
    xy <- .build_data(e, e$x$mapping$x, serie)
  
  if(!is.null(bind))
    xy <- .add_bind(e, xy, bind)
  
  serie <- list(
    name = name,
    type = "scatter",
    data = xy,
    coordinateSystem = coord.system,
    ...
  )
  
  if(coord.system != "cartesian2d"){
    e <- .rm_axis(e, rm.x, "x")
    e <- .rm_axis(e, rm.y, "y")
  } else {
    serie$yAxisIndex = y.index
    serie$xAxisIndex = x.index
    if(isTRUE(legend))
      e$x$opts$legend$data <- append(e$x$opts$legend$data, list(name))
  }
  
  if(!missing(size))
    serie$symbolSize <- htmlwidgets::JS(
      paste("function(data){ return data[2]", scale, ";}")
    )
  
  e$x$opts$series <- append(e$x$opts$series, list(serie))
  e
}

#' @rdname scatter
#' @export
e_effect_scatter_ <- function(e, serie, size = NULL, bind = NULL, scale = "* 1", name = NULL, 
                             coord.system = "cartesian2d", legend = TRUE, 
                             y.index = 0, x.index = 0, rm.x = TRUE, rm.y = TRUE, ...){
  
  if(missing(serie))
    stop("must pass serie", call. = FALSE)
  
  if(is.null(name)) # defaults to column name
    name <- serie
  
  if(y.index != 0)
    e <- .set_y_axis(e, serie, y.index)
  
  if(x.index != 0)
    e <- .set_x_axis(e, x.index)
  
  if(!is.null(size))
    xy <- .build_data(e, e$x$mapping$x, serie, size)
  else
    xy <- .build_data(e, e$x$mapping$x, serie)
  
  if(!is.null(bind))
    xy <- .add_bind(e, xy, bind)
  
  if(coord.system != "cartesian2d"){
    e <- .rm_axis(e, rm.x, "x")
    e <- .rm_axis(e, rm.y, "y")
  } else {
    if(isTRUE(legend))
      e$x$opts$legend$data <- append(e$x$opts$legend$data, list(name))
  }
  
  serie <- list(
    name = name,
    type = "effectScatter",
    data = xy,
    coordinateSystem = coord.system,
    yAxisIndex = y.index,
    xAxisIndex = x.index,
    ...
  )
  
  if(!missing(size))
    serie$symbolSize <- htmlwidgets::JS(
      paste("function(data){ return data[2]", scale, ";}")
    )
  
  e$x$opts$series <- append(e$x$opts$series, list(serie))
  e
}

#' @rdname e_candle
#' @export
e_candle_ <- function(e, opening, closing, low, high, bind = NULL, name = NULL, legend = TRUE, ...){
  
  if(missing(opening) || missing(closing) || missing(low) || missing(high))
    stop("missing inputs", call. = FALSE)
  
  data <- .build_data(e, opening, closing, low, high)
  
  if(!is.null(bind))
    data <- .add_bind(e, data, bind)
  
  serie <- list(
    name = name,
    type = "candlestick",
    data = data,
    ...
  )
  
  if(isTRUE(legend))
    e$x$opts$legend$data <- append(e$x$opts$legend$data, list(name))
  
  e$x$opts$series <- append(e$x$opts$series, list(serie))
  e
}

#' @rdname e_radar
#' @export
e_radar_ <- function(e, serie, max = 100, name = NULL, legend = TRUE, 
                    rm.x = TRUE, rm.y = TRUE, ...){
  
  r.index = 0
  
  if(missing(e))
    stop("must pass e", call. = FALSE)
  
  if(missing(serie))
    stop("must pass serie", call. = FALSE)
  
  # remove axis
  e <- .rm_axis(e, rm.x, "x")
  e <- .rm_axis(e, rm.y, "y")
  
  if(is.null(name)) # defaults to column name
    name <- serie
  
  # build JSON data
  .get_data(e, serie) -> vector
  
  series <- purrr::map(e$x$opts$series, "type") %>% 
    unlist()
  
  if(!"radar" %in% series){
    serie <- list(
      name = name,
      type = "radar",
      data = list(list(value = vector, name = name)),
      radarIndex = r.index,
      ...
    )
    
    # add indicators
    e <- .add_indicators(e, r.index, max) 
    
    # add serie
    e$x$opts$series <- append(e$x$opts$series, list(serie))
  } else { # append to radar
    e$x$opts$series[[grep("radar", series)]]$data <- append(
      e$x$opts$series[[grep("radar", series)]]$data,
      list(list(value = vector, name = name))
    )
  }
  
  if(isTRUE(legend))
    e$x$opts$legend$data <- append(e$x$opts$legend$data, list(name))
  
  e
}

#' @rdname e_funnel
#' @export
e_funnel_ <- function(e, values, labels, name = NULL, legend = TRUE, rm.x = TRUE, rm.y = TRUE, ...){
  
  if(missing(values) || missing(labels))
    stop("missing values or labels", call. = FALSE)
  
  # remove axis
  e <- .rm_axis(e, rm.x, "x")
  e <- .rm_axis(e, rm.y, "y")
  
  # build JSON data
  funnel <- .build_data(e, values)
  
  funnel <- .add_bind(e, funnel, labels)
  
  serie <- list(
    name = name,
    type = "funnel",
    data = funnel,
    ...
  )
  
  # addlegend
  
  if(isTRUE(legend)){
    legend <- .get_data(e, labels) %>% as.character()
    e$x$opts$legend$data <- append(e$x$opts$legend$data, legend)
  }
  
  e$x$opts$series <- append(e$x$opts$series, list(serie))
  e
  
}

#' @rdname e_sankey
#' @export
e_sankey_ <- function(e, source, target, value, layout = "none", rm.x = TRUE, rm.y = TRUE, ...){
  
  if(missing(source) || missing(target) || missing(value))
    stop("missing source, target or values", call. = FALSE)
  
  e <- .rm_axis(e, rm.x, "x")
  e <- .rm_axis(e, rm.y, "y")
  
  # build JSON data
  nodes <- .build_sankey_nodes(e$x$data, source, target)
  
  # build JSON data
  edges <- .build_sankey_edges(e$x$data, source, target, value)
  
  serie <- list(
    type = "sankey",
    layout = layout,
    nodes = nodes,
    links = edges,
    ...
  )
  
  e$x$opts$series <- append(e$x$opts$series, list(serie))
  e
}

#' @rdname e_heatmap
#' @export
e_heatmap_ <- function(e, y, z = NULL, name = NULL, coord.system = "cartesian2d", rm.x = TRUE, rm.y = TRUE, ...){
  if(missing(y))
    stop("must pass y", call. = FALSE)
  
  # build JSON data
  if(!is.null(z))
    xyz <- .build_data(e, e$x$mapping$x, y, z)
  else 
    xyz <- .build_data(e, e$x$mapping$x, y)
  
  serie <- list(
    name = name,
    type = "heatmap",
    data = xyz,
    coordinateSystem = coord.system,
    ...
  )
  
  if(coord.system != "cartesian2d"){
    e <- .rm_axis(e, rm.x, "x")
    e <- .rm_axis(e, rm.y, "y")
  } else {
    e$x$opts$xAxis <- list(
      data = unique(
        .get_data(e, e$x$mapping$x)
      )
    )
    
    e$x$opts$yAxis <- list(
      data = unique(.get_data(e, y))
    )
  }
  
  e$x$opts$series <- append(e$x$opts$series, list(serie))
  e
}

#' @rdname e_parallel
#' @export
e_parallel_ <- function(e, ..., name = NULL, rm.x = TRUE, rm.y = TRUE){
  if(missing(e))
    stop("must pass e", call. = FALSE) 
  
  e <- .rm_axis(e, rm.x, "x")
  e <- .rm_axis(e, rm.y, "y")
  
  e$x$data %>% 
    dplyr::select_(...) -> df
  
  # remove names
  data <- df
  row.names(data) <- NULL
  data <- unname(data)
  
  data <- apply(data, 1, as.list)
  
  serie <- list(
    name = name,
    type = "parallel",
    data = data
  )
  
  para <- list()
  for(i in 1:ncol(df)){
    line <- list()
    line$dim <- i - 1
    line$name <- names(df)[i]
    if(inherits(df[,i], "character") || inherits(df[, i], "factor")){
      line$type <- "category"
      line$data <- unique(df[,i])
    }
    
    para[[i]] <- line
  }
  
  e$x$opts$series <- append(e$x$opts$series, serie)
  e$x$opts$parallelAxis <- para
  e
}

#' @rdname e_pie
#' @export
e_pie_ <- function(e, serie, name = NULL, legend = TRUE, rm.x = TRUE, rm.y = TRUE, ...){
  if(missing(e))
    stop("must pass e", call. = FALSE)
  
  if(missing(serie))
    stop("must pass serie", call. = FALSE)
  
  e <- .rm_axis(e, rm.x, "x")
  e <- .rm_axis(e, rm.y, "y")
  
  if(is.null(name)) # defaults to column name
    name <- serie
  
  # build JSON data
  data <- .build_data(e, serie)
  data <- .add_bind(e, data, e$x$mapping$x)
  
  serie <- list(
    name = name,
    type = "pie",
    data = data,
    ...
  )
  
  if(isTRUE(legend))
    e$x$opts$legend$data <- append(e$x$opts$legend$data, e$x$data[[e$x$mapping$x]])
  
  e$x$opts$series <- append(e$x$opts$series, list(serie))
  e
}