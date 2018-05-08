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