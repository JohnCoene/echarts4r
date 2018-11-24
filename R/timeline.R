#' Timeline
#' 
#' Create a timeline.
#' 
#' @inheritParams init
#' @param ... Options passed to \code{\link{e_timeline_serie_opts}}.
#' 
#' @details Group your data by steps with \link[dplyr]{group_by}, see examples.
#' 
#' @examples 
#' iris %>% 
#'   group_by(Species) %>% 
#'   e_timeline(Sepal.Length, autoPlay = TRUE) %>% 
#'   e_timeline_serie(Sepal.Width) %>% 
#'   e_timeline_serie(Petal.Length) 
#' 
#' @export
e_timeline <- function(data, x, width = NULL, height = NULL, elementId = NULL, dispose = TRUE, renderer = "canvas", ...) {
  
  xmap <- NULL
  if(!missing(x))
    xmap <- deparse(substitute(x))
  
  if(!dplyr::is_grouped_df(data))
    stop("must pass grouped data", call. = FALSE)
  
  # forward options using x
  x = list(
    theme = "",
    renderer = tolower(renderer),
    mapping = list(),
    events = list(),
    buttons = list(),
    opts = list(
      baseOption = list(
        yAxis = list(
          list(show = TRUE)
        )
      ),
      options = lapply(1:dplyr::n_groups(data), function(x) list())
    )
  )
  
  if(!missing(data)){
    
    row.names(data) <- NULL
    
    if(!is.null(xmap))
      data <- .arrange_data_x(data, xmap)
    
    x$data <- map_grps_(data)
  }
  
  x$opts$baseOption$timeline <- list(
    data = names(x$data),
    axisType = "category",
    ...
  )
  
  if(!is.null(xmap)){
    x$mapping$x <- xmap[1]
    x$mapping$x_class <- class(data[[xmap]])

    x$mapping$include_x <- FALSE
    cl <- x$mapping$x_class
    if(cl == "character" || cl == "factor"){
      labs <- unique(data[[x$mapping$x]])
      
      if(length(labs) == 1)
        labs <- list(labs)
      
      x$opts$baseOption$xAxis <- list(list(data = labs, type = "category", boundaryGap = TRUE))
    } else if(cl == "POSIXct" || cl == "POSIXlt" || cl == "Date") {
      
      labs <- unique(data[[x$mapping$x]])
      
      if(length(labs) == 1)
        labs <- list(labs)
      
      x$opts$baseOption$xAxis <- list(list(data = labs, type = "time", boundaryGap = TRUE))
    } else {
      x$mapping$include_x <- TRUE
      x$opts$baseOption$xAxis <- list(list(type = "value"))
    }
  }
  
  x$dispose <- dispose
  
  # create widget
  htmlwidgets::createWidget(
    name = 'echarts4r',
    x,
    width = width,
    height = height,
    package = 'echarts4r',
    elementId = elementId,
    preRenderHook = echarts_build,
    sizingPolicy = htmlwidgets::sizingPolicy(
      defaultWidth = "100%",
      knitr.figure = FALSE,
      browser.fill = TRUE
    )
  )
}

#' Timeline serie
#' 
#' Add timeline series.
#' 
#' @param e An \code{echarts4r} object as returned by \code{\link{e_timeline}}.
#' @param type Chart type.
#' @param name name of serie.
#' @param serie_name Name of serie, used for legend.
#' @param coord_system Coordinate system.
#' @param legend Set to \code{TRUE} to show \code{serie_name} in legend.
#' @param y,z Any column name.
#' @param base_opts A \code{list} of base serie options.
#' 
#' @examples 
#' iris %>% 
#'   group_by(Species) %>% 
#'   e_timeline(Sepal.Length, autoPlay = TRUE) %>% 
#'   e_timeline_serie(
#'     Sepal.Width, 
#'     serie_name = "Width", 
#'     legend = TRUE
#'   ) %>% 
#'   e_timeline_serie(
#'     Petal.Length, 
#'     serie_name = "Length", 
#'     legend = TRUE
#'   ) 
#' 
#' @name timeline-serie
#' @export
e_timeline_serie <- function(e, y, z, type = "line", name = NULL, serie_name = NULL, 
                             legend = FALSE, base_opts = NULL, coord_system = "cartesian2d"){
  
  if(!missing(z))
    z <- deparse(substitute(z))
  else
    z <- NULL
  
  e_timeline_serie_(
    e = e, 
    y = deparse(substitute(y)), 
    z = z, 
    type = type, 
    name = name, 
    serie_name = serie_name, 
    legend = legend, 
    base_opts = base_opts,
    coord_system = coord_system
  )
}

#' @rdname timeline-serie
#' @export
e_timeline_serie_ <- function(e, y, z = NULL, type = "line", name = NULL, serie_name = NULL, 
                              legend = FALSE, base_opts = NULL, coord_system = "cartesian2d"){
  
  if(missing(e))
    stop("must pass see e, see e_timeline", call. = FALSE)
  
  # series data
  for(i in 1:length(e$x$data)){
    
    if(isTRUE(legend))
      e$x$opts$baseOption$legend$data <- append(e$x$opts$baseOption$legend$data, serie_name) 
    
    # data
    if(type %in% c("bar", "line", "scatter")){
      
      data <- .build_data2(e$x$data[[i]], e$x$mapping$x, y)
      
      if(!is.null(name))
        data <- .add_bind(e, data, name)
      
    } else if(type %in% c("pie")){
      data <- .build_data2(e$x$data[[i]], y)
      data <- .add_bind(e, data, e$x$mapping$x)
    }
    
    if(coord_system != "cartesian2d"){
      e$x$opts$baseOptions$xAxis <- NULL
      e$x$opts$baseOptions$yAxis <- NULL
    }
    
    e_serie <- list(
      data = data
    )
    
    e$x$opts$options[[i]]$series <- append(e$x$opts$options[[i]]$series, list(e_serie))
  }
  
  # series options
  series_opts <- list(
    name = serie_name, 
    type = type,
    coordinateSystem = coord_system
  )
  
  if(!is.null(base_opts))
    series_opts <- append(series_opts, base_opts)
  
  e$x$opts$baseOption$series <- append(e$x$opts$baseOption$series, list(series_opts))
  e
}

#' Timeline series options
#' 
#' @param e An \code{echarts4r} object as returned by \code{\link{e_timeline}}.
#' @param ... Named options \emph{the same length as the number of steps} (groups).
#' 
#' @examples 
#' iris %>% 
#'   group_by(Species) %>% 
#'   e_timeline(Sepal.Length, autoPlay = TRUE) %>% 
#'   e_timeline_serie(Sepal.Width) %>% 
#'   e_timeline_serie(Petal.Length) %>% 
#'   e_timeline_serie_opts(
#'     title = list(
#'       list(text = "setosa"),
#'       list(text = "versicolor"),
#'       list(text = "virginica")
#'     )
#'   )
#' 
#' @export
e_timeline_serie_opts <- function(e, ...){
  
  args <- list(...)
  
  for(i in 1:length(e$x$opts$options)){
    
    for(j in 1:length(args)){
      e$x$opts$options[[i]][[names(args)[j]]] <- args[[j]][[i]]
    }
    
  }
  
  e
  
}

#' Timeline options
#' 
#' Set timeline options.
#' 
#' @param e An \code{echarts4r} object as returned by \code{\link{e_timeline}}.
#' @param axis_type Type of axis, \code{category}, \code{value} or \code{time}.
#' @param data Timeline steps.
#' @param ... Any other named option.
#' 
#' @examples 
#' iris %>% 
#'   group_by(Species) %>% 
#'   e_timeline(Sepal.Length) %>% 
#'   e_timeline_serie_("Sepal.Width") %>% 
#'   e_timeline_serie_("Petal.Length") %>% 
#'   e_timeline_opts(
#'     autoPlay = TRUE,
#'     rewind = TRUE
#'   )
#' 
#' @export
e_timeline_opts <- function(e, axis_type = "category", data = NULL, ...){
  
  if(!is.null(data))
    e$x$opts$baseOption$timeline$data <- data
  
  e$x$opts$baseOption$timeline$axisType <- axis_type
  
  e$x$opts$baseOption$timeline <- append(e$x$opts$baseOption$timeline, list(...))
  e
}

#' Base Options
#' 
#' Add base options
#' 
#' @param e An \code{echarts4r} object as returned by \code{\link{e_timeline}}.
#' @param ... Any option.
#' 
#' @export
e_timeline_base_opts <- function(e, ...){
  e$x$opts$baseOption <- append(e$x$opts$baseOption, list(...))
  e
}