#' @rdname e_bar
#' @export
e_bar_ <- function(e, serie, bind = NULL, name = NULL, legend = TRUE, y_index = 0, x_index = 0, coord_system = "cartesian2d", ...){
  if(missing(e))
    stop("must pass e", call. = FALSE)
  
  if(missing(serie))
    stop("must pass serie", call. = FALSE)
  
  for(i in 1:length(e$x$data)){
    
    nm <- .name_it(e, serie, name, i)
    
    .build_data2(e$x$data[[i]], e$x$mapping$x, serie) -> vector
    
    if(!is.null(bind))
      vector <- .add_bind2(e$x$data[[i]], vector, bind, i = i)
    
    if(y_index != 0)
      e <- .set_y_axis(e, serie, y_index, i)
    
    if(x_index != 0)
      e <- .set_x_axis(e, x_index, i)
    
    e.serie <- list(
      name = nm,
      type = "bar",
      data = vector,
      yAxisIndex = y_index,
      xAxisIndex = x_index,
      coordinateSystem = coord_system,
      ...
    )
    
    if(coord_system == "polar"){
      e.serie$data <- e$x$data[[i]] %>% dplyr::select_(serie) %>% unlist %>% unname %>% as.list
    }
    
    if(isTRUE(legend))
      e$x$opts$legend$data <- append(e$x$opts$legend$data, list(nm))
    
    e$x$opts$series <- append(e$x$opts$series, list(e.serie))
  }
  e
}

#' @rdname e_line
#' @export
e_line_ <- function(e, serie, bind = NULL, name = NULL, legend = TRUE, y_index = 0, x_index = 0, 
                    coord_system = "cartesian2d", ...){
  if(missing(e))
    stop("must pass e", call. = FALSE)
  
  if(missing(serie))
    stop("must pass serie", call. = FALSE)
  

  for(i in 1:length(e$x$data)){
    nm <- .name_it(e, serie, name, i)
    
    # build JSON data
    .build_data2(e$x$data[[i]], e$x$mapping$x, serie) -> vector
    
    if(!is.null(bind))
      vector <- .add_bind2(e, vector, bind, i = i)
    
    l <- list(
      name = nm,
      type = "line",
      data = vector,
      coordinateSystem = coord_system,
      ...
    )
    
    if(coord_system == "cartesian2d"){
      if(y_index != 0)
        e <- .set_y_axis(e, serie, y_index, i)
      
      if(x_index != 0)
        e <- .set_x_axis(e, x_index, i)
      
      l$yAxisIndex <- y_index
      l$xAxisIndex <- x_index
    } else if(coord_system == "polar") {
      l$data <- e$x$data[[i]] %>% dplyr::select_(serie) %>% unlist %>% unname %>% as.list
    }
    
    if(isTRUE(legend))
      e$x$opts$legend$data <- append(e$x$opts$legend$data, list(nm))
    
    e$x$opts$series <- append(e$x$opts$series, list(l))
  }
  
  e
}

#' @rdname e_area
#' @export
e_area_ <- function(e, serie, bind = NULL, name = NULL, legend = TRUE, y_index = 0, x_index = 0, ...){
  if(missing(e))
    stop("must pass e", call. = FALSE)
  
  if(missing(serie))
    stop("must pass serie", call. = FALSE)
  
  for(i in 1:length(e$x$data)){
    nm <- .name_it(e, serie, name, i)
    
    if(y_index != 0)
      e <- .set_y_axis(e, serie, y_index, i)
    
    if(x_index != 0)
      e <- .set_x_axis(e, x_index, i)
    
    # build JSON data
    .build_data2(e$x$data[[i]], e$x$mapping$x, serie) -> vector
    
    if(!is.null(bind))
      vector <- .add_bind2(e, vector, bind, i = i)
    
    e.serie <- list(
      name = nm,
      type = "line",
      data = vector,
      yAxisIndex = y_index,
      xAxisIndex = x_index,
      areaStyle = list(normal = list()),
      ...
    )
    
    if(isTRUE(legend))
      e$x$opts$legend$data <- append(e$x$opts$legend$data, list(nm))
    
    e$x$opts$series <- append(e$x$opts$series, list(e.serie))
  }
  
  e
}

#' @rdname e_step
#' @export
e_step_ <- function(e, serie, bind = NULL, step = c("start", "middle", "end"), fill = FALSE, 
                   name = NULL, legend = TRUE, y_index = 0, x_index = 0, ...){
  if(missing(e))
    stop("must pass e", call. = FALSE)
  
  if(missing(serie))
    stop("must pass serie", call. = FALSE)
  
  if(!step[1] %in% c("start", "middle", "end"))
    stop("wrong step", call. = FALSE)
  
  for(i in 1:length(e$x$data)){
    
    nm <- .name_it(e, serie, name, i)
    
    if(y_index != 0)
      e <- .set_y_axis(e, serie, y_index, i)
    
    if(x_index != 0)
      e <- .set_x_axis(e, x_index, i)
    
    # build JSON data
    .build_data2(e$x$data[[i]], e$x$mapping$x, serie) -> vector
    
    if(!is.null(bind))
      vector <- .add_bind2(e, vector, bind, i = i)
    
    e.serie <- list(
      name = nm,
      type = "line",
      data = vector,
      yAxisIndex = y_index,
      xAxisIndex = x_index,
      step = step[1],
      ...
    )
    
    if(isTRUE(fill)) e.serie$areaStyle <- list(normal = list())
    
    if(isTRUE(legend))
      e$x$opts$legend$data <- append(e$x$opts$legend$data, list(nm))
    
    e$x$opts$series <- append(e$x$opts$series, list(e.serie))
  }
  
  e
}

#' @rdname scatter
#' @export
e_scale <- function(x){
  
  if(!inherits(x, "numeric"))
    stop("x must be numeric")
  
  scales::rescale(x, to = c(1, 20))
}

#' @rdname scatter
#' @export
e_scatter_ <- function(e, serie, size = NULL, bind = NULL, symbol_size = 1, 
                       scale = e_scale, name = NULL, coord_system = "cartesian2d", 
                       legend = TRUE, y_index = 0, x_index = 0, rm_x = TRUE, 
                       rm_y = TRUE, ...){
  
  if(missing(serie))
    stop("must pass serie", call. = FALSE)
  
  for(i in 1:length(e$x$data)){
    
    nm <- .name_it(e, serie, name, i)
    
    if(y_index != 0)
      e <- .set_y_axis(e, serie, y_index, i)
    
    if(x_index != 0)
      e <- .set_x_axis(e, x_index, i)
    
    if(!is.null(size))
      xy <- .build_data_size(e$x$data[[i]], e$x$mapping$x, serie, size, scale, symbol_size)
    else
      xy <- .build_data2(e$x$data[[i]], e$x$mapping$x, serie)
    
    if(!is.null(bind))
      xy <- .add_bind2(e, xy, bind, i = i)
    
    e.serie <- list(
      name = nm,
      type = "scatter",
      data = xy,
      coordinateSystem = coord_system,
      ...
    )
    
    if(!coord_system %in% c("cartesian2d", "singleAxis", "polar")){
      e <- .rm_axis(e, rm_x, "x")
      e <- .rm_axis(e, rm_y, "y")
    } else {
      
      if(coord_system == "cartesian2d"){
        e.serie$yAxisIndex = y_index
        e.serie$xAxisIndex = x_index
      } else if(coord_system == "singleAxis") {
        e.serie$singleAxisIndex = x_index
      } else if(coord_system == "polar"){
        e.serie$data <- e$x$data[[i]] %>% dplyr::select_(serie) %>% unlist %>% unname %>% as.list
      }
      
      if(isTRUE(legend))
        e$x$opts$legend$data <- append(e$x$opts$legend$data, list(nm))
    }
    
    if(!is.null(size)){
      e$scaling <- scale
      e.serie$symbolSize <- htmlwidgets::JS(
        "function(data){ return data[3];}"
      )
    } else {
      size_total <- sum(symbol_size)
      
      if(size_total == 1)
        symbol_size <- 10
      
      e.serie$symbolSize <- symbol_size
    }
    
    e$x$opts$series <- append(e$x$opts$series, list(e.serie))
  }
  
  e
}

#' @rdname scatter
#' @export
e_effect_scatter_ <- function(e, serie, size = NULL, bind = NULL, symbol_size = 1, 
                              scale = e_scale, name = NULL, coord_system = "cartesian2d", 
                              legend = TRUE, y_index = 0, x_index = 0, rm_x = TRUE, 
                              rm_y = TRUE, ...){
  
  if(missing(serie))
    stop("must pass serie", call. = FALSE)
  
  for(i in 1:length(e$x$data)){
    
    nm <- .name_it(e, serie, name, i)
    
    if(y_index != 0)
      e <- .set_y_axis(e, serie, y_index, i)
    
    if(x_index != 0)
      e <- .set_x_axis(e, x_index, i)
    
    if(!is.null(size))
      xy <- .build_data_size(e$x$data[[i]], e$x$mapping$x, serie, size, scale, symbol_size)
    else
      xy <- .build_data2(e$x$data[[i]], e$x$mapping$x, serie)
    
    if(!is.null(bind))
      xy <- .add_bind2(e, xy, bind, i = i)
    
    if(coord_system != "cartesian2d"){
      e <- .rm_axis(e, rm_x, "x")
      e <- .rm_axis(e, rm_y, "y")
    } else {
      if(isTRUE(legend))
        e$x$opts$legend$data <- append(e$x$opts$legend$data, list(nm))
    }
    
    e.serie <- list(
      name = nm,
      type = "effectScatter",
      data = xy,
      coordinateSystem = coord_system,
      yAxisIndex = y_index,
      xAxisIndex = x_index,
      ...
    )
    
    if(!is.null(size)){
      e.serie$symbolSize <- htmlwidgets::JS(
        "function(data){ return data[3];}"
      )
    } else {
      size_total <- sum(symbol_size)
      
      if(size_total == 1)
        symbol_size <- 10
      e.serie$symbolSize <- symbol_size
    }
    
    e$x$opts$series <- append(e$x$opts$series, list(e.serie))
  }
  
  e
}

#' @rdname e_candle
#' @export
e_candle_ <- function(e, opening, closing, low, high, bind = NULL, name = NULL, legend = TRUE, ...){
  
  if(missing(opening) || missing(closing) || missing(low) || missing(high))
    stop("missing inputs", call. = FALSE)
  
  for(i in 1:length(e$x$data)){
    
    data <- .build_data2(e$x$data[[i]], opening, closing, low, high)
    
    if(!is.null(bind))
      data <- .add_bind2(e, data, bind, i = i)
    
    nm <- .name_it(e, NULL, name, i)
    
    e.serie <- list(
      name = nm,
      type = "candlestick",
      data = data,
      ...
    )
    
    if(isTRUE(legend))
      e$x$opts$legend$data <- append(e$x$opts$legend$data, list(nm))
    
    e$x$opts$series <- append(e$x$opts$series, list(e.serie))
  }
  e
}

#' @rdname e_radar
#' @export
e_radar_ <- function(e, serie, max = 100, name = NULL, legend = TRUE, 
                    rm_x = TRUE, rm_y = TRUE, ...){
  
  r.index = 0
  
  if(missing(e))
    stop("must pass e", call. = FALSE)
  
  if(missing(serie))
    stop("must pass serie", call. = FALSE)
  
  # remove axis
  e <- .rm_axis(e, rm_x, "x")
  e <- .rm_axis(e, rm_y, "y")
  
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
e_funnel_ <- function(e, values, labels, name = NULL, legend = TRUE, rm_x = TRUE, rm_y = TRUE, ...){
  
  if(missing(values) || missing(labels))
    stop("missing values or labels", call. = FALSE)
  
  # remove axis
  e <- .rm_axis(e, rm_x, "x")
  e <- .rm_axis(e, rm_y, "y")
  
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
e_sankey_ <- function(e, source, target, value, layout = "none", rm_x = TRUE, rm_y = TRUE, ...){
  
  if(missing(source) || missing(target) || missing(value))
    stop("missing source, target or values", call. = FALSE)
  
  e <- .rm_axis(e, rm_x, "x")
  e <- .rm_axis(e, rm_y, "y")
  
  # build JSON data
  nodes <- .build_sankey_nodes(e$x$data[[1]], source, target)
  
  # build JSON data
  edges <- .build_sankey_edges(e$x$data[[1]], source, target, value)
  
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
e_heatmap_ <- function(e, y, z = NULL, name = NULL, coord_system = "cartesian2d", 
                       rm_x = TRUE, rm_y = TRUE, calendar = NULL, ...){
  
  if(missing(y))
    stop("must pass y", call. = FALSE)
  
  for(i in 1:length(e$x$data)){
    
    if(!is.null(z))
      xyz <- .build_data2(e$x$data[[i]], e$x$mapping$x, y, z)
    else 
      xyz <- .build_data2(e$x$data[[i]], e$x$mapping$x, y)
    
    serie <- list(
      name = name,
      type = "heatmap",
      data = xyz,
      coordinateSystem = coord_system,
      ...
    )
    
    if(coord_system == "calendar"){
      if(!is.null(calendar)){
        serie$calendarIndex <- calendar[i]
      } else if(length(e$x$opts$calendar) == length(e$x$data)) {
        serie$calendarIndex <- i - 1
      } 
    }
    
    if(coord_system != "cartesian2d"){
      e <- .rm_axis(e, rm_x, "x")
      e <- .rm_axis(e, rm_y, "y")
    } else {
      
      xdata <- unique(.get_data(e, e$x$mapping$x, i))
      
      if(length(xdata) == 1)
        xdata <- list(xdata)
      
      e$x$opts$xAxis <- list(
        list(
          data = xdata
        )
      )
      
      ydata <- unique(.get_data(e, y, i))
      
      if(length(ydata) == 1)
        ydata <- list(ydata)
      
      e$x$opts$yAxis <- list(
        list(
          data = ydata
        )
      )
    }
    
    e$x$opts$series <- append(e$x$opts$series, list(serie))
  }
  
  e
}

#' @rdname e_parallel
#' @export
e_parallel_ <- function(e, ..., name = NULL, rm_x = TRUE, rm_y = TRUE){
  
  if(missing(e))
    stop("must pass e", call. = FALSE) 
  
  e <- .rm_axis(e, rm_x, "x")
  e <- .rm_axis(e, rm_y, "y")
  
  e$x$data[[1]] %>% 
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
e_pie_ <- function(e, serie, name = NULL, legend = TRUE, rm_x = TRUE, rm_y = TRUE, ...){
  if(missing(e))
    stop("must pass e", call. = FALSE)
  
  if(missing(serie))
    stop("must pass serie", call. = FALSE)
  
  e <- .rm_axis(e, rm_x, "x")
  e <- .rm_axis(e, rm_y, "y")
  
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

#' @rdname e_sunburst
#' @export
e_sunburst_ <- function(e, parent, child, value, itemStyle = NULL, rm_x = TRUE, rm_y = TRUE, ...){
  if(missing(e))
    stop("must pass e", call. = FALSE)
  
  e <- .rm_axis(e, rm_x, "x")
  e <- .rm_axis(e, rm_y, "y")
  
  # build JSON data
  data <- .build_sun(e, parent, child, value, itemStyle)
  
  serie <- list(
    type = "sunburst",
    data = data,
    ...
  )
  
  e$x$opts$series <- append(e$x$opts$series, list(serie))
  e
}

#' @rdname e_treemap
#' @export
e_treemap_ <- function(e, parent, child, value, rm_x = TRUE, rm_y = TRUE, ...){
  if(missing(e))
    stop("must pass e", call. = FALSE)
  
  if(missing(parent) || missing(child) || missing(value))
    stop("must pass parent, child and value", call. = FALSE)
  
  e <- .rm_axis(e, rm_x, "x")
  e <- .rm_axis(e, rm_y, "y")
  
  # build JSON data
  data <- .build_sun(e, parent, child, value)
  
  serie <- list(
    type = "treemap",
    data = data,
    ...
  )
  
  e$x$opts$series <- append(e$x$opts$series, list(serie))
  e
}

#' @rdname e_river
#' @export
e_river_ <- function(e, serie, name = NULL, legend = TRUE, rm_x = TRUE, rm_y = TRUE, ...){
  if(missing(e))
    stop("must pass e", call. = FALSE)
  
  if(missing(serie))
    stop("must pass serie", call. = FALSE)
  
  for(i in 1:length(e$x$data)){
    
    nm <- .name_it(e, serie, name, i)
    
    if(length(e$x$opts$xAxis$data))
      e$X <- e$x$opts$xAxis$data
    
    # build JSON data
    data <- .build_river(e, serie, nm, i)
    
    if(!length(e$x$opts$series)){
      e.serie <- list(
        type = "themeRiver",
        data = data,
        ...
      )
      e$x$opts$series <- append(e$x$opts$series, list(e.serie))
    } else {
      e$x$opts$series[[1]]$data <- append(e$x$opts$series[[1]]$data, data)
    }
    
    e <- .rm_axis(e, rm_x, "x")
    e <- .rm_axis(e, rm_y, "y")
    
    e$x$opts$singleAxis <- list(type = "time")
    
    if(isTRUE(legend))
      e$x$opts$legend$data <- append(e$x$opts$legend$data, list(nm))
  }
  
  e
}

#' @rdname e_boxplot
#' @export
e_boxplot_ <- function(e, serie, name = NULL, outliers = TRUE, ...){
  
  if(missing(serie))
    stop("must pass serie", call. = FALSE)
  
  for(i in 1:length(e$x$data)){
    nm <- .name_it(e, serie, NULL, i)
    
    # build JSON data
    vector <- .build_boxplot(e, serie, i)
    
    if(length(e$x$opts$series) >= 1){
      e$x$opts$series[[1]]$data <- append(
        e$x$opts$series[[1]]$data, 
        list(vector)
      )
    } else {
      # boxplot + opts
      box <- list(
        name = nm,
        type = "boxplot",
        data = list(vector),
        ...
      )
      e$x$opts$series <- append(e$x$opts$series, list(box))
    }
    
    # data/outliers
    if(isTRUE(outliers)){
      e <- .add_outliers(e, serie, i)
    }
    
    # xaxis
    e$x$opts$xAxis$data <- append(e$x$opts$xAxis$data, list(nm))
    e$x$opts$xAxis$type <- "category"
  }
  
  e
}

#' @rdname e_tree
#' @export
e_tree_ <- function(e, parent, child, rm_x = TRUE, rm_y = TRUE, ...){
  if(missing(e))
    stop("must pass e", call. = FALSE)
  
  if(missing(parent) || missing(child))
    stop("must pass parent and child", call. = FALSE)
  
  # remove axis
  e <- .rm_axis(e, rm_x, "x")
  e <- .rm_axis(e, rm_y, "y")
  
  # build JSON data
  data <- .build_tree(e, parent, child)
  
  serie <- list(
    type = "tree",
    data = list(data),
    ...
  )
  
  e$x$opts$series <- append(e$x$opts$series, list(serie))
  e
}

#' @rdname line3D
#' @export
e_lines_3d_ <- function(e, source_lon, source_lat, target_lon, target_lat, name = NULL, coord_system = "globe",
                        rm_x = TRUE, rm_y = TRUE, ...){
  if(missing(e))
    stop("must pass e", call. = FALSE)
  
  if(missing(source_lat) || missing(source_lon) || missing(target_lat) || missing(target_lon))
    stop("missing coordinates", call. = FALSE)
  
  # remove axis
  e <- .rm_axis(e, rm_x, "x")
  e <- .rm_axis(e, rm_y, "y")
  
  for(i in 1:length(e$x$data)){
    
    data <- .map_lines(e, source_lon, source_lat, target_lon, target_lat, i)
    
    serie <- list(
      type = "lines3D",
      coordinateSystem = coord_system,
      data = data,
      ...
    )
    
    nm <- .name_it(e, NULL, name, i)
    
    if(!is.null(nm)){
      e$x$opts$legend$data <- append(e$x$opts$legend$data, list(nm))
      serie$name <- nm
    }
    
    e$x$opts$series <- append(e$x$opts$series, list(serie))
  }
  
  e
}

#' @rdname line3D
#' @export
e_line_3d_ <- function(e, y, z, name = NULL, coord_system = NULL, rm_x = TRUE, rm_y = TRUE, ...){
  if(missing(e))
    stop("must pass e", call. = FALSE)
  
  if(missing(y) || missing(z))
    stop("missing coordinates", call. = FALSE)
  
  # remove axis
  e <- .rm_axis(e, rm_x, "x")
  e <- .rm_axis(e, rm_y, "y")

  for(i in 1:length(e$x$data)){
    
    nm <- .name_it(e, NULL, name, i)
    
    if(!is.null(nm))
      e$x$opts$legend$data <- append(e$x$opts$legend$data, nm)
    
    if(!length(e$x$opts$zAxis3D))
      e$x$opts$zAxis3D <- list(list(show = TRUE))
    
    if(!length(e$x$opts$grid3D))
      e$x$opts$grid3D <- list(list(show = TRUE))
    
    e <- .set_axis_3D(e, "x", e$x$mapping$x, 0)
    e <- .set_axis_3D(e, "y", y, 0)
    e <- .set_axis_3D(e, "z", z, 0)
    
    # build JSON data
    data <- .build_data2(e$x$data[[i]], e$x$mapping$x, y, z)
    
    e.serie <- list(
      type = "line3D",
      data = data,
      name = nm,
      ...
    )
    
    if(!is.null(coord_system))
      e.serie$coordinateSystem <- coord_system
    
    e$x$opts$series <- append(e$x$opts$series, list(e.serie))
  }
  
  e
}

#' @rdname e_bar_3d
#' @export
e_bar_3d_ <- function(e, y, z, bind = NULL, coord_system = "cartesian3D", name = NULL, rm_x = TRUE, rm_y = TRUE, ...){
  
  if(missing(e))
    stop("must pass e", call. = FALSE)
  
  if(missing(y) || missing(z))
    stop("must pass y and z", call. = FALSE)
  
  # remove axis
  e <- .rm_axis(e, rm_x, "x")
  e <- .rm_axis(e, rm_y, "y")
  
  for(i in 1:length(e$x$data)){
    
    # globe
    if(coord_system != "cartesian3D"){
      
      data <- .build_data2(e$x$data[[i]], e$x$mapping$x, y, z)
      
      if(!is.null(bind))
        data <- .add_bind2(e, data, bind, i = i)
      
    } else { # cartesian
      
      if(!length(e$x$opts$zAxis3D))
        e$x$opts$zAxis3D <- list(list(show = TRUE))
      
      if(!length(e$x$opts$grid3D))
        e$x$opts$grid3D <- list(list(show = TRUE))
      
      e <- .set_axis_3D(e, "x", e$x$mapping$x, 0)
      e <- .set_axis_3D(e, "y", y, 0)
      e <- .set_axis_3D(e, "z", z, 0)
      
      data <- .build_cartesian3D(e, e$x$mapping$x, y, z, i = i)
    }
    
    nm <- .name_it(e, NULL, name, i)
    
    if(!is.null(nm))
      e$x$opts$legend$data <- append(e$x$opts$legend$data, nm)
    
    e.serie <- list(
      name = nm,
      type = "bar3D",
      coordinateSystem = coord_system,
      data = data,
      ...
    )
    
    e$x$opts$series <- append(e$x$opts$series, list(e.serie))
    
  }
  
  e
}

#' @rdname e_lines
#' @export
e_lines_ <- function(e, source_lon, source_lat, target_lon, target_lat, coord_system = "geo", name = NULL, 
                    rm_x = TRUE, rm_y = TRUE, ...){
  if(missing(e))
    stop("must pass e", call. = FALSE)
  
  if(missing(source_lat) || missing(source_lon) || missing(target_lat) || missing(target_lon))
    stop("missing coordinates", call. = FALSE)
  
  # remove axis
  e <- .rm_axis(e, rm_x, "x")
  e <- .rm_axis(e, rm_y, "y")
  
  
  for(i in 1:length(e$x$data)){
    
    nm <- .name_it(e, NULL, name, i)
    
    data <- .map_lines(e, source_lon, source_lat, target_lon, target_lat, i)
    
    e.serie <- list(
      name = nm,
      type = "lines",
      coordinateSystem = coord_system,
      data = data,
      ...
    )
    
    e$x$opts$series <- append(e$x$opts$series, list(e.serie))
  }
  
  e
}

#' @rdname e_scatter_3d
#' @export
e_scatter_3d_ <- function(e, y, z, color = NULL, size = NULL, bind = NULL, coord_system = "cartesian3D", name = NULL, 
                         rm_x = TRUE, rm_y = TRUE, legend = FALSE, ...){
  if(missing(e))
    stop("must pass e", call. = FALSE)
  
  if(missing(y) || missing(z))
    stop("must pass y and z", call. = FALSE)
  
  if(is.null(name)) # defaults to column name
    name <- z
  
  # remove axis
  e <- .rm_axis(e, rm_x, "x")
  e <- .rm_axis(e, rm_y, "y")
  
  for(i in 1:length(e$x$data)){
    # globe
    if(coord_system != "cartesian3D"){
      
      data <- .build_data2(e$x$data[[i]], e$x$mapping$x, y, z)
      
      if(!is.null(bind))
        data <- .add_bind2(e, data, bind, i = i)
      
      
    } else { # cartesian
      
      if(!length(e$x$opts$zAxis3D))
        e$x$opts$zAxis3D <- list(list(show = TRUE))
      
      if(!length(e$x$opts$grid3D))
        e$x$opts$grid3D <- list(list(show = TRUE))
      
      e <- .set_axis_3D(e, "x", e$x$mapping$x, 0)
      e <- .set_axis_3D(e, "y", y, 0)
      e <- .set_axis_3D(e, "z", z, 0)
      
      if(is.null(color))
        data <- .build_data2(e$x$data[[i]], e$x$mapping$x, y, z)
      else if(!is.null(color) && is.null(size))
        data <- .build_data2(e$x$data[[i]], e$x$mapping$x, y, z, color)
      else if(!is.null(color) && !is.null(size))
        data <- .build_data2(e$x$data[[i]], e$x$mapping$x, y, z, color, size)
      
    }
    
    nm <- .name_it(e, NULL, name, i)
    
    e.serie <- list(
      name = nm,
      type = "scatter3D",
      coordinateSystem = coord_system,
      data = data,
      ...
    )
    
    if(isTRUE(legend))
      e$x$opts$legend$data <- append(e$x$opts$legend$data, list(nm))
    
    e$x$opts$series <- append(e$x$opts$series, list(e.serie))
  }
  
  e
}

#' @rdname e_flow_gl
#' @export
e_flow_gl_ <- function(e, y, sx, sy, color = NULL, name = NULL, coord_system = NULL, rm_x = TRUE, rm_y = TRUE, ...){
  if(missing(e))
    stop("must pass e", call. = FALSE)
  
  if(missing(y) || missing(sx) || missing(sy))
    stop("must pass y and z", call. = FALSE)
  
  if(is.null(coord_system)){
    e <- .set_x_axis(e, 0)
    e <- .set_y_axis(e, deparse(substitute(y)), 0)
  } else {
    # remove axis
    e <- .rm_axis(e, rm_x, "x")
    e <- .rm_axis(e, rm_y, "y")
  }
  
  if(is.null(color))
    data <- .build_data(e, e$x$mapping$x, y, sx,sy)
  else 
    data <- .build_data(e, e$x$mapping$x, y, sx, sy, color)
  
  serie <- list(
    type = "flowGL",
    data = data,
    ...
  )
  
  if(!is.null(name))
    serie$name <- name
  
  if(!is.null(coord_system))
    serie$coordinateSystem <- coord_system
  
  e$x$opts$series <- append(e$x$opts$series, list(serie))
  
  e
}

#' @rdname e_scatter_gl
#' @export
e_scatter_gl_ <- function(e, y, z, name = NULL, coord_system = "geo", rm_x = TRUE, rm_y = TRUE, ...){
  if(missing(e))
    stop("must pass e", call. = FALSE)
  
  if(missing(y) || missing(z))
    stop("must pass y and z", call. = FALSE)
  
  for(i in 1:length(e$x$data)){
    
    nm <- .name_it(e, z, name, i)
    
    # remove axis
    e <- .rm_axis(e, rm_x, "x")
    e <- .rm_axis(e, rm_y, "y")
    
    data <- .build_data2(e$x$data[[i]], e$x$mapping$x, y, z)
    
    # globe
    if(coord_system == "cartesian3D"){
      if(!length(e$x$opts$zAxis3D))
        e$x$opts$zAxis3D <- list(list(show = TRUE))
      
      if(!length(e$x$opts$grid3D))
        e$x$opts$grid3D <- list(list(show = TRUE))
      
      e <- .set_axis_3D(e, "x", e$x$mapping$x, 0)
      e <- .set_axis_3D(e, "y", y, 0)
      e <- .set_axis_3D(e, "z", z, 0)
    } 
    
    serie <- list(
      name = nm,
      type = "scatterGL",
      coordinateSystem = coord_system,
      data = data,
      ...
    )
    
    e$x$opts$series <- append(e$x$opts$series, list(serie))
  }
  
  e
}

#' @rdname e_pictorial
#' @export
e_pictorial_ <- function(e, serie, symbol, bind = NULL, name = NULL, legend = TRUE, y_index = 0, x_index = 0, ...){
  
  if(missing(e))
    stop("must pass e", call. = FALSE)
  
  if(missing(serie) || missing(symbol))
    stop("must pass serie and symbol", call. = FALSE)
  
  if(is.null(name)) # defaults to column name
    name <- serie
  
  if(y_index != 0)
    e <- .set_y_axis(e, serie, y_index)
  
  if(x_index != 0)
    e <- .set_x_axis(e, x_index)
  
  # build JSON data
  .build_data(e, e$x$mapping$x, serie) -> vector
  
  if(!is.null(bind))
    vector <- .add_bind(e, vector, bind)
  
  if(symbol %in% colnames(e$x$data[[1]]))
    vector <- .add_bind(e, vector, symbol, "symbol")
  
  serie <- list(
    name = name,
    type = "pictorialBar",
    data = vector,
    yAxisIndex = y_index,
    xAxisIndex = x_index,
    ...
  )
  
  if(!symbol %in% colnames(e$x$data[[1]]))
    serie$symbol <- symbol
  
  if(isTRUE(legend))
    e$x$opts$legend$data <- append(e$x$opts$legend$data, list(name))
  
  e$x$opts$series <- append(e$x$opts$series, list(serie))
  e
}

#' @rdname histogram
#' @export
e_histogram_ <- function(e, serie, breaks = "Sturges", name = NULL, legend = TRUE,
                        bar.width = "99%", x_index = 0, y_index = 0, ...){
  
  if(missing(e))
    stop("must pass e", call. = FALSE)
  
  if(missing(serie))
    stop("must pass serie", call. = FALSE)
  
  for(i in 1:length(e$x$data)){
    
    nm <- .name_it(e, NULL, name, i)
    
    data <- .get_data(e, serie, i)
    histogram <- hist(data, plot = FALSE, breaks)
    
    hist <- data.frame(
      histogram$mids,
      histogram$counts
    )
    
    hist <- apply(unname(hist), 1, as.list)
    
    if(y_index != 0)
      e <- .set_y_axis(e, serie, y_index)
    
    if(x_index != 0)
      e <- .set_x_axis(e, x_index)
    
    if(!length(e$x$opts$xAxis))
      e$x$opts$xAxis <- list(
        list(
          type = "value", scale = TRUE
        )
      )
    
    e.serie <- list(
      name = nm,
      type = "bar",
      data = hist,
      barWidth = bar.width,
      yAxisIndex = y_index,
      xAxisIndex = x_index,
      stack = "stackedHistogram",
      ...
    )
    
    if(isTRUE(legend))
      e$x$opts$legend$data <- append(e$x$opts$legend$data, list(nm))
    
    e$x$opts$series <- append(e$x$opts$series, list(e.serie))
  }
  
  e
}

#' @rdname histogram
#' @export
e_density_ <- function(e, serie, breaks = "Sturges", name = NULL, legend = TRUE, 
                      x_index = 0, y_index = 0, ...){
  if(missing(e))
    stop("must pass e", call. = FALSE)
  
  if(missing(serie))
    stop("must pass serie", call. = FALSE)
  
  if(is.null(name)) # defaults to column name
    name <- serie
  
  data <- .get_data(e, serie)
  histogram <- hist(data, plot = FALSE, breaks)
  
  hist <- data.frame(
    histogram$mids,
    histogram$density
  )
  
  hist <- apply(unname(hist), 1, as.list)
  
  if(y_index != 0)
    e <- .set_y_axis(e, serie, y_index)
  
  if(x_index != 0)
    e <- .set_x_axis(e, x_index)
  
  if(!length(e$x$opts$xAxis))
    e$x$opts$xAxis <- list(
      list(
        type = "value", scale = TRUE
      )
    )
  
  serie <- list(
    name = name,
    type = "line",
    data = hist,
    yAxisIndex = y_index,
    xAxisIndex = x_index,
    ...
  )
  
  if(isTRUE(legend))
    e$x$opts$legend$data <- append(e$x$opts$legend$data, list(name))
  
  e$x$opts$series <- append(e$x$opts$series, list(serie))
  e
}
