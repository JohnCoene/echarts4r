#' Bar and Line chart
#' 
#' Add bar serie.
#' 
#' @param e An \code{echarts4r} object as returned by \code{\link{e_charts}}.
#' @param serie Column name of serie to plot.
#' @param name name of the serie.
#' @param ... Any other option to pass to \code{bar} or \code{line} char types.
#' @param x.index,y.index Indexes of x and y axis.
#' 
#' @examples 
#' USArrests %>% 
#'   dplyr::mutate(
#'     State = row.names(.),
#'     Rape = -Rape
#'   ) %>% 
#'   e_charts(State) %>% 
#'   e_bar(Murder) %>% 
#'   e_bar(Rape, name = "Sick basterd", x.index = 1) # second y axis
#' 
#' @export
e_bar <- function(e, serie, name = NULL, y.index = 0, x.index = 0, ...){
  
  if(missing(e))
    stop("must pass e", call. = FALSE)

  if(missing(serie))
    stop("must pass serie", call. = FALSE)
  
  serie <- deparse(substitute(serie))
  
  if(is.null(name)) # defaults to column name
    name <- serie
  
  if(y.index != 0)
    e <- .set_y_axis(e, serie, y.index)
  
  if(x.index != 0)
    e <- .set_x_axis(e, x.index)
  
  # build JSON data
  vector <- .redirect_vect_xy(e, serie)

  serie <- list(
    name = name,
    type = "bar",
    data = vector,
    yAxisIndex = y.index,
    xAxisIndex = x.index,
    ...
  )

  e$x$opts$legend$data <- append(e$x$opts$legend$data, list(name))

  e$x$opts$series <- append(e$x$opts$series, list(serie))
  e
}

#' Line 
#' 
#' Add line serie.
#' 
#' @inheritParams e_bar
#' @param coord.system Coordinate system to plot against.
#' 
#' @examples 
#' USArrests %>% 
#'   e_charts(Assault) %>% 
#'   e_line(Murder) %>% 
#'   e_line(UrbanPop, y.index = 1) # second y axis
#' 
#' @export
e_line <- function(e, serie, name = NULL, y.index = 0, x.index = 0, coord.system = "cartesian2d", ...){
  if(missing(e))
    stop("must pass e", call. = FALSE)
  
  if(missing(serie))
    stop("must pass serie", call. = FALSE)
  
  serie <- deparse(substitute(serie))
  
  if(is.null(name)) # defaults to column name
    name <- serie
  
  # build JSON data
  vector <- .redirect_vect_xy(e, serie)
  
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
  
  e$x$opts$legend$data <- append(e$x$opts$legend$data, list(name))
  
  e$x$opts$series <- append(e$x$opts$series, list(l))
  e
}

#' Area 
#' 
#' Add area serie.
#' 
#' @inheritParams e_bar
#' 
#' @examples 
#' USArrests %>% 
#'   e_charts(Assault) %>% 
#'   e_area(Murder, stack = "grp") %>% # stacking
#'   e_area(UrbanPop, stack = "grp") # stacking
#' 
#' @export
e_area <- function(e, serie, name = NULL, y.index = 0, x.index = 0, ...){
  if(missing(e))
    stop("must pass e", call. = FALSE)
  
  if(missing(serie))
    stop("must pass serie", call. = FALSE)
  
  serie <- deparse(substitute(serie))
  
  if(is.null(name)) # defaults to column name
    name <- serie
  
  if(y.index != 0)
    e <- .set_y_axis(e, serie, y.index)
  
  if(x.index != 0)
    e <- .set_x_axis(e, x.index)
  
  # build JSON data
  vector <- .redirect_vect_xy(e, serie)
  
  serie <- list(
    name = name,
    type = "line",
    data = vector,
    yAxisIndex = y.index,
    xAxisIndex = x.index,
    areaStyle = list(normal = list()),
    ...
  )
  
  e$x$opts$legend$data <- append(e$x$opts$legend$data, list(name))
  
  e$x$opts$series <- append(e$x$opts$series, list(serie))
  e
}

#' Step 
#' 
#' Add step serie.
#' 
#' @inheritParams  e_bar
#' @param step Step type, one of \code{start}, \code{middle} or \code{end}.
#' @param fill Set to fill as area.
#' 
#' @examples 
#' USArrests %>% 
#'   dplyr::mutate(State = row.names(.)) %>% 
#'   e_charts(State) %>%
#'   e_step(Murder, name = "Start", step = "start", fill = TRUE) %>% 
#'   e_step(Rape, name = "Middle", step = "middle") %>% 
#'   e_step(Assault, name = "End", step = "end") %>% 
#'   e_tooltip(trigger = "axis")
#' 
#' @export
e_step <- function(e, serie, step = c("start", "middle", "end"), fill = FALSE, 
                   name = NULL, y.index = 0, x.index = 0, ...){
  if(missing(e))
    stop("must pass e", call. = FALSE)
  
  if(missing(serie))
    stop("must pass serie", call. = FALSE)
  
  if(!step %in% c("start", "middle", "end"))
    stop("wrong step", call. = FALSE)
  
  serie <- deparse(substitute(serie))
  
  if(is.null(name)) # defaults to column name
    name <- serie
  
  if(y.index != 0)
    e <- .set_y_axis(e, serie, y.index)
  
  if(x.index != 0)
    e <- .set_x_axis(e, x.index)
  
  # build JSON data
  vector <- .redirect_vect_xy(e, serie)
  
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
  
  e$x$opts$legend$data <- append(e$x$opts$legend$data, list(name))
  
  e$x$opts$series <- append(e$x$opts$series, list(serie))
  e
}

#' Scatter
#' 
#' Add scatter serie.
#' 
#' @inheritParams e_bar
#' @param size Column name containing size of points.
#' @param scale Scale for \code{size}, defaults to \code{* 1} which multiplies the size
#'  by \code{1} (equivalent to no multiplier).
#' @param coord.system Coordinate system to plot against.
#' 
#' @examples 
#' USArrests %>% 
#'   e_charts(Assault) %>% 
#'   e_scatter(Murder, Rape) %>% 
#'   e_scatter(Rape, Murder, y.index = 1) %>% 
#'   e_grid(index = c(0, 1)) %>% 
#'   e_tooltip()
#' 
#' quakes %>% 
#'   e_charts(long) %>% 
#'   e_geo(
#'     roam = TRUE,
#'     boundingCoords = list(
#'       c(185, - 10),
#'       c(165, -40)
#'     )
#'   ) %>% 
#'   e_effect_scatter(lat, mag, coord.system = "geo") %>% 
#'   e_visual_map(min = 4, max = 6.5)
#'   
#' # Calendar
#' year <- seq.Date(as.Date("2017-01-01"), as.Date("2017-12-31"), by = "day")
#' values <- rnorm(length(year), 20, 6)
#' 
#' year <- data.frame(year = year, values = values)
#' 
#' year %>% 
#'   e_charts(year) %>% 
#'   e_calendar(range = "2018") %>% 
#'   e_scatter(values, coord.system = "calendar") %>% 
#'   e_visual_map(max = 30)
#'   
#' 
#' @rdname scatter
#' @export
e_scatter <- function(e, serie, size, scale = "* 1", name = NULL, coord.system = "cartesian2d", y.index = 0, x.index = 0, ...){
  
  if(missing(serie))
    stop("must pass serie", call. = FALSE)
  
  serie <- deparse(substitute(serie))
  
  if(is.null(name)) # defaults to column name
    name <- serie
  
  if(y.index != 0)
    e <- .set_y_axis(e, serie, y.index)
  
  if(x.index != 0)
    e <- .set_x_axis(e, x.index)
  
  if(!missing(size))
    xy <- .build_data(e, e$x$mapping$x, serie, deparse(substitute(size)))
  else
    xy <- .build_data(e, e$x$mapping$x, serie)
  
  serie <- list(
    name = name,
    type = "scatter",
    data = xy,
    coordinateSystem = coord.system,
    ...
  )
  
  if(coord.system != "cartesian2d"){
    e$x$opts$xAxis <- NULL
    e$x$opts$yAxis <- NULL
  } else {
    serie$yAxisIndex = y.index
    serie$xAxisIndex = x.index
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
e_effect_scatter <- function(e, serie, size, scale = "* 1", name = NULL, coord.system = "cartesian2D", y.index = 0, x.index = 0, ...){
  
  if(missing(serie))
    stop("must pass serie", call. = FALSE)
  
  serie <- deparse(substitute(serie))
  
  if(is.null(name)) # defaults to column name
    name <- serie
  
  if(y.index != 0)
    e <- .set_y_axis(e, serie, y.index)
  
  if(x.index != 0)
    e <- .set_x_axis(e, x.index)
  
  if(!missing(size))
    xy <- .build_data(e, e$x$mapping$x, serie, deparse(substitute(size)))
  else
    xy <- .build_data(e, e$x$mapping$x, serie)
  
  if(coord.system != "cartesian2D"){
    e$x$opts$xAxis <- NULL
    e$x$opts$yAxis <- NULL
  } else {
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

#' Candlestick
#' 
#' Add a candlestick chart.
#' 
#' @inheritParams e_bar
#' @param opening,closing,low,high Stock prices
#' 
#' @examples 
#' date <- c("2017-01-01", "2017-01-02", "2017-01-03", "2017-01-04", "2017-03-05", 
#'          "2017-01-06", "2017-01-07")
#'          
#' stock <- data.frame(
#'   date = date,
#'   opening = c(200.60, 200.22, 198.43, 199.05, 203.54, 203.40, 208.34),
#'   closing = c(200.72, 198.85, 199.05, 203.73, 204.08, 208.11, 211.88),
#'   low = c(197.82, 198.07, 197.90, 198.10, 202.00, 201.50, 207.60),
#'   high = c(203.32, 200.67, 200.00, 203.95, 204.90, 208.44, 213.17)
#' )
#' 
#' stock %>% 
#'   e_charts(date) %>% 
#'   e_candle(opening, closing, low, high)
#' 
#' @export
e_candle <- function(e, opening, closing, low, high, name = NULL, ...){
  
  data <- .build_data(
    e, 
    deparse(substitute(opening)), 
    deparse(substitute(closing)), 
    deparse(substitute(low)), 
    deparse(substitute(high))
  )
  
  serie <- list(
    name = name,
    type = "candlestick",
    data = data,
    ...
  )
  
  e$x$opts$legend$data <- append(e$x$opts$legend$data, list(name))
  
  e$x$opts$series <- append(e$x$opts$series, list(serie))
  e
}

#' Funnel
#' 
#' Add a funnel.
#' 
#' @param e An \code{echarts4r} object as returned by \code{\link{e_charts}}.
#' @param name name of the serie.
#' @param ... Any other option to pass to \code{bar} or \code{line} char types.
#' @param values,labels Values and labels of funnel.
#' 
#' @examples 
#' funnel <- data.frame(stage = c("View", "Click", "Purchase"), value = c(80, 30, 20))
#' 
#' funnel %>% 
#'   e_charts() %>% 
#'   e_funnel(value, stage)
#' 
#' @export
e_funnel <- function(e, values, labels, name = NULL, ...){
  
  if(missing(values) || missing(labels))
    stop("missing values or labels", call. = FALSE)
  
  e$x$opts$xAxis <- NULL # remove
  e$x$opts$yAxis <- NULL # remove
  e$x$opts$legend <- NULL # remove
  
  # build JSON data
  funnel <- .build_data(
    e, 
    deparse(substitute(values)), 
    deparse(substitute(labels)),
    names = c("value", "name")
  )
  
  serie <- list(
    name = name,
    type = "funnel",
    data = funnel,
    ...
  )
  
  e$x$opts$legend$data <- append(e$x$opts$legend$data, list(name))
  
  e$x$opts$series <- append(e$x$opts$series, list(serie))
  e
  
}

#' Sankey
#' 
#' Draw a sankey diagram.
#' 
#' @inheritParams e_bar
#' @param layout Layout of sankey.
#' @param source,target Source and target columns.
#' @param value Value change from \code{source} to \code{target}.
#' 
#' @examples
#' sankey <- data.frame(
#'   source = c("a", "b", "c", "d", "c"),
#'   target = c("b", "c", "d", "e", "e"),
#'   value = ceiling(rnorm(5, 10, 1)),
#'   stringsAsFactors = FALSE
#' )
#' 
#' sankey %>% 
#'   e_charts() %>% 
#'   e_sankey(source, target, value) 
#' 
#' @export
e_sankey <- function(e, source, target, value, layout = "none", ...){
  
  if(missing(source) || missing(target) || missing(value))
    stop("missing source, target or values", call. = FALSE)
  
  e$x$opts$xAxis <- NULL # remove
  e$x$opts$yAxis <- NULL # remove
  e$x$opts$legend <- NULL # remove
  
  # build JSON data
  nodes <- .build_sankey_nodes(
    e$x$data, 
    deparse(substitute(source)), 
    deparse(substitute(target))
  )
  
  # build JSON data
  edges <- .build_sankey_edges(
    e$x$data, 
    dplyr::enquo(source), 
    dplyr::enquo(target),
    dplyr::enquo(value)
  )
  
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
#' Graph
#' 
#' Create a graph.
#' 
#' @param e An \code{echarts4} object as returned by \code{e_charts}.
#' @param name Name of graph.
#' @param nodes Data.frame of nodes.
#' @param names Names of nodes, unique.
#' @param value values of nodes.
#' @param size Size of nodes.
#' @param category Group of nodes (i.e.: group membership).
#' @param edges Data.frame of edges.
#' @param source,target Column names of source and target.
#' @param layout Layout, one of \code{force}, \code{none} or \code{circular}.
#' @param ... Any other parameter.
#' 
#' @examples 
#' #Use graphGL for larger networks
#' nodes <- data.frame(
#'   name = paste0(LETTERS, 1:1000),
#'   value = rnorm(1000, 10, 2),
#'   size = rnorm(1000, 10, 2),
#'   grp = rep(c("grp1", "grp2"), 500),
#'   stringsAsFactors = FALSE
#' )
#' 
#' edges <- data.frame(
#'   source = sample(nodes$name, 2000, replace = TRUE),
#'   target = sample(nodes$name, 2000, replace = TRUE),
#'   stringsAsFactors = FALSE
#' )
#' 
#' e_charts() %>% 
#'   e_graph_gl() %>% 
#'   e_graph_nodes(nodes, name, value, size, grp) %>% 
#'   e_graph_edges(edges, source, target)
#' 
#' @rdname graph
#' @export
e_graph <- function(e, layout = "force", name = NULL, ...){
  if(missing(e))
    stop("must pass e", call. = FALSE)
  
  e$x$opts$xAxis <- NULL # remove
  e$x$opts$yAxis <- NULL # remove
  
  serie <- list(
    name = name,
    type = "graph",
    layout = layout,
    ...
  )
  
  e$x$opts$series <- append(e$x$opts$series, list(serie))
  e
}

#' @rdname graph
#' @export
e_graph_gl <- function(e, layout = "force", name = NULL, ...){
  if(missing(e))
    stop("must pass e", call. = FALSE)
  
  e$x$opts$xAxis <- NULL # remove
  e$x$opts$yAxis <- NULL # remove
  
  serie <- list(
    name = name,
    type = "graphGL",
    layout = layout,
    ...
  )
  
  e$x$opts$series <- append(e$x$opts$series, list(serie))
  e
}

#' @rdname graph
#' @export
e_graph_nodes <- function(e, nodes, names, value, size, category){
  
  if(missing(nodes) || missing(names))
    stop("must pass nodes and names", call. = FALSE)
  
  if(!missing(category)){
    e$x$opts$series[[length(e$x$opts$series)]]$categories <- .build_graph_category(nodes, dplyr::enquo(category))
    #e$x$opts$legend$data <- .graph_cat_legend(nodes, dplyr::enquo(category))
  }
  
  value <- dplyr::enquo(value)
  symbolSize <- dplyr::enquo(size)
  names <- dplyr::enquo(names)
  
  nodes <- .build_graph_nodes(
    nodes, 
    names, 
    value,
    symbolSize,
    dplyr::enquo(category)
  )
  
  # build JSON data
  e$x$opts$series[[length(e$x$opts$series)]]$data <- nodes
  e
}

#' @rdname graph
#' @export
e_graph_edges <- function(e, edges, source, target){
  if(missing(edges) || missing(source) || missing(target))
    stop("must pass edges, source and target", call. = FALSE)
  
  source <- dplyr::enquo(source)
  target <- dplyr::enquo(target)
  
  data <- .build_graph_edges(
    edges, 
    source, 
    target
  )
  
  # build JSON data
  e$x$opts$series[[length(e$x$opts$series)]]$links <- data
  e
}

#' Heatmap
#' 
#' Draw heatmap by coordinates.
#' 
#' @inheritParams e_bar
#' @param y,z Coordinates and values.
#' @param coord.system Coordinate system to plot against, takes 
#' \code{cartesian2d}, \code{geo} or \code{calendar}.
#' 
#' @examples 
#' v <- LETTERS[1:10]
#' matrix <- data.frame(
#'   x = sample(v, 300, replace = TRUE), 
#'   y = sample(v, 300, replace = TRUE), 
#'   z = rnorm(300, 10, 1),
#'   stringsAsFactors = FALSE
#' ) %>% 
#'   dplyr::group_by(x, y) %>% 
#'   dplyr::summarise(z = sum(z)) %>% 
#'   dplyr::ungroup()
#' 
#' matrix %>% 
#'   e_charts(x) %>% 
#'   e_heatmap(y, z) %>% 
#'   e_visual_map(min = 1, max = max(matrix$z))
#'
#' # calendar   
#' dates <- seq.Date(as.Date("2018-01-01"), as.Date("2018-12-31"), by = "day")
#' values <- rnorm(length(dates), 20, 6)
#' 
#' year <- data.frame(date = dates, values = values)
#' 
#' year %>% 
#'   e_charts(date) %>% 
#'   e_calendar(range = "2018") %>% 
#'   e_heatmap(values, coord.system = "calendar") %>% 
#'   e_visual_map(max = 30)
#' 
#' @export
e_heatmap <- function(e, y, z, name = NULL, coord.system = "cartesian2d", ...){
  if(missing(y))
    stop("must pass y", call. = FALSE)
  
  # build JSON data
  if(!missing(z))
    xyz <- .build_data(e, e$x$mapping$x, deparse(substitute(y)), deparse(substitute(z)))
  else 
    xyz <- .build_data(e, e$x$mapping$x, deparse(substitute(y)))
  
  serie <- list(
    name = name,
    type = "heatmap",
    data = xyz,
    coordinateSystem = coord.system,
    ...
  )
  
  if(coord.system != "cartesian2d"){
    e$x$opts$xAxis <- NULL
    e$x$opts$yAxis <- NULL
  } else {
    e$x$opts$xAxis <- list(
      data = unique(
        .get_data(e, e$x$mapping$x)
      )
    )
    
    e$x$opts$yAxis <- list(
      data = unique(
        .get_data(e, deparse(substitute(y))
        )
      )
    )
  }
  
  e$x$opts$series <- append(e$x$opts$series, list(serie))
  e
}

#' Parallel
#' 
#' Draw parallel coordinates.
#' 
#' @inheritParams e_bar
#' 
#' @examples 
#' df <- data.frame(
#'   price = rnorm(5, 10),
#'   amount = rnorm(5, 15),
#'   letter = LETTERS[1:5]
#' )
#' 
#' df %>% 
#'   e_charts() %>% 
#'   e_parallel(price, amount, letter) 
#' 
#' @export
e_parallel <- function(e, ..., name = NULL){
  if(missing(e))
    stop("must pass e", call. = FALSE) 
  
  e$x$opts$xAxis <- NULL # remove
  e$x$opts$yAxis <- NULL # remove
  
  e$x$data %>% 
    dplyr::select(...) -> df
  
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

#' Pie
#' 
#' Draw pie and donut charts.
#' 
#' @inheritParams e_bar
#' 
#' @examples 
#' mtcars %>% 
#'   head() %>% 
#'   dplyr::mutate(model = row.names(.)) %>% 
#'   e_charts(model) %>% 
#'   e_pie(carb)
#' 
#' @export
e_pie <- function(e, serie, name = NULL, ...){
  if(missing(e))
    stop("must pass e", call. = FALSE)
  
  if(missing(serie))
    stop("must pass serie", call. = FALSE)
  
  e$x$opts$xAxis <- NULL # remove
  e$x$opts$yAxis <- NULL # remove
  
  if(is.null(name)) # defaults to column name
    name <- deparse(substitute(serie))
  
  # build JSON data
  data <- .build_data(e, e$x$mapping$x, deparse(substitute(serie)), names = c("name", "value"))
  
  serie <- list(
    name = name,
    type = "pie",
    data = data,
    ...
  )
  
  e$x$opts$legend$data <- append(e$x$opts$legend$data, .graph_cat_legend(e))
  
  e$x$opts$series <- append(e$x$opts$series, list(serie))
  e
}

#' Sunburst
#' 
#' Build a sunburst.
#' 
#' @inheritParams e_bar
#' @param parent,child Edges.
#' @param value Name of column containing values.
#' 
#' @examples 
#' \dontrun{
#' df <- data.frame(
#'   parent = c("earth", "earth", "earth", "mars", "mars"), 
#'   child = c("forest", "ocean", "iceberg", "elon", "curiosity"),
#'   value = ceiling(rnorm(5, 10, 2))
#' )
#' 
#' df %>% 
#'   e_charts() %>% 
#'   e_sunburst(parent, child, value)
#' }
#' 
#' @export
e_sunburst <- function(e, parent, child, value, ...){
  if(missing(e))
    stop("must pass e", call. = FALSE)
  
  e$x$opts$xAxis <- NULL # remove
  e$x$opts$yAxis <- NULL # remove
  
  # build JSON data
  data <- .build_sun(
    e, 
    deparse(substitute(parent)), 
    deparse(substitute(child)),
    deparse(substitute(value))
  )
  
  serie <- list(
    type = "sunburst",
    data = data,
    ...
  )
  
  e$x$opts$series <- append(e$x$opts$series, list(serie))
  e
}

#' Treemap
#' 
#' Build a treemap.
#' 
#' @inheritParams e_bar
#' @param parent,child Edges.
#' @param value Value of edges.
#' 
#' @examples 
#' \dontrun{
#' df <- data.frame(
#'   parent = c("earth", "earth", "earth", "mars", "mars"), 
#'   child = c("forest", "ocean", "iceberg", "elon", "curiosity"),
#'   value = ceiling(rnorm(5, 10, 2))
#' )
#' 
#' df %>% 
#'   e_charts() %>% 
#'   e_treemap(parent, child, value)
#' }
#' 
#' @export
e_treemap <- function(e, parent, child, value, ...){
  if(missing(e))
    stop("must pass e", call. = FALSE)
  
  if(missing(parent) || missing(child) || missing(value))
    stop("must pass parent, child and value", call. = FALSE)
  
  e$x$opts$xAxis <- NULL # remove
  e$x$opts$yAxis <- NULL # remove
  
  # build JSON data
  data <- .build_sun(
    e, 
    deparse(substitute(parent)), 
    deparse(substitute(child)),
    deparse(substitute(value))
  )
  
  serie <- list(
    type = "treemap",
    data = data,
    ...
  )
  
  e$x$opts$series <- append(e$x$opts$series, list(serie))
  e
}

#' River
#' 
#' Build a theme river.
#' 
#' @inheritParams e_bar
#' 
#' @examples 
#' dates <- seq.Date(Sys.Date() - 30, Sys.Date(), by = "day")
#' 
#' df <- data.frame(
#'   dates = dates,
#'   apples = runif(length(dates)),
#'   bananas = runif(length(dates)),
#'   pears = runif(length(dates))
#' )
#' 
#' df %>% 
#'   e_charts(dates) %>% 
#'   e_river(apples) %>% 
#'   e_river(bananas) %>% 
#'   e_river(pears) %>% 
#'   e_tooltip(trigger = "axis")
#' 
#' @export
e_river <- function(e, serie, name = NULL, ...){
  if(missing(e))
    stop("must pass e", call. = FALSE)
  
  if(missing(serie))
    stop("must pass serie", call. = FALSE)
  
  if(is.null(name)) # defaults to column name
    name <- deparse(substitute(serie))
  
  if(length(e$x$opts$xAxis$data))
    e$X <- e$x$opts$xAxis$data
  
  # build JSON data
  data <- .build_river(e, deparse(substitute(serie)), name)
  
  if(!length(e$x$opts$series)){
    serie <- list(
      type = "themeRiver",
      data = data,
      ...
    )
    e$x$opts$series <- append(e$x$opts$series, list(serie))
  } else {
    e$x$opts$series[[1]]$data <- append(e$x$opts$series[[1]]$data, data)
  }
  
  e$x$opts$xAxis <- NULL # remove
  e$x$opts$yAxis <- NULL # remove
  
  e$x$opts$singleAxis <- list(type = "time")
  
  e
}

#' Boxplot
#' 
#' Draw boxplot.
#' 
#' @inheritParams e_bar
#' @param outliers Whether to plot outliers.
#
#' 
#' @examples 
#' df <- data.frame(
#'   x = c(1:10, 25),
#'   y = c(1:10, -6)
#' )
#' 
#' df %>% 
#'   e_charts() %>% 
#'   e_boxplot(y, outliers = TRUE) %>% 
#'   e_boxplot(x, outliers = TRUE)
#' 
#' @export
e_boxplot <- function(e, serie, name = NULL, outliers = TRUE, ...){
  
  if(missing(serie))
    stop("must pass serie", call. = FALSE)
  
  if(is.null(name)) # defaults to column name
    name <- deparse(substitute(serie))
  
  # build JSON data
  serie <- dplyr::enquo(serie)
  vector <- .build_boxplot(e$x$data, serie)
  
  if(length(e$x$opts$series) >= 1){
    e$x$opts$series[[1]]$data <- append(
      e$x$opts$series[[1]]$data, 
      list(vector)
    )
  } else {
    # boxplot + opts
    box <- list(
      name = name,
      type = "boxplot",
      data = list(vector),
      ...
    )
    e$x$opts$series <- append(e$x$opts$series, list(box))
  }
  
  # data/outliers
  if(isTRUE(outliers)){
    e <- .add_outliers(e, serie)
  }
  
  # xaxis
  e$x$opts$xAxis$data <- append(e$x$opts$xAxis$data, list(name))
  e$x$opts$xAxis$type <- "category"
  
  # legend
  # e$x$opts$legend$data <- append(e$x$opts$legend$data, list(name))
  
  e
}

#' Tree
#' 
#' Build a tree.
#' 
#' @inheritParams e_bar
#' @param parent,child Edges.
#' 
#' @examples 
#' df <- data.frame(
#'   parent = c("earth","earth","forest","forest","ocean","ocean","ocean","ocean"), 
#'   child = c("ocean","forest","tree","sasquatch","fish","seaweed","mantis shrimp","sea monster")
#' )
#' 
#' df %>% 
#'   e_charts() %>% 
#'   e_tree(parent, child)
#' 
#' @export
e_tree <- function(e, parent, child, ...){
  if(missing(e))
    stop("must pass e", call. = FALSE)
  
  if(missing(parent) || missing(child))
    stop("must pass parent and child", call. = FALSE)
  
  e$x$opts$xAxis <- NULL # remove
  e$x$opts$yAxis <- NULL # remove
  
  # build JSON data
  data <- .build_tree(
    e, 
    deparse(substitute(parent)), 
    deparse(substitute(child))
  )
  
  serie <- list(
    type = "tree",
    data = list(data),
    ...
  )
  
  e$x$opts$series <- append(e$x$opts$series, list(serie))
  e
}

#' Gauge
#' 
#' Plot a gauge.
#' 
#' @inheritParams e_bar
#' @param value Value to gauge.
#' @param name Text on gauge.
#' 
#' @examples 
#' e_charts() %>% 
#'   e_gauge(57, "PERCENT")
#' 
#' @export
e_gauge <- function(e, value, name, ...){
  
  if(missing(e) || missing(value) || missing(name))
    stop("missing e, name, or value", call. = FALSE)
  
  if(!inherits(value, "numeric"))
    stop("must pass numeric or interger", call. = FALSE)
  
  e$x$opts$yAxis <- NULL
  e$x$opts$xAxis <- NULL
  
  e$x$opts$series <- list(
    list(
      type = "gauge",
      data = list(list(value = value, name = name))
    )
  )
  e
}

#' Lines 3D
#' 
#' Add 3D lines.
#' 
#' @inheritParams e_bar
#' @param coord.system Coordinate system to use, one of \code{cartesian3D}, \code{globe}.
#' @param source.lon,source.lat,target.lon,target.lat coordinates.
#' 
#' @examples 
#' flights <- read.csv(
#'   paste0("https://raw.githubusercontent.com/plotly/datasets/",
#'          "master/2011_february_aa_flight_paths.csv")
#' )
#' 
#' flights %>% 
#'   e_charts() %>% 
#'   e_globe(
#'     base.texture = e_map_texture(),
#'     height.texture = e_map_texture(),
#'     environment = e_stars_texture(),
#'     displacementScale = 0.05
#'   ) %>% 
#'   e_line_3d(
#'     start_lon, 
#'     start_lat, 
#'     end_lon, 
#'     end_lat,
#'     name = "flights"
#'   )
#'   
#' flights %>% 
#'   e_charts() %>% 
#'   e_geo_3d() %>% 
#'   e_line_3d(
#'     start_lon, 
#'     start_lat, 
#'     end_lon, 
#'     end_lat,
#'     coord.system = "geo3D"
#'   )
#' 
#' @export
e_line_3d <- function(e, source.lon, source.lat, target.lon, target.lat, name = NULL, coord.system = "globe", ...){
  if(missing(e))
    stop("must pass e", call. = FALSE)
  
  if(missing(source.lat) || missing(source.lon) || missing(target.lat) || missing(target.lon))
    stop("missing coordinates", call. = FALSE)
  
  e$x$opts$xAxis <- NULL # remove
  e$x$opts$yAxis <- NULL # remove
  
  if(!is.null(name))
    e$x$opts$legend$data <- append(e$x$opts$legend$data, name)
  
  # build JSON data
  data <- .map_lines(
    e, 
    deparse(substitute(source.lon)), 
    deparse(substitute(source.lat)), 
    deparse(substitute(target.lon)), 
    deparse(substitute(target.lat))
  )
  
  serie <- list(
    type = "lines3D",
    coordinateSystem = coord.system,
    data = data,
    ...
  )
  
  e$x$opts$series <- append(e$x$opts$series, list(serie))
  
  e
}

#' Bar 3D
#' 
#' Add 3D bars
#' 
#' @inheritParams e_bar
#' @param y,z Coordinates.
#' @param coord.system Coordinate system to use, one of \code{cartesian3D}, \code{geo3D}, \code{globe}.
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
#'   e_globe(
#'     environment = e_stars_texture(),
#'     base.texture = e_globe_texture()
#'   ) %>% 
#'   e_bar_3d(lat, value, coord.system = "globe") %>% 
#'   e_visual_map()
#'   
#' data %>% 
#'   e_charts(lon) %>% 
#'   e_geo_3d() %>% 
#'   e_bar_3d(lat, value, coord.system = "geo3D") %>% 
#'   e_visual_map()
#'   
#' v <- LETTERS[1:10]
#' matrix <- data.frame(
#'   x = sample(v, 300, replace = TRUE), 
#'   y = sample(v, 300, replace = TRUE), 
#'   z1 = rnorm(300, 10, 1),
#'   z2 = rnorm(300, 10, 1),
#'   stringsAsFactors = FALSE
#' ) %>% 
#'   dplyr::group_by(x, y) %>% 
#'   dplyr::summarise(
#'     z1 = sum(z1),
#'     z2 = sum(z2)
#'   ) %>% 
#'   dplyr::ungroup() 
#'   
#' trans <- list(opacity = 0.4) # transparency
#' emphasis <- list(itemStyle = list(color = "#313695"))
#'   
#' matrix %>% 
#'   e_charts(x) %>% 
#'   e_bar_3d(y, z1, stack = "stack", name = "Serie 1", itemStyle = trans, emphasis = emphasis) %>%
#'   e_bar_3d(y, z2, stack = "stack", name = "Serie 2", itemStyle = trans, emphasis = emphasis) %>% 
#'   e_legend()
#' }
#' 
#' @export
e_bar_3d <- function(e, y, z, coord.system = "cartesian3D", name = NULL, ...){
  if(missing(e))
    stop("must pass e", call. = FALSE)
  
  if(missing(y) || missing(z))
    stop("must pass y and z", call. = FALSE)
  
  if(is.null(name)) # defaults to column name
    name <- deparse(substitute(z))
  
  e$x$opts$xAxis <- NULL # remove
  e$x$opts$yAxis <- NULL # remove
  
  # globe
  if(coord.system != "cartesian3D"){
  
    data <- .build_data(e, e$x$mapping$x, deparse(substitute(y)), deparse(substitute(z)))
    
  } else { # cartesian
    
    if(!length(e$x$opts$zAxis3D))
      e$x$opts$zAxis3D <- list(show = TRUE)
    
    if(!length(e$x$opts$grid3D))
      e$x$opts$grid3D <- list(show = TRUE)
    
    e <- .set_axis_3D(e, "x", e$x$mapping$x, 0)
    e <- .set_axis_3D(e, "y", deparse(substitute(y)), 0)
    e <- .set_axis_3D(e, "z", deparse(substitute(z)), 0)
    
    data <- .build_cartesian3D(
      e, 
      e$x$mapping$x, 
      deparse(substitute(y)), 
      deparse(substitute(z))
    )
  }
  
  serie <- list(
    name = name,
    type = "bar3D",
    coordinateSystem = coord.system,
    data = data,
    ...
  )
  
  e$x$opts$series <- append(e$x$opts$series, list(serie))
  
  e
}

#' Lines
#' 
#' Add lines.
#' 
#' @inheritParams e_bar
#' @param source.lon,source.lat,target.lon,target.lat coordinates.
#' @param coord.system Coordinate system to use, one of \code{geo}, or \code{cartesian2D}.
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
e_lines <- function(e, source.lon, source.lat, target.lon, target.lat, coord.system = "geo", name = NULL, ...){
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
  
  serie <- list(
    name = name,
    type = "lines",
    coordinateSystem = coord.system,
    data = data,
    ...
  )
  
  e$x$opts$series <- append(e$x$opts$series, list(serie))
  
  e
}

#' Scatter 3D
#' 
#' Add 3D scatter.
#' 
#' @inheritParams e_bar
#' @param y,z Coordinates.
#' @param color,size Color and Size of bubbles.
#' @param coord.system Coordinate system to use, one of \code{geo3D}, \code{globe}, or \code{cartesian3D}.
#' 
#' @examples 
#' v <- LETTERS[1:10]
#' matrix <- data.frame(
#'   x = sample(v, 300, replace = TRUE), 
#'   y = sample(v, 300, replace = TRUE), 
#'   z = rnorm(300, 10, 1),
#'   color = rnorm(300, 10, 1),
#'   size = rnorm(300, 10, 1),
#'   stringsAsFactors = FALSE
#' ) %>% 
#'   dplyr::group_by(x, y) %>% 
#'   dplyr::summarise(
#'     z = sum(z),
#'     color = sum(color),
#'     size = sum(size)
#'   ) %>% 
#'   dplyr::ungroup() 
#'   
#' matrix %>% 
#'   e_charts(x) %>% 
#'   e_scatter_3d(y, z, size, color) %>% 
#'   e_visual_map(
#'     min = 1, max = 100,
#'     inRange = list(symbolSize = c(1, 30)), # scale size
#'     dimension = 3 # third dimension 0 = x, y = 1, z = 2, size = 3
#'   ) %>% 
#'   e_visual_map(
#'     min = 1, max = 100,
#'     inRange = list(color = c('#bf444c', '#d88273', '#f6efa6')), # scale colors
#'     dimension = 4, # third dimension 0 = x, y = 1, z = 2, size = 3, color = 4
#'     bottom = 300 # padding to avoid visual maps overlap
#'   )
#'   
#' airports <- read.csv(
#'   paste0("https://raw.githubusercontent.com/plotly/datasets/",
#'          "master/2011_february_us_airport_traffic.csv")
#' )
#' 
#' airports %>% 
#'   e_charts(long) %>% 
#'   e_globe(
#'     environment = e_stars_texture(),
#'     base.texture = e_globe_texture(), 
#'     globeOuterRadius = 100
#'   ) %>% 
#'   e_scatter_3d(lat, cnt, coord.system = "globe", blendMode = 'lighter') %>% 
#'   e_visual_map(inRange = list(symbolSize = c(1, 10)))
#' 
#' @export
e_scatter_3d <- function(e, y, z, color, size, coord.system = "cartesian3D", name = NULL, ...){
  if(missing(e))
    stop("must pass e", call. = FALSE)
  
  if(missing(y) || missing(z))
    stop("must pass y and z", call. = FALSE)
  
  if(is.null(name)) # defaults to column name
    name <- deparse(substitute(z))
  
  e$x$opts$xAxis <- NULL # remove
  e$x$opts$yAxis <- NULL # remove
  
  # globe
  if(coord.system != "cartesian3D"){
    
    data <- .build_data(e, e$x$mapping$x, deparse(substitute(y)), deparse(substitute(z)))
  
    
  } else { # cartesian
    
    if(!length(e$x$opts$zAxis3D))
      e$x$opts$zAxis3D <- list(show = TRUE)
    
    if(!length(e$x$opts$grid3D))
      e$x$opts$grid3D <- list(show = TRUE)
    
    e <- .set_axis_3D(e, "x", e$x$mapping$x, 0)
    e <- .set_axis_3D(e, "y", deparse(substitute(y)), 0)
    e <- .set_axis_3D(e, "z", deparse(substitute(z)), 0)
    
    if(missing(color))
      data <- .build_data(e, e$x$mapping$x, deparse(substitute(y)), deparse(substitute(z)))
    else if(!missing(color) && missing(size))
      data <- .build_data(e, e$x$mapping$x, deparse(substitute(y)), deparse(substitute(z)), deparse(substitute(color)))
    else if(!missing(color) && !missing(size))
      data <- .build_data(e, e$x$mapping$x, deparse(substitute(y)), deparse(substitute(z)), deparse(substitute(color)),
                          deparse(substitute(size)))

  }
  
  serie <- list(
    name = name,
    type = "scatter3D",
    coordinateSystem = coord.system,
    data = data,
    ...
  )
  
  e$x$opts$series <- append(e$x$opts$series, list(serie))
  
  e
}

#' Flow GL
#' 
#' @inheritParams e_bar
#' @param y Vector position on the y axis.
#' @param sx,sy Velocity in respective axis.
#' @param color Vector color.
#' @param coord.system Coordinate system to use.
#' 
#' @examples 
#' # coordinates
#' vectors <- expand.grid(0:9, 0:9)
#' names(vectors) <- c("x", "y")
#' vectors$sx <- rnorm(100)
#' vectors$sy <- rnorm(100)
#' vectors$color <- log10(runif(100, 1, 10))
#' 
#' vectors %>% 
#'   e_charts(x) %>% 
#'   e_flow_gl(y, sx, sy, color) %>% 
#'   e_visual_map(
#'     min = 0, max = 1, # log 10
#'     dimension = 4, # x = 0, y = 1, sx = 3, sy = 4
#'     show = FALSE, # hide
#'     inRange = list(
#'       color = c('#313695', '#4575b4', '#74add1', '#abd9e9', '#e0f3f8',
#'                 '#ffffbf', '#fee090', '#fdae61', '#f46d43', '#d73027', '#a50026')
#'     )
#'   ) %>% 
#'   e_x_axis(
#'     splitLine = list(show = FALSE)
#'   ) %>% 
#'   e_y_axis(
#'     splitLine = list(show = FALSE)
#'   )   
#'   
#' # map
#' latlong <- seq(-180, 180, by = 5)
#' wind = expand.grid(lng = latlong, lat = latlong)
#' wind$slng <- rnorm(nrow(wind), 0, 200)
#' wind$slat <- rnorm(nrow(wind), 0, 200)
#' wind$color <- abs(wind$slat) - abs(wind$slng)
#' rng <- range(wind$color)
#' 
#' trans <- list(opacity = 0.5) # transparency
#' 
#' wind %>% 
#'   e_charts(lng, backgroundColor = '#333') %>% 
#'   e_geo(
#'     itemStyle = list(
#'       normal = list(
#'         areaColor = "#323c48",
#'         borderColor = "#111"
#'       )
#'     )
#'   ) %>% 
#'   e_flow_gl(lat, slng, slat, color, 
#'     coord.system = "geo", 
#'     itemStyle = trans,
#'     particleSize = 2
#'   ) %>% 
#'   e_visual_map(
#'     min = rng[1], max = rng[2], # range
#'     dimension = 4, # lng = 0, lat = 1, slng = 2, slat = 3, color = 4
#'     show = FALSE, # hide
#'     inRange = list(
#'       color = c('#313695', '#4575b4', '#74add1', '#abd9e9', '#e0f3f8', 
#'                 '#ffffbf', '#fee090', '#fdae61', '#f46d43', '#d73027', '#a50026')
#'     )
#'   )
#' 
#' @export
e_flow_gl <- function(e, y, sx, sy, color, name = NULL, coord.system = NULL, ...){
  if(missing(e))
    stop("must pass e", call. = FALSE)
  
  if(missing(y) || missing(sx) || missing(sy))
    stop("must pass y and z", call. = FALSE)
  
  if(is.null(coord.system)){
    e <- .set_x_axis(e, 0)
    e <- .set_y_axis(e, deparse(substitute(y)), 0)
  } else {
    e$x$opts$xAxis <- NULL # remove
    e$x$opts$yAxis <- NULL # remove
  }
  
  if(missing(color))
    data <- .build_data(
      e, 
      e$x$mapping$x, 
      deparse(substitute(y)), 
      deparse(substitute(sx)), 
      deparse(substitute(sy))
    )
  else 
    data <- .build_data(
      e, 
      e$x$mapping$x, 
      deparse(substitute(y)), 
      deparse(substitute(sx)), 
      deparse(substitute(sy)),
      deparse(substitute(color))
    )
  
  serie <- list(
    type = "flowGL",
    data = data,
    ...
  )
  
  if(!is.null(name))
    serie$name <- name
  
  if(!is.null(coord.system))
    serie$coordinateSystem <- coord.system
  
  e$x$opts$series <- append(e$x$opts$series, list(serie))
  
  e
}