#' Bar & Line chart
#' 
#' Add bar or line serie.
#' 
#' @param e An \code{echarts4} object as returned by \code{e_charts}.
#' @param serie Column name of serie to plot.
#' @param name name of the serie.
#' @param ... Any other option to pass to \code{bar} or \code{line} char types.
#' 
#' @examples 
#' USArrests %>% 
#'   e_charts(Assault) %>% 
#'   e_bar(Murder, name = "Euwww") %>% 
#'   e_line(Rape)
#' 
#' @rdname barline
#' @export
e_bar <- function(e, serie, name = NULL, ...){
  
  if(missing(e))
    stop("must pass e", call. = FALSE)

  if(missing(serie))
    stop("must pass serie", call. = FALSE)

  if(is.null(name)) # defaults to column name
    name <- deparse(substitute(serie))

  # build JSON data
  vector <- .build_vector(e$x$data, dplyr::enquo(serie))

  serie <- list(
    name = name,
    type = "bar",
    data = vector,
    ...
  )

  e$x$opts$legend$data <- append(e$x$opts$legend$data, list(name))

  e$x$opts$series <- append(e$x$opts$series, list(serie))
  e
}

#' @rdname barline
#' @export
e_line <- function(e, serie, name = NULL, ...){
  # SEE e_bar
  if(missing(serie))
    stop("must pass serie", call. = FALSE)
  
  if(is.null(name))
    name <- deparse(substitute(serie))
  
  vector <- .build_vector(e$x$data, dplyr::enquo(serie))
  
  serie <- list(
    name = name,
    type = "line",
    data = vector,
    ...
  )
  
  e$x$opts$legend$data <- append(e$x$opts$legend$data, list(name))
  
  e$x$opts$series <- append(e$x$opts$series, list(serie))
  e
}

#' Scatter
#' 
#' Add a scatter serie.
#' 
#' @inheritParams e_bar
#' @param size Column name containing size of points.
#' @param scale Scale for \code{size}, defaults to \code{* 1} which multiplies the size 
#' by \code{1} (equivalent to no multiplier).
#' 
#' @examples 
#' USArrests %>% 
#'   e_charts(Assault) %>% 
#'   e_scatter(Murder, Rape)
#' 
#' @export
e_scatter <- function(e, serie, size, scale = "* 1", name = NULL, ...){
  
  if(missing(serie))
    stop("must pass serie", call. = FALSE)
  
  if(is.null(name))
    name <- deparse(substitute(serie))
  
  x <- e$x$opts$xAxis$data # extract x
  e$x$opts$xAxis$data <- NULL # remove
  
  if(!missing(size))
    xy <- .build_xy(e$x$data, x, dplyr::enquo(serie), dplyr::enquo(size))
  else
    xy <- .build_xy(e$x$data, x, dplyr::enquo(serie))
  
  serie <- list(
    name = name,
    type = "scatter",
    data = xy,
    ...
  )
  
  if(!missing(size))
    serie$symbolSize <- htmlwidgets::JS(
      paste0("function(data){
          return data[2] ", scale, ";
      }")
    )
  
  e$x$opts$legend$data <- append(e$x$opts$legend$data, list(name))
  
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
  
  data <- .build_candle(
    e$x$data, 
    dplyr::enquo(opening), 
    dplyr::enquo(closing), 
    dplyr::enquo(low), 
    dplyr::enquo(high)
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
#' @inheritParams e_bar
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
  funnel <- .build_funnel(e$x$data, dplyr::enquo(values), dplyr::enquo(labels))
  
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
    dplyr::enquo(source), 
    dplyr::enquo(target)
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
#' @param ... Any other parameter.
#' 
#' @examples 
#' nodes <- data.frame(
#'   name = LETTERS,
#'   value = rnorm(26),
#'   size = rnorm(26) * 15,
#'   grp = rep(c("grp1", "grp2"), 13),
#'   stringsAsFactors = FALSE
#' )
#' 
#' edges <- data.frame(
#'   source = sample(LETTERS, 40, replace = TRUE),
#'   target = sample(LETTERS, 40, replace = TRUE),
#'   stringsAsFactors = FALSE
#' )
#' 
#' e_charts() %>% 
#'   e_graph() %>% 
#'   e_graph_nodes(nodes, name, value, size, grp) %>% 
#'   e_graph_edges(edges, source, target)
#' 
#' @rdname graph
#' @export
e_graph <- function(e, name = NULL, ...){
  if(missing(e))
    stop("must pass e", call. = FALSE)
  
  e$x$opts$xAxis <- NULL # remove
  e$x$opts$yAxis <- NULL # remove
  
  serie <- list(
    name = name,
    type = "graph",
    layout = "force",
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
    e$x$opts$series[[length(e$x$opts$series)]]$categories <- .build_graph_category(nodes, enquo(category))
    #e$x$opts$legend$data <- .graph_cat_legend(nodes, enquo(category))
  }
  
  value <- enquo(value)
  symbolSize <- enquo(size)
  names <- enquo(names)
  
  nodes <- .build_graph_nodes(
    nodes, 
    names, 
    value,
    symbolSize,
    enquo(category)
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
  
  source <- enquo(source)
  target <- enquo(target)
  
  data <- .build_graph_edges(
    edges, 
    source, 
    target
  )
  
  # build JSON data
  e$x$opts$series[[length(e$x$opts$series)]]$links <- data
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
  e$x$opts$legend$data <- append(e$x$opts$legend$data, list(name))
  
  e
}

#' Heatmap
#' 
#' Draw heatmap by coordinates.
#' 
#' @inheritParams e_bar
#' @param x,y,z Coordinates and values.
#' 
#' @examples 
#' v <- LETTERS[1:10]
#' matrix <- data.frame(
#'   x = sample(v, 300, replace = TRUE), 
#'   y = sample(v, 300, replace = TRUE), 
#'   z = rnorm(300, 10, 1)
#' ) %>% 
#'   dplyr::group_by(x, y) %>% 
#'   dplyr::summarise(z = sum(z)) %>% 
#'   dplyr::ungroup()
#' 
#' matrix %>% 
#'   e_charts() %>% 
#'   e_heatmap(x, y, z) %>% 
#'   e_visual_map()
#' 
#' @export
e_heatmap <- function(e, x, y, z, name = NULL, ...){
  if(missing(x) || missing(y) || missing(z))
    stop("must pass x, y and z", call. = FALSE)
  
  # build JSON data
  xyz <- .build_3d(e$x$data, dplyr::enquo(x), dplyr::enquo(y), dplyr::enquo(z))
  
  serie <- list(
    name = name,
    type = "heatmap",
    data = xyz,
    ...
  )
  
  e$x$opts$xAxis <- list(
    data = unique(
      .build_vector(e$x$data, dplyr::enquo(x)
    ))
  )
  
  e$x$opts$yAxis <- list(
    data = unique(
      .build_vector(e$x$data, dplyr::enquo(y)
    ))
  )
  
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
#' @param label Labels of slices.
#' 
#' @examples 
#' mtcars %>% 
#'   head() %>% 
#'   dplyr::mutate(model = row.names(.)) %>% 
#'   e_charts() %>% 
#'   e_pie(carb, model)
#' 
#' @export
e_pie <- function(e, serie, label, name = NULL, ...){
  if(missing(e))
    stop("must pass e", call. = FALSE)
  
  if(missing(serie) || missing(label))
    stop("must pass serie and label", call. = FALSE)
  
  e$x$opts$xAxis <- NULL # remove
  e$x$opts$yAxis <- NULL # remove
  
  if(is.null(name)) # defaults to column name
    name <- deparse(substitute(serie))
  
  # build JSON data
  data <- .build_pie(e$x$data, dplyr::enquo(serie), dplyr::enquo(label))
  
  serie <- list(
    name = name,
    type = "pie",
    data = data,
    ...
  )
  
  e$x$opts$legend$data <- append(e$x$opts$legend$data, .graph_cat_legend(e$x$data, dplyr::enquo(label)))
  
  e$x$opts$series <- append(e$x$opts$series, list(serie))
  e
}

#' Tree
#' 
#' Build a treemap.
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
#'   e_treemap(parent, child)
#' 
#' @export
e_treemap <- function(e, parent, child, ...){
  if(missing(e))
    stop("must pass e", call. = FALSE)
  
  if(missing(parent) || missing(child))
    stop("must pass parent and child", call. = FALSE)
  
  e$x$opts$xAxis <- NULL # remove
  e$x$opts$yAxis <- NULL # remove
  
  # build JSON data
  data <- .build_tree(
    e$x$data, 
    dplyr::enquo(parent), 
    dplyr::enquo(child)
  )
  
  serie <- list(
    type = "tree",
    data = list(data),
    ...
  )
  
  e$x$opts$series <- append(e$x$opts$series, list(serie))
  e
}