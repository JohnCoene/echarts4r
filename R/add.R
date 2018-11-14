#' Bar and Line chart
#' 
#' Add bar serie.
#' 
#' @param e An \code{echarts4r} object as returned by \code{\link{e_charts}}.
#' @param serie Column name of serie to plot.
#' @param bind Binding between datasets, namely for use of \code{\link{e_brush}}.
#' @param name name of the serie.
#' @param legend Whether to add serie to legend.
#' @param ... Any other option to pass, check See Also section.
#' @param x_index,y_index Indexes of x and y axis.
#' @param coord_system Coordinate system to plot against.
#' 
#' @examples 
#' iris %>% 
#'   group_by(Species) %>% 
#'   e_charts(Sepal.Length) %>% 
#'   e_line(Sepal.Width)
#' 
#' @seealso \href{https://ecomfe.github.io/echarts-doc/public/en/option.html#series-bar}{Additional arguments}
#' 
#' @rdname e_bar
#' @export
e_bar <- function(e, serie, bind, name = NULL, legend = TRUE, y_index = 0, x_index = 0, coord_system = "cartesian2d", ...){
  
  if(missing(e))
    stop("must pass e", call. = FALSE)
  
  if(missing(serie))
    stop("must pass serie", call. = FALSE)
  
  if(missing(bind))
    bd <- NULL
  else
    bd <- deparse(substitute(bind))
  
  sr <- deparse(substitute(serie))
  
  e_bar_(e, sr, bd, name, legend, y_index, x_index, coord_system, ...)
}

#' Line 
#' 
#' Add line serie.
#' 
#' @inheritParams e_bar
#' @param coord_system Coordinate system to plot against.
#' 
#' @examples 
#' iris %>% 
#'   group_by(Species) %>% 
#'   e_charts(Sepal.Length) %>% 
#'   e_line(Sepal.Width) %>% 
#'   e_tooltip(trigger = "axis")
#' 
#' @seealso \href{https://ecomfe.github.io/echarts-doc/public/en/option.html#series-line}{Additional arguments}  
#' 
#' @rdname e_line
#' @export
e_line <- function(e, serie, bind, name = NULL, legend = TRUE, y_index = 0, x_index = 0, 
                   coord_system = "cartesian2d", ...){
  if(missing(e))
    stop("must pass e", call. = FALSE)
  
  if(missing(serie))
    stop("must pass serie", call. = FALSE)
  
  serie <- deparse(substitute(serie))
  
  if(missing(bind))
    bd <- NULL
  else
    bd <- deparse(substitute(bind))
  
  e_line_(e, serie, bd, name, legend, y_index, x_index, coord_system, ...)
}

#' Area 
#' 
#' Add area serie.
#' 
#' @inheritParams e_bar
#' 
#' @examples 
#' CO2 %>% 
#'   group_by(Plant) %>% 
#'   e_charts(conc) %>% 
#'   e_area(uptake) %>% 
#'   e_tooltip(trigger = "axis")
#' 
#' @seealso \href{https://ecomfe.github.io/echarts-doc/public/en/option.html#series-line}{Additional arguments}
#' 
#' @rdname e_area
#' @export
e_area <- function(e, serie, bind, name = NULL, legend = TRUE, y_index = 0, x_index = 0, ...){
  
  if(missing(e))
    stop("must pass e", call. = FALSE)
  
  if(missing(serie))
    stop("must pass serie", call. = FALSE)
  
  serie <- deparse(substitute(serie))
  
  if(missing(bind))
    bd <- NULL
  else
    bd <- deparse(substitute(bind))
  
  e_area_(e, serie, bd, name, legend, y_index, x_index, ...)
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
#' @seealso \href{https://ecomfe.github.io/echarts-doc/public/en/option.html#series-line}{Additional arguments}
#' 
#' @rdname e_step
#' @export
e_step <- function(e, serie, bind, step = c("start", "middle", "end"), fill = FALSE, 
                   name = NULL, legend = TRUE, y_index = 0, x_index = 0, ...){
  if(missing(e))
    stop("must pass e", call. = FALSE)
  
  if(missing(serie))
    stop("must pass serie", call. = FALSE)
  
  serie <- deparse(substitute(serie))
  
  if(missing(bind))
    bd <- NULL
  else
    bd <- deparse(substitute(bind))
  
  e_step_(e, serie, bd, step, fill, name, legend, y_index, x_index, ...)
}

#' Scatter
#' 
#' Add scatter serie.
#' 
#' @inheritParams e_bar
#' @param size Column name containing size of points.
#' @param symbol_size Size of points, either an integer or a vector of length 2, 
#' if \code{size} is \emph{not} \code{NULL} or missing it is applied as a multiplier to \code{scale}. 
#' @param scale A function that takes a vector of \code{numeric} and returns a vector of \code{numeric}
#' of the same length.
#' @param coord_system Coordinate system to plot against, see examples.
#' @param rm_x,rm_y Whether to remove x and y axis, only applies if \code{coord_system} is not 
#' set to \code{cartesian2d}.
#' @param x A vector of integers or numeric.
#' 
#' @section Scaling function: defaults to \code{e_scale} which is a basic function that rescales \code{size}
#' between 1 and 20 for that makes for decent sized points on the chart.
#' 
#' @examples 
#' # scaling
#' e_scale(c(1, 1000))
#' 
#' mtcars %>% 
#'   e_charts(mpg) %>% 
#'   e_scatter(wt, qsec)
#'   
#' my_scale <- function(x) scales::rescale(x, to = c(2, 50))
#'   
#' echart <- mtcars %>% 
#'   e_charts(mpg) %>% 
#'   e_scatter(wt, qsec, scale = my_scale)
#'   
#' echart
#' 
#' # rescale color too
#' echart %>% 
#'   e_visual_map(wt, scale = my_scale)
#'   
#' # or
#' echart %>% 
#'   e_visual_map(min = 2, max = 50)
#' 
#' # applications
#' USArrests %>% 
#'   e_charts(Assault) %>% 
#'   e_scatter(Murder, Rape) %>% 
#'   e_effect_scatter(Rape, Murder, y_index = 1) %>% 
#'   e_grid(index = c(0, 1)) %>% 
#'   e_tooltip()
#'
#' iris %>% 
#'   e_charts_("Sepal.Length") %>% 
#'   e_scatter_(
#'     "Sepal.Width", 
#'     symbol_size = c(8, 2), 
#'     symbol = "rect"
#'   ) %>% 
#'   e_x_axis(min = 4)
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
#'   e_scatter(lat, mag, coord_system = "geo") %>% 
#'   e_visual_map(min = 4, max = 6.5)
#'   
#' @seealso \href{https://ecomfe.github.io/echarts-doc/public/en/option.html#series-scatter}{Additional arguments scatter},
#'  \href{https://ecomfe.github.io/echarts-doc/public/en/option.html#series-effectScatter}{Additional arguments for effect scatter}
#' 
#' @rdname scatter
#' @export
e_scatter <- function(e, serie, size, bind, symbol_size = 1, scale = e_scale, name = NULL, 
                      coord_system = "cartesian2d", legend = TRUE, y_index = 0, 
                      x_index = 0, rm_x = TRUE, rm_y = TRUE, ...){
  
  if(missing(serie))
    stop("must pass serie", call. = FALSE)
  
  serie <- deparse(substitute(serie))
  
  if(missing(size))
    size <- NULL
  else
    size <- deparse(substitute(size))
  
  if(missing(bind))
    bd <- NULL
  else
    bd <- deparse(substitute(bind))
  
  e_scatter_(e = e, serie = serie, size = size, bind = bd, symbol_size = symbol_size, 
             scale = scale, name = name, coord_system = coord_system, 
             legend = legend, y_index = y_index, x_index = x_index, rm_x = rm_x, 
             rm_y = rm_y, ...)
 
}

#' @rdname scatter
#' @export
e_effect_scatter <- function(e, serie, size, bind, symbol_size = 1, scale = e_scale, name = NULL, 
                             coord_system = "cartesian2d", legend = TRUE, 
                             y_index = 0, x_index = 0, rm_x = TRUE, rm_y = TRUE, ...){
  
  if(missing(serie))
    stop("must pass serie", call. = FALSE)
  
  serie <- deparse(substitute(serie))
  
  if(missing(size))
    size <- NULL
  else
    size <- deparse(substitute(size))
  
  if(missing(bind))
    bd <- NULL
  else
    bd <- deparse(substitute(bind))
  
  e_effect_scatter_(e, serie = serie, size = size, bind = bd, 
                    symbol_size = symbol_size, scale = scale, name = name, 
                    coord_system, legend, 
                    y_index, x_index, rm_x, rm_y, ...)
}

#' Candlestick
#' 
#' Add a candlestick chart.
#' 
#' @inheritParams e_bar
#' @param opening,closing,low,high Stock prices.
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
#'   e_candle(opening, closing, low, high) %>% 
#'   e_y_axis(min = 190, max = 220)
#'   
#' @seealso \href{https://ecomfe.github.io/echarts-doc/public/en/option.html#series-candlestick}{Additional arguments}
#' 
#' @rdname e_candle
#' @export
e_candle <- function(e, opening, closing, low, high, bind, name = NULL, legend = TRUE, ...){
  
  if(missing(opening) || missing(closing) || missing(low) || missing(high))
    stop("missing inputs", call. = FALSE)
  
  if(!missing(bind))
    bind <- deparse(substitute(bind))
  else 
    bind <- NULL
  
  e_candle_(e, deparse(substitute(opening)), 
            deparse(substitute(closing)), 
            deparse(substitute(low)), 
            deparse(substitute(high)), 
            bind, name, legend, ...)
}

#' Radar
#' 
#' Add a radar chart
#' 
#' @inheritParams e_bar
#' @param max Maximum value.
#' @param rm_x,rm_y Whether to remove x and y axis, defaults to \code{TRUE}.
#' 
#' @examples
#' df <- data.frame(
#'   x = LETTERS[1:5],
#'   y = runif(5, 1, 5),
#'   z = runif(5, 3, 7)
#' )
#' 
#' df %>% 
#'   e_charts(x) %>%  
#'   e_radar(y, max = 7) %>% 
#'   e_radar(z) %>% 
#'   e_tooltip(trigger = "item")
#' 
#' @rdname e_radar
#' @export
e_radar <- function(e, serie, max = 100, name = NULL, legend = TRUE, 
                    rm_x = TRUE, rm_y = TRUE, ...){
  
  r.index = 0
  
  if(missing(e))
    stop("must pass e", call. = FALSE)
  
  if(missing(serie))
    stop("must pass serie", call. = FALSE)
  
  serie <- deparse(substitute(serie))
  
  e_radar_(e, serie, max, name, legend, rm_x, rm_y, ...)
}

#' Funnel
#' 
#' Add a funnel.
#' 
#' @param e An \code{echarts4r} object as returned by \code{\link{e_charts}}.
#' @param name name of the serie.
#' @param legend Whether to add serie to legend.
#' @param ... Any other option to pass to \code{bar} or \code{line} char types.
#' @param values,labels Values and labels of funnel.
#' @param rm_x,rm_y Whether to remove x and y axis, defaults to \code{TRUE}.
#' 
#' @details No \code{bind} argument here, with a funnel \code{bind} = \code{labels}.
#' 
#' @examples 
#' funnel <- data.frame(stage = c("View", "Click", "Purchase"), value = c(80, 30, 20))
#' 
#' funnel %>% 
#'   e_charts() %>% 
#'   e_funnel(value, stage)
#' 
#' @seealso \href{https://ecomfe.github.io/echarts-doc/public/en/option.html#series-funnel}{Additional arguments}
#' 
#' @rdname e_funnel
#' @export
e_funnel <- function(e, values, labels, name = NULL, legend = TRUE, rm_x = TRUE, rm_y = TRUE, ...){
  
  if(missing(values) || missing(labels))
    stop("missing values or labels", call. = FALSE)
  
  e_funnel_(e = e, 
            values = deparse(substitute(values)), 
            labels = deparse(substitute(labels)), 
            name = name, 
            legend = legend, 
            rm_x = rm_x, 
            rm_y = rm_y, 
            ...)
  
}

#' Sankey
#' 
#' Draw a sankey diagram.
#' 
#' @inheritParams e_bar
#' @param layout Layout of sankey.
#' @param source,target Source and target columns.
#' @param value Value change from \code{source} to \code{target}.
#' @param rm_x,rm_y Whether to remove the x and y axis, defaults to \code{TRUE}.
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
#' @seealso \href{https://ecomfe.github.io/echarts-doc/public/en/option.html#series-sankey}{Additional arguments}
#' 
#' @rdname e_sankey
#' @export
e_sankey <- function(e, source, target, value, layout = "none", rm_x = TRUE, rm_y = TRUE, ...){
  
  if(missing(source) || missing(target) || missing(value))
    stop("missing source, target or values", call. = FALSE)
  
  e <- .rm_axis(e, rm_x, "x")
  e <- .rm_axis(e, rm_y, "y")
  
  e_sankey_(e, deparse(substitute(source)), deparse(substitute(target)), 
            deparse(substitute(value)), layout, rm_x, rm_y, ...)
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
#' @param legend Whether to add serie to legend.
#' @param category Group of nodes (i.e.: group membership).
#' @param edges Data.frame of edges.
#' @param source,target Column names of source and target.
#' @param layout Layout, one of \code{force}, \code{none} or \code{circular}.
#' @param rm_x,rm_y Whether to remove the x and y axis, defaults to \code{TRUE}.
#' @param ... Any other parameter.
#' 
#' @examples 
#' value <- rnorm(10, 10, 2)
#' 
#' nodes <- data.frame(
#'   name = sample(LETTERS, 10),
#'   value = value,
#'   size = value,
#'   grp = rep(c("grp1", "grp2"), 5),
#'   stringsAsFactors = FALSE
#' )
#' 
#' edges <- data.frame(
#'   source = sample(nodes$name, 20, replace = TRUE),
#'   target = sample(nodes$name, 20, replace = TRUE),
#'   stringsAsFactors = FALSE
#' )
#' 
#' e_charts() %>% 
#'   e_graph() %>% 
#'   e_graph_nodes(nodes, name, value, size, grp) %>% 
#'   e_graph_edges(edges, source, target)
#' 
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
#' @seealso \href{Additional arguments}{https://ecomfe.github.io/echarts-doc/public/en/option.html#series-graph},
#'  \code{\link{e_modularity}}
#' 
#' @rdname graph
#' @export
e_graph <- function(e, layout = "force", name = NULL, rm_x = TRUE, rm_y = TRUE, ...){
  if(missing(e))
    stop("must pass e", call. = FALSE)
  
  e <- .rm_axis(e, rm_x, "x")
  e <- .rm_axis(e, rm_y, "y")
  
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
e_graph_gl <- function(e, layout = "force", name = NULL, rm_x = TRUE, rm_y = TRUE, ...){
  if(missing(e))
    stop("must pass e", call. = FALSE)
  
  e <- .rm_axis(e, rm_x, "x")
  e <- .rm_axis(e, rm_y, "y")
  
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
e_graph_nodes <- function(e, nodes, names, value, size, category, legend = TRUE){
  
  if(missing(nodes) || missing(names) || missing(value))
    stop("missing arguments", call. = FALSE)
  
  value <- dplyr::enquo(value)
  symbolSize <- dplyr::enquo(size)
  names <- dplyr::enquo(names)
  
  if(!missing(category) && !missing(size)){
    
    e$x$opts$series[[length(e$x$opts$series)]]$categories <- .build_graph_category(nodes, dplyr::enquo(category))
    
    if(isTRUE(legend))
      e$x$opts$legend$data <- append(e$x$opts$legend$data, unique(nodes[[deparse(substitute(category))]]))
    
    nodes <- .build_graph_nodes(
      nodes, 
      names, 
      value,
      symbolSize,
      dplyr::enquo(category)
    )
  } else if(missing(category) && !missing(size)) {
    nodes <- .build_graph_nodes_no_cat(
      nodes, 
      names, 
      value,
      symbolSize
    )
  } else if (missing(category) && missing(size)){
    nodes <- .build_graph_nodes_no_size(
      nodes, 
      names, 
      value
    )
  }
  
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
#' @param coord_system Coordinate system to plot against, takes 
#' \code{cartesian2d}, \code{geo} or \code{calendar}.
#' @param rm_x,rm_y Whether to remove x and y axis, only applies if \code{coord_system} is not 
#' set to \code{cartesian2d}.
#' @param calendar The index of the calendar to plot against.
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
#'   e_heatmap(y, z, itemStyle = list(emphasis = list(shadowBlur = 10))) %>% 
#'   e_visual_map(z)
#'
#' # calendar   
#' dates <- seq.Date(as.Date("2017-01-01"), as.Date("2018-12-31"), by = "day")
#' values <- rnorm(length(dates), 20, 6)
#' 
#' year <- data.frame(date = dates, values = values)
#' 
#' year %>% 
#'   e_charts(date) %>% 
#'   e_calendar(range = "2018") %>% 
#'   e_heatmap(values, coord_system = "calendar") %>% 
#'   e_visual_map(max = 30)
#'   
#' # multiple years
#' year %>% 
#'   dplyr::mutate(year = format(date, "%Y")) %>% 
#'   group_by(year) %>% 
#'   e_charts(date) %>% 
#'   e_calendar(range = "2017", top = 40) %>% 
#'   e_calendar(range = "2018", top = 260) %>% 
#'   e_heatmap(values, coord_system = "calendar") %>% 
#'   e_visual_map(max = 30)
#' 
#' @seealso \href{https://ecomfe.github.io/echarts-doc/public/en/option.html#series-heatmap}{Additional arguments}
#' 
#' @rdname e_heatmap
#' @export
e_heatmap <- function(e, y, z, name = NULL, coord_system = "cartesian2d", rm_x = TRUE, rm_y = TRUE, 
                      calendar = NULL, ...){
  if(missing(y))
    stop("must pass y", call. = FALSE)
  
  if(!missing(z))
    z <- deparse(substitute(z))
  else
    z <- NULL
  
  e_heatmap_(e, deparse(substitute(y)), z, name, coord_system, rm_x, rm_y, calendar, ...)
}

#' Parallel
#' 
#' Draw parallel coordinates.
#' 
#' @inheritParams e_bar
#' @param rm_x,rm_y Whether to remove x and y axis, defaults to \code{TRUE}.
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
#' @seealso \href{https://ecomfe.github.io/echarts-doc/public/en/option.html#series-parallel}{Additional arguments}
#' 
#' @rdname e_parallel
#' @export
e_parallel <- function(e, ..., name = NULL, rm_x = TRUE, rm_y = TRUE){
  if(missing(e))
    stop("must pass e", call. = FALSE) 
  
  e <- .rm_axis(e, rm_x, "x")
  e <- .rm_axis(e, rm_y, "y")
  
  e$x$data[[1]] %>% 
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
#' @param rm_x,rm_y Whether to remove x and y axis, defaults to \code{TRUE}.
#' 
#' @examples 
#' mtcars %>% 
#'   head() %>% 
#'   dplyr::mutate(model = row.names(.)) %>% 
#'   e_charts(model) %>% 
#'   e_pie(carb)
#'   
#' @seealso \href{https://ecomfe.github.io/echarts-doc/public/en/option.html#series-pie}{Additional arguments}
#' 
#' @rdname e_pie
#' @export
e_pie <- function(e, serie, name = NULL, legend = TRUE, rm_x = TRUE, rm_y = TRUE, ...){
  if(missing(e))
    stop("must pass e", call. = FALSE)
  
  if(missing(serie))
    stop("must pass serie", call. = FALSE)
  
  e_pie_(e, deparse(substitute(serie)), name, legend, rm_x, rm_y, ...)
}

#' Sunburst
#' 
#' Build a sunburst.
#' 
#' @inheritParams e_bar
#' @param parent,child Edges.
#' @param value Name of column containing values.
#' @param itemStyle Name of column containing styles to pass to \code{child}, 
#' expects a \code{data.frame} or a \code{list}, see details.
#' @param rm_x,rm_y Whether to remove x and y axis, defaults to \code{TRUE}.
#' 
#' @details The \code{itemStyle} argument essentially is a nested data.frame with column names such as
#' \code{color}, or \code{borderColor} as specified in the 
#' \href{https://ecomfe.github.io/echarts-doc/public/en/option.html#series-sunburst.data.itemStyle}{official documentation}.
#' 
#' @examples 
#' df <- data.frame(
#'   parent = c("earth", "earth", "earth", "mars", "mars"), 
#'   child = c("forest", "ocean", "iceberg", "elon", "curiosity"),
#'   value = ceiling(rnorm(5, 10, 2))
#' )
#' 
#' df %>% 
#'   e_charts() %>% 
#'   e_sunburst(parent, child, value) %>% 
#'   e_theme("westeros")
#'   
#' # with itemStyle
#' colors <- c("red", "black", "blue")
#' 
#' df$color <- sample(colors, 5, replace = TRUE)
#' df$borderColor <- sample(colors, 5, replace = TRUE)
#' 
#' df %>% 
#'   tidyr::nest(color, borderColor, .key = "style") %>% # nest
#'   e_charts() %>% 
#'   e_sunburst(parent, child, value, style) 
#' 
#' @seealso \href{https://ecomfe.github.io/echarts-doc/public/en/option.html#series-sunburst}{Additional arguments}
#' 
#' @rdname e_sunburst
#' @export
e_sunburst <- function(e, parent, child, value, itemStyle, rm_x = TRUE, rm_y = TRUE, ...){
  if(missing(e))
    stop("must pass e", call. = FALSE)
  
  if(!missing(itemStyle))
    style <- deparse(substitute(itemStyle))
  else
    style <- NULL
  
  e_sunburst_(e, deparse(substitute(parent)), 
              deparse(substitute(child)), deparse(substitute(value)), 
              style, rm_x, rm_y, ...)
}

#' Treemap
#' 
#' Build a treemap.
#' 
#' @inheritParams e_bar
#' @param parent,child Edges.
#' @param value Value of edges.
#' @param rm_x,rm_y Whether to remove x and y axis, defaults to \code{TRUE}.
#' 
#' @examples 
#' df <- data.frame(
#'   parent = c("earth", "earth", "earth", "mars", "mars"), 
#'   child = c("forest", "ocean", "iceberg", "elon", "curiosity"),
#'   value = ceiling(rnorm(5, 10, 2))
#' )
#' 
#' df %>% 
#'   e_charts() %>% 
#'   e_treemap(parent, child, value)
#'   
#' @seealso \href{https://ecomfe.github.io/echarts-doc/public/en/option.html#series-treemap}{Additional arguments}
#' 
#' @rdname e_treemap
#' @export
e_treemap <- function(e, parent, child, value, rm_x = TRUE, rm_y = TRUE, ...){
  if(missing(e))
    stop("must pass e", call. = FALSE)
  
  if(missing(parent) || missing(child) || missing(value))
    stop("must pass parent, child and value", call. = FALSE)
  
  e_treemap_(e, deparse(substitute(parent)), 
              deparse(substitute(child)), deparse(substitute(value)), 
              rm_x, rm_y, ...)
}

#' River
#' 
#' Build a theme river.
#' 
#' @inheritParams e_bar
#' @param rm_x,rm_y Whether to remove x and y axis, defaults to \code{TRUE}.
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
#' @seealso \href{https://ecomfe.github.io/echarts-doc/public/en/option.html#series-themeRiver}{Additional arguments}
#' 
#' @rdname e_river
#' @export
e_river <- function(e, serie, name = NULL, legend = TRUE, rm_x = TRUE, rm_y = TRUE, ...){
  if(missing(e))
    stop("must pass e", call. = FALSE)
  
  if(missing(serie))
    stop("must pass serie", call. = FALSE)
  
  e_river_(e, deparse(substitute(serie)), name, legend, rm_x, rm_y, ...)
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
#' @seealso \href{https://ecomfe.github.io/echarts-doc/public/en/option.html#series-boxplot}{Additional arguments}
#' 
#' @rdname e_boxplot
#' @export
e_boxplot <- function(e, serie, name = NULL, outliers = TRUE, ...){
  
  if(missing(serie))
    stop("must pass serie", call. = FALSE)
  
  if(is.null(name)) # defaults to column name
    name <- deparse(substitute(serie))
  
  e_boxplot_(e, deparse(substitute(serie)), name, outliers, ...)
}

#' Tree
#' 
#' Build a tree.
#' 
#' @inheritParams e_bar
#' @param parent,child Edges.
#' @param rm_x,rm_y Whether to remove x and y axis, defaults to \code{TRUE}.
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
#' @seealso \href{https://ecomfe.github.io/echarts-doc/public/en/option.html#series-tree}{Additional arguments}
#' 
#' @rdname e_tree
#' @export
e_tree <- function(e, parent, child, rm_x = TRUE, rm_y = TRUE, ...){
  if(missing(e))
    stop("must pass e", call. = FALSE)
  
  if(missing(parent) || missing(child))
    stop("must pass parent and child", call. = FALSE)
  
  e_tree_(e, deparse(substitute(parent)), deparse(substitute(child)), rm_x, rm_y, ...)
}

#' Gauge
#' 
#' Plot a gauge.
#' 
#' @inheritParams e_bar
#' @param value Value to gauge.
#' @param name Text on gauge.
#' @param rm_x,rm_y Whether to remove x and y axis, defaults to \code{TRUE}.
#' 
#' @examples 
#' e_charts() %>% 
#'   e_gauge(57, "PERCENT")
#' 
#' @seealso \href{https://ecomfe.github.io/echarts-doc/public/en/option.html#series-gauge}{Additional arguments}
#' 
#' @rdname e_gauge
#' @export
e_gauge <- function(e, value, name, rm_x = TRUE, rm_y = TRUE, ...){
  
  if(missing(e) || missing(value) || missing(name))
    stop("missing e, name, or value", call. = FALSE)
  
  if(!inherits(value, "numeric"))
    stop("must pass numeric or interger", call. = FALSE)
  
  # remove axis
  e <- .rm_axis(e, rm_x, "x")
  e <- .rm_axis(e, rm_y, "y")
  
  e$x$opts$series <- list(
    list(
      type = "gauge",
      data = list(list(value = value, name = name))
    )
  )
  e
}

#' @rdname e_gauge
#' @export
e_gauge_ <- e_gauge

#' Lines 3D
#' 
#' Add 3D lines.
#' 
#' @inheritParams e_bar
#' @param coord_system Coordinate system to use, such as \code{cartesian3D}, or \code{globe}.
#' @param y,z Coordinates of lines.
#' @param source_lon,source_lat,target_lon,target_lat coordinates.
#' @param rm_x,rm_y Whether to remove x and y axis, defaults to \code{TRUE}.
#' 
#' @examples 
#' # get data
#' flights <- read.csv(
#'   paste0("https://raw.githubusercontent.com/plotly/datasets/",
#'          "master/2011_february_aa_flight_paths.csv")
#' )
#' 
#' # Lines 3D
#' # Globe
#' flights %>% 
#'   e_charts() %>% 
#'   e_globe(
#'     displacementScale = 0.05
#'   ) %>% 
#'   e_lines_3d(
#'     start_lon, 
#'     start_lat, 
#'     end_lon, 
#'     end_lat,
#'     name = "flights",
#'     effect = list(show = TRUE)
#'   ) %>% 
#'   e_legend(FALSE)
#' 
#' # Geo 3D
#' flights %>% 
#'   e_charts() %>% 
#'   e_geo_3d() %>% 
#'   e_lines_3d(
#'     start_lon, 
#'     start_lat, 
#'     end_lon, 
#'     end_lat,
#'     coord_system = "geo3D"
#'   )
#'  
#' # line 3D 
#' df <- data.frame(
#'   x = 1:100,
#'   y = runif(100, 10, 25),
#'   z = rnorm(100, 100, 50)
#' )
#' 
#' df %>% 
#'   e_charts(x) %>% 
#'   e_line_3d(y, z) %>% 
#'   e_visual_map() %>% 
#'   e_title("nonsense")
#' 
#' @seealso \href{http://echarts.baidu.com/option-gl.html#series-lines3D}{Additional arguments for lines 3D},
#'  \href{http://echarts.baidu.com/option-gl.html#series-line3D}{Additional arguments for line 3D}
#' 
#' @rdname line3D
#' @export
e_lines_3d <- function(e, source_lon, source_lat, target_lon, target_lat, name = NULL, 
                       coord_system = "globe", rm_x = TRUE, rm_y = TRUE, ...){
  if(missing(e))
    stop("must pass e", call. = FALSE)
  
  if(missing(source_lat) || missing(source_lon) || missing(target_lat) || missing(target_lon))
    stop("missing coordinates", call. = FALSE)
  
  e_lines_3d_(e, deparse(substitute(source_lon)), deparse(substitute(source_lat)), 
              deparse(substitute(target_lon)), deparse(substitute(target_lat)), 
              name, coord_system, rm_x, rm_y, ...)
}

#' @rdname line3D
#' @export
e_line_3d <- function(e, y, z, name = NULL, coord_system = NULL, rm_x = TRUE, rm_y = TRUE, ...){
  if(missing(e))
    stop("must pass e", call. = FALSE)
  
  if(missing(y) || missing(z))
    stop("missing coordinates", call. = FALSE)
  
  e_line_3d_(e, deparse(substitute(y)), deparse(substitute(z)), 
             name, coord_system, rm_x, rm_y, ...)
}

#' Bar 3D
#' 
#' Add 3D bars
#' 
#' @inheritParams e_bar
#' @param y,z Coordinates.
#' @param bind Binding.
#' @param coord_system Coordinate system to use, one of \code{cartesian3D}, \code{geo3D}, \code{globe}.
#' @param rm_x,rm_y Whether to remove x and y axis, defaults to \code{TRUE}.
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
#'   e_globe() %>% 
#'   e_bar_3d(lat, value, coord_system = "globe") %>% 
#'   e_visual_map()
#'   
#' data %>% 
#'   e_charts(lon) %>% 
#'   e_geo_3d() %>% 
#'   e_bar_3d(lat, value, coord_system = "geo3D") %>% 
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
#' @seealso \href{http://echarts.baidu.com/option-gl.html#series-bar3D}{Additional arguments}
#' 
#' @rdname e_bar_3d
#' @export
e_bar_3d <- function(e, y, z, bind, coord_system = "cartesian3D", name = NULL, 
                     rm_x = TRUE, rm_y = TRUE, ...){
  if(missing(e))
    stop("must pass e", call. = FALSE)
  
  if(missing(y) || missing(z))
    stop("must pass y and z", call. = FALSE)
  
  if(missing(bind))
    bd <- NULL
  else
    bd <- deparse(substitute(bind))
  
  e_bar_3d_(e, deparse(substitute(y)), deparse(substitute(z)), bd, 
            coord_system, name, rm_x, rm_y, ...)
}

#' Lines
#' 
#' Add lines.
#' 
#' @inheritParams e_bar
#' @param source_lon,source_lat,target_lon,target_lat coordinates.
#' @param coord_system Coordinate system to use, one of \code{geo}, or \code{cartesian2d}.
#' @param rm_x,rm_y Whether to remove x and y axis, defaults to \code{TRUE}.
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
#' @seealso \href{https://ecomfe.github.io/echarts-doc/public/en/option.html#series-lines}{Additional arguments}
#' 
#' @rdname e_lines
#' @export
e_lines <- function(e, source_lon, source_lat, target_lon, target_lat, coord_system = "geo", name = NULL, 
                    rm_x = TRUE, rm_y = TRUE, ...){
  if(missing(e))
    stop("must pass e", call. = FALSE)
  
  if(missing(source_lat) || missing(source_lon) || missing(target_lat) || missing(target_lon))
    stop("missing coordinates", call. = FALSE)
  
  e_lines_(e, deparse(substitute(source_lon)), deparse(substitute(source_lat)), 
           deparse(substitute(target_lon)), deparse(substitute(target_lat)), 
           coord_system, name, rm_x, rm_y, ...)
}

#' Scatter 3D
#' 
#' Add 3D scatter.
#' 
#' @inheritParams e_bar
#' @param y,z Coordinates.
#' @param bind Binding.
#' @param color,size Color and Size of bubbles.
#' @param coord_system Coordinate system to use, one of \code{geo3D}, \code{globe}, or \code{cartesian3D}.
#' @param rm_x,rm_y Whether to remove x and y axis, defaults to \code{TRUE}.
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
#'     globeOuterRadius = 100
#'   ) %>% 
#'   e_scatter_3d(lat, cnt, coord_system = "globe", blendMode = 'lighter') %>% 
#'   e_visual_map(inRange = list(symbolSize = c(1, 10)))
#' 
#' @seealso \href{http://echarts.baidu.com/option-gl.html#series-scatter3D}{Additional arguments}
#' 
#' @rdname e_scatter_3d
#' @export
e_scatter_3d <- function(e, y, z, color, size, bind, coord_system = "cartesian3D", name = NULL, 
                         rm_x = TRUE, rm_y = TRUE, legend = FALSE, ...){
  if(missing(e))
    stop("must pass e", call. = FALSE)
  
  if(missing(y) || missing(z))
    stop("must pass y and z", call. = FALSE)
  
  if(!missing(color))
    colour <- deparse(substitute(color))
  else
    colour <- NULL
  
  if(!missing(size))
    sz <- deparse(substitute(size))
  else
    sz <- NULL
  
  if(!missing(bind))
    bd <- deparse(substitute(bind))
  else
    bd <- NULL
  
  e_scatter_3d_(e, deparse(substitute(y)), deparse(substitute(z)), colour, sz, bd, coord_system, name, 
                rm_x, rm_y, legend, ...)
}

#' Flow GL
#' 
#' @inheritParams e_bar
#' @param y Vector position on the y axis.
#' @param sx,sy Velocity in respective axis.
#' @param color Vector color.
#' @param coord_system Coordinate system to use.
#' @param rm_x,rm_y Whether to remove x and y axis, only applies if \code{coord_system} is not \code{null}.
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
#'     coord_system = "geo", 
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
#' @seealso \href{http://echarts.baidu.com/option-gl.html#series-flowGL}{Additional arguments}
#' 
#' @rdname e_flow_gl
#' @export
e_flow_gl <- function(e, y, sx, sy, color, name = NULL, coord_system = NULL, rm_x = TRUE, rm_y = TRUE, ...){
  if(missing(e))
    stop("must pass e", call. = FALSE)
  
  if(missing(y) || missing(sx) || missing(sy))
    stop("must pass y and z", call. = FALSE)
  
  if(!missing(color))
    colour <- deparse(substitute(color))
  else
    colour <- NULL
  
  e_flow_gl_(e, deparse(substitute(y)), deparse(substitute(sx)), deparse(substitute(sy)), colour, 
             name, coord_system, rm_x, rm_y, ...)
}

#' Scatter GL
#' 
#' Draw scatter GL.
#' 
#' @inheritParams e_bar
#' @param y,z Column names containing y and z data.
#' @param coord_system Coordinate system to plot against.
#' @param rm_x,rm_y Whether to remove x and y axis, defaults to \code{TRUE}.
#' 
#' @examples 
#' quakes %>% 
#'   e_charts(long) %>% 
#'   e_geo(
#'     roam = TRUE,
#'     boundingCoords = list(
#'       c(185, - 10),
#'       c(165, -40)
#'      )
#'   ) %>% 
#'   e_scatter_gl(lat, depth)
#' 
#' @seealso \href{http://echarts.baidu.com/option-gl.html#series-scatterGL}{Additional arguments}
#' 
#' @rdname e_scatter_gl
#' @export
e_scatter_gl <- function(e, y, z, name = NULL, coord_system = "geo", rm_x = TRUE, rm_y = TRUE, ...){
  if(missing(e))
    stop("must pass e", call. = FALSE)
  
  if(missing(y) || missing(z))
    stop("must pass y and z", call. = FALSE)
  
  e_scatter_gl_(e, deparse(substitute(y)), deparse(substitute(z)), name, coord_system, rm_x, rm_y, ...)
}

#' Pictorial
#' 
#' Pictorial bar chart is a type of bar chart that custimzed glyph 
#' (like images, SVG PathData) can be used instead of rectangular bar.
#' 
#' @inheritParams e_bar
#' @param symbol Symbol to plot.
#' 
#' @section Symbols:
#' \itemize{
#'   \item{Built-in}{ \code{circle}, \code{rect}, \code{roundRect}, \code{triangle}, \code{diamond}, 
#'   \code{pin}, \code{arrow}.}
#'   \item{SVG Path}
#'   \item{Images}{ Path to image, don't forget to precede it with \code{image://}, see examples.}
#' }
#' 
#' @examples 
#' # built-in symbols
#' y <- rnorm(10, 10, 2)
#' df <- data.frame(
#'   x = 1:10,
#'   y = y,
#'   z = y - rnorm(10, 5, 1)
#' )
#' 
#' df %>% 
#'   e_charts(x) %>% 
#'   e_bar(z, barWidth = 10) %>% 
#'   e_pictorial(y, symbol = "rect", symbolRepeat = TRUE, z = -1,
#'     symbolSize = c(10, 4)) %>% 
#'   e_theme("westeros")
#'   
#' # svg path
#' path <- "path://M0,10 L10,10 C5.5,10 5.5,5 5,0 C4.5,5 4.5,10 0,10 z"
#' 
#' style <- list(
#'   normal = list(opacity = 0.5), # normal
#'   emphasis = list(opacity = 1) # on hover
#' )
#' 
#' df %>% 
#'   e_charts(x) %>% 
#'   e_pictorial(y, symbol = path, barCategoryGap = "-130%",
#'     itemStyle = style) 
#'     
#' # image
#' # might not work in RStudio viewer
#' # open in browser
#' qomo <- paste0(
#'   "https://ecomfe.github.io/echarts-examples/public/",
#'   "data/asset/img/hill-Qomolangma.png"
#' )
#' 
#' kili <- paste0(
#'   "https://ecomfe.github.io/echarts-examples/public/", 
#'   "data/asset/img/hill-Kilimanjaro.png"
#' )
#' 
#' data <- data.frame(
#'   x = c("Qomolangma", "Kilimanjaro"), 
#'   value = c(8844, 5895),
#'   symbol = c(paste0("image://", qomo),
#'     paste0("image://", kili))
#' )
#' 
#' data %>% 
#'   e_charts(x) %>% 
#'   e_pictorial(value, symbol) %>% 
#'   e_legend(FALSE) 
#' 
#' @seealso \href{https://ecomfe.github.io/echarts-doc/public/en/option.html#series-pictorialBar}{Additional arguments}
#' 
#' @rdname e_pictorial
#' @export
e_pictorial <- function(e, serie, symbol, bind, name = NULL, legend = TRUE, y_index = 0, x_index = 0, ...){
  
  if(missing(e))
    stop("must pass e", call. = FALSE)
   
  if(missing(serie) || missing(symbol))
    stop("must pass serie and symbol", call. = FALSE)
  
  if(!missing(bind))
    bd <- deparse(substitute(bind))
  else
    bd <- NULL
  
  # only deparse if it is a column name
  if(deparse(substitute(symbol)) %in% colnames(e$x$data[[1]]))
    symbol <- deparse(substitute(symbol))
  
  e_pictorial_(e, deparse(substitute(serie)), symbol, bd, name, legend, y_index, x_index, ...)
}

#' Smooth
#' 
#' Plot formulas.
#' 
#' @inheritParams e_bar
#' @param formula formula to pass to \code{\link{lm}}.
#' @param symbol Symbol to use in \code{\link{e_line}}.
#' @param smooth Whether to smooth the line.
#' @param ... Additional arguments to pass to \code{\link{e_line}}.
#' 
#' @examples 
#' mtcars %>% 
#'   e_charts(mpg) %>% 
#'   e_scatter(qsec, symbol_size = 10) %>% 
#'   e_lm(qsec ~ mpg, name = "y = ax + b")
#'   
#' mtcars %>% 
#'   e_charts(disp) %>% 
#'   e_scatter(mpg, qsec) %>% 
#'   e_loess(mpg ~ disp)
#'   
#' CO2 %>% 
#'   e_charts(conc) %>% 
#'   e_scatter(uptake, symbol_size = 10) %>% 
#'   e_glm(uptake ~ conc, name = "GLM")
#' 
#' @rdname smooth
#' @export
e_lm <- function(e, formula, name = NULL, legend = TRUE, symbol = "none", smooth = TRUE, ...){
  form <- as.formula(formula)
  model <- eval(
    lm(form, data = e$x$data[[1]])
  )
  
  e <- .build_model(e, model, name, symbol, smooth, ...)
  
  if(isTRUE(legend))
    e$x$opts$legend$data <- append(e$x$opts$legend$data, list(name))
  e 
}

#' @rdname smooth
#' @export
e_glm <- function(e, formula, name = NULL, legend = TRUE, symbol = "none", smooth = TRUE, ...){
  form <- as.formula(formula)
  model <- eval(
    glm(form, data = e$x$data[[1]])
  )
  
  e <- .build_model(e, model, name, symbol, smooth, ...)
  
  if(isTRUE(legend))
    e$x$opts$legend$data <- append(e$x$opts$legend$data, list(name))
  e 
}

#' @rdname smooth
#' @export
e_loess <- function(e, formula, name = NULL, legend = TRUE, symbol = "none", smooth = TRUE, 
                    x_index = 0, y_index = 0, ...){
  mod <- eval(
    loess(as.formula(formula), data = e$x$data[[1]])
  )
  
  if(is.null(name))
    name <- "ECHARTS4RLOESS"
  
  e$x$data[[1]][,name] <- predict(mod)
  
  vector <- .build_data(
    e, 
    e$x$mapping$x,
    name
  )

  l <- list(
    name = name,
    type = "line",
    data = vector,
    ...
  )
  
  if(y_index != 0)
    e <- .set_y_axis(e, name, y_index)
  
  if(x_index != 0)
    e <- .set_x_axis(e, x_index)
  
  l$yAxisIndex <- y_index
  l$xAxisIndex <- x_index
  
  if(isTRUE(legend) && !is.null(name))
    e$x$opts$legend$data <- append(e$x$opts$legend$data, list(name))
  
  e$x$opts$series <- append(e$x$opts$series, list(l))
  e
  
  e
}

#' Histogram & Density
#' 
#' Add a histogram or density plots.
#' 
#' @inheritParams e_bar
#' @param bar.width Width of bars.
#' @param breaks Passed to \code{\link{hist}}.
#' 
#' @examples 
#' mtcars %>% 
#'   e_charts() %>% 
#'   e_histogram(mpg, name = "histogram") %>% 
#'   e_density(mpg, areaStyle = list(opacity = .4), smooth = TRUE, name = "density", y_index = 1) %>% 
#'   e_tooltip(trigger = "axis")
#' 
#' @rdname histogram
#' @export
e_histogram <- function(e, serie, breaks = "Sturges", name = NULL, legend = TRUE,
                        bar.width = "99%", x_index = 0, y_index = 0, ...){
  
  if(missing(e))
    stop("must pass e", call. = FALSE)
  
  if(missing(serie))
    stop("must pass serie", call. = FALSE)
  
  e_histogram_(e, deparse(substitute(serie)), breaks, name, legend, bar.width, x_index, y_index, ...)
}

#' @rdname histogram
#' @export
e_density <- function(e, serie, breaks = "Sturges", name = NULL, legend = TRUE, 
                      x_index = 0, y_index = 0, ...){
  if(missing(e))
    stop("must pass e", call. = FALSE)
  
  if(missing(serie))
    stop("must pass serie", call. = FALSE)
  
  e_density_(e, deparse(substitute(serie)), breaks, name, legend, x_index, y_index, ...)
}

