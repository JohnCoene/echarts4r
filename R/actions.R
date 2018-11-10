#' Highlight & Downplay
#' 
#' Highlight series
#' 
#' @inheritParams e_bar
#' @param series.index,series.name Index or name of serie to highlight or \code{list} or \code{vector} of series.
#' @param btn A \code{\link{e_button}} id.
#' 
#' @examples 
#' iris %>% 
#'   group_by(Species) %>% 
#'   e_charts(Sepal.Length) %>% 
#'   e_line(Sepal.Width) %>% 
#'   e_line(Petal.Length) %>% 
#'   e_highlight(series.name = "setosa") # highlight group
#' 
#' @name highlight_action
#' @export
e_highlight <- function(e, series.index = NULL, series.name = NULL, btn = NULL){
  
  if(is.null(series.index) && is.null(series.name))
    stop("must set index or name", call. = FALSE)
  
  opts <- list()
  
  data <- list(type = "highlight")
  if(!is.null(series.index)) data$seriesIndex <- as.list(series.index)
  if(!is.null(series.name)) data$seriesName <- as.list(series.name)
  opts$data <- data
  
  if(!is.null(btn)){
    if(!btn %in% names(e$x$buttons)){
      e$x$buttons[[btn]] <- list(opts)
    } else {
      e$x$buttons[[btn]] <- append(e$x$buttons[[btn]], list(opts))
    }
  } else {
    e$x$events <- append(e$x$events, list(opts))
  }
  
  return(e)
}

#' @name highlight_action
#' @export
e_downplay <- function(e, series.index = NULL, series.name = NULL, btn = NULL){
  
  if(is.null(series.index) && is.null(series.name))
    stop("must set index or name", call. = FALSE)
  
  opts <- list()
  
  data <- list(type = "downplay")
  if(!is.null(series.index)) data$seriesIndex <- as.list(series.index)
  if(!is.null(series.name)) data$seriesName <- as.list(series.name)
  opts$data <- data
  
  if(!is.null(btn)){
    if(!btn %in% names(e$x$buttons)){
      e$x$buttons[[btn]] <- list(opts)
    } else {
      e$x$buttons[[btn]] <- append(e$x$buttons[[btn]], list(opts))
    }
  } else {
    e$x$events <- append(e$x$events, list(opts))
  }
  
  return(e)
}

#' Show & Hide Tooltip
#' 
#' Show or hide tooltip.
#' 
#' @inheritParams e_bar
#' @param btn A \code{\link{e_button}} id.
#' @param ... Any options, see \href{https://ecomfe.github.io/echarts-doc/public/en/api.html#action.tooltip}{official documentation}
#' 
#' @note The tooltip must be initialised with \code{\link{e_tooltip}} for this to work.
#' 
#' @examples 
#' cars %>% 
#'   e_charts(dist) %>% 
#'   e_scatter(speed) %>% 
#'   e_tooltip() %>% 
#'   e_hidetip(btn = "btn") %>% 
#'   e_button("btn", "Hide tooltip")
#' 
#' @name tooltip_action
#' @export
e_showtip <- function(e, ..., btn = NULL){
  
  opts <- list()
  data <- list(type = "showTip", ...)
  opts$data <- data
  
  if(!is.null(btn)){
    if(!btn %in% names(e$x$buttons)){
      e$x$buttons[[btn]] <- list(opts)
    } else {
      e$x$buttons[[btn]] <- append(e$x$buttons[[btn]], list(opts))
    }
  } else {
    e$x$events <- append(e$x$events, list(opts))
  }
  
  return(e)
}

#' @name tooltip_action
#' @export
e_hidetip <- function(e, ..., btn = NULL){
  
  opts <- list()
  data <- list(type = "hideTip", ...)
  opts$data <- data
  
  if(!is.null(btn)){
    if(!btn %in% names(e$x$buttons)){
      e$x$buttons[[btn]] <- list(opts)
    } else {
      e$x$buttons[[btn]] <- append(e$x$buttons[[btn]], list(opts))
    }
  } else {
    e$x$events <- append(e$x$events, list(opts))
  }
  
  return(e)
}

#' Zoom
#' 
#' Zoom on a region.
#' 
#' @inheritParams e_bar
#' @param btn A \code{\link{e_button}} id.
#' @param ... Any options, see \href{https://ecomfe.github.io/echarts-doc/public/en/api.html#action.dataZoom.dataZoom}{official documentation}
#' 
#' @examples 
#' cars %>% 
#'   e_charts(dist) %>% 
#'   e_scatter(speed) %>% 
#'   e_datazoom() %>% 
#'   e_zoom(
#'     dataZoomIndex = 0,
#'     start = 20,
#'     end = 40,
#'     btn = "BUTTON"
#'   ) %>% 
#'   e_button("BUTTON", "Zoom in")
#' 
#' @export
e_zoom <- function(e, ..., btn = NULL){
  
  opts <- list()
  opts$data <- list(type = "dataZoom", ...)
  
  if(!is.null(btn)){
    if(!btn %in% names(e$x$buttons)){
      e$x$buttons[[btn]] <- list(opts)
    } else {
      e$x$buttons[[btn]] <- append(e$x$buttons[[btn]], list(opts))
    }
  } else {
    e$x$events <- append(e$x$events, list(opts))
  }
  
  return(e)
}

#' Select Visual Map
#' 
#' Selects data range of visual mapping.
#' 
#' @inheritParams e_bar
#' @param btn A \code{\link{e_button}} id.
#' @param ... Any options, see \href{https://ecomfe.github.io/echarts-doc/public/en/api.html#action.visualMap}{official documentation}
#' 
#' @examples 
#' data("state")
#' 
#' as.data.frame(state.x77) %>% 
#'   e_charts(Population) %>% 
#'   e_scatter(Income, Frost, scale = "/5") %>% 
#'   e_visual_map(Frost) %>% 
#'   e_legend(FALSE) %>% 
#'   e_visual_map_range(
#'     selected = list(60, 120)
#'   )
#' 
#' @export
e_visual_map_range <- function(e, ..., btn = NULL){
  opts <- list()
  if(!is.null(btn)) opts$id <- btn
  opts$data <- list(type = "selectDataRange", ...)
  
  if(!is.null(btn)){
    if(!btn %in% names(e$x$buttons)){
      e$x$buttons[[btn]] <- list(opts)
    } else {
      e$x$buttons[[btn]] <- append(e$x$buttons[[btn]], list(opts))
    }
  } else {
    e$x$events <- append(e$x$events, list(opts))
  }
  
  return(e)
}

#' Select & Unselect Pie
#' 
#' Actions related to \code{\link{e_pie}}.
#' 
#' @inheritParams e_bar
#' @param btn A \code{\link{e_button}} id.
#' @param ... Any options, see \href{https://ecomfe.github.io/echarts-doc/public/en/api.html#action.pie}{official documentation}
#' 
#' @examples 
#' mtcars %>% 
#'   head() %>% 
#'   dplyr::mutate(model = row.names(.)) %>% 
#'   e_charts(model) %>% 
#'   e_pie(carb) %>% 
#'   e_pie_select(dataIndex = 0)
#' 
#' @name pie_action
#' @export
e_pie_select <- function(e, ..., btn = NULL){
  opts <- list()
  if(!is.null(btn)) opts$id <- btn
  opts$data <- list(type = "pieSelect", ...)
  
  if(!is.null(btn)){
    if(!btn %in% names(e$x$buttons)){
      e$x$buttons[[btn]] <- list(opts)
    } else {
      e$x$buttons[[btn]] <- append(e$x$buttons[[btn]], list(opts))
    }
  } else {
    e$x$events <- append(e$x$events, list(opts))
  }
  
  return(e)
}

#' @rdname pie_action
#' @export
e_pie_unselect <- function(e, ..., btn = NULL){
  opts <- list()
  if(!is.null(btn)) opts$id <- btn
  opts$data <- list(type = "pieUnSelect", ...)
  
  if(!is.null(btn)){
    if(!btn %in% names(e$x$buttons)){
      e$x$buttons[[btn]] <- list(opts)
    } else {
      e$x$buttons[[btn]] <- append(e$x$buttons[[btn]], list(opts))
    }
  } else {
    e$x$events <- append(e$x$events, list(opts))
  }
  
  return(e)
}

#' Nodes Adjacency
#' 
#' Actions related to \code{\link{e_graph}}.
#' 
#' @inheritParams e_bar
#' @param btn A \code{\link{e_button}} id.
#' @param ... Any options, see \href{https://ecomfe.github.io/echarts-doc/public/en/api.html#action.graph}{official documentation}
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
#'   e_graph_edges(edges, source, target) %>% 
#'   e_focus_adjacency(
#'     seriesIndex = 0,
#'     dataIndex = 4
#'   )
#' 
#' @name graph_action
#' @export
e_focus_adjacency <- function(e, ..., btn = NULL){
  opts <- list()
  if(!is.null(btn)) opts$id <- btn
  opts$data <- list(type = "focusNodeAdjacency", ...)
  
  if(!is.null(btn)){
    if(!btn %in% names(e$x$buttons)){
      e$x$buttons[[btn]] <- list(opts)
    } else {
      e$x$buttons[[btn]] <- append(e$x$buttons[[btn]], list(opts))
    }
  } else {
    e$x$events <- append(e$x$events, list(opts))
  }
  
  return(e)
}

#' @rdname graph_action
#' @export
e_unfocus_adjacency <- function(e, ..., btn = NULL){
  opts <- list()
  if(!is.null(btn)) opts$id <- btn
  opts$data <- list(type = "unfocusNodeAdjacency", ...)
  
  if(!is.null(btn)){
    if(!btn %in% names(e$x$buttons)){
      e$x$buttons[[btn]] <- list(opts)
    } else {
      e$x$buttons[[btn]] <- append(e$x$buttons[[btn]], list(opts))
    }
  } else {
    e$x$events <- append(e$x$events, list(opts))
  }
  
  return(e)
}
