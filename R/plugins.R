#' Wordcloud
#' 
#' Draw a wordcloud.
#' 
#' @inheritParams e_bar
#' @param word,freq Terms and their frequencies.
#' @param color Word color.
#' @param rm.x,rm.y Whether to remove x and y axis, defaults to \code{TRUE}.
#' 
#' @examples 
#' words <- function(n = 5000) {
#'   a <- do.call(paste0, replicate(5, sample(LETTERS, n, TRUE), FALSE))
#'   paste0(a, sprintf("%04d", sample(9999, n, TRUE)), sample(LETTERS, n, TRUE))
#' }
#' 
#' tf <- data.frame(terms = words(100), 
#'   freq = rnorm(100, 55, 10)) %>% 
#'   dplyr::arrange(-freq)
#' 
#' tf %>% 
#'   e_color_range(freq, color) %>% 
#'   e_charts() %>% 
#'   e_cloud(terms, freq, color, shape = "circle", sizeRange = c(3, 15))
#' 
#' @seealso \href{official documentation}{https://github.com/ecomfe/echarts-wordcloud}
#' 
#' @export
e_cloud <- function(e, word, freq, color, rm.x = TRUE, rm.y = TRUE, ...){
  
  if(missing(e))
    stop("missing e", call. = FALSE)
  
  e <- .rm_axis(e, rm.x, "x")
  e <- .rm_axis(e, rm.y, "y")
  
  data <- .build_data(e, deparse(substitute(freq)))
  data <- .add_bind(e, data, deparse(substitute(word)))
  
  if(!missing(color)){
    color <- .get_data(e, deparse(substitute(color)))
    for(i in 1:length(data)){
      col <- list(
        normal = list(
          color = color[i]
        )
      )
      data[[i]]$textStyle <- col
    }
  }
  
  serie <- list(
    type = "wordCloud",
    data = data,
    ...
  )
  
  e$x$opts$series <- append(e$x$opts$series, list(serie))
  
  e
  
}

#' Liquid fill
#' 
#' Draw liquid fill.
#' 
#' @inheritParams e_bar
#' @param color Color to plot.
#' @param rm.x,rm.y Whether to remove x and y axis, defaults to \code{TRUE}.
#' 
#' @examples 
#' df <- data.frame(val = c(0.6, 0.5, 0.4))
#' 
#' df %>% 
#'   e_charts() %>% 
#'   e_liquid(val) %>% 
#'   e_theme("dark")
#' 
#' @seealso \href{official documentation}{https://github.com/ecomfe/echarts-liquidfill}
#' 
#' @export
e_liquid <- function(e, serie, color, rm.x = TRUE, rm.y = TRUE, ...){
  if(missing(e))
    stop("missing e", call. = FALSE)
  
  e <- .rm_axis(e, rm.x, "x")
  e <- .rm_axis(e, rm.y, "y")
  
  data <- .get_data(e, deparse(substitute(serie))) %>% 
    unlist() %>% 
    unname()
  
  serie <- list(
    type = "liquidFill",
    data = data,
    ...
  )
  
  if(!missing(color))
    serie$color <- .build_data(e, deparse(substitute(color)))
  
  e$x$opts$series <- append(e$x$opts$series, list(serie))
  
  e
}

#' Modularity
#' 
#' Graph modularity extension will do community detection and partian a graph's vertices in several subsets. 
#' Each subset will be assigned a different color.
#' 
#' @inheritParams e_bar
#' @param modularity Either set to \code{TRUE}, or a \code{list}.
#' 
#' @section Modularity:
#' \itemize{
#'   \item{\code{resolution} Resolution}
#'   \item{\code{sort} Whether to sort to comunities}
#' }
#' 
#' @examples 
#' nodes <- data.frame(
#'   name = paste0(LETTERS, 1:100),
#'   value = rnorm(100, 10, 2),
#'   stringsAsFactors = FALSE
#' )
#' 
#' edges <- data.frame(
#'   source = sample(nodes$name, 200, replace = TRUE),
#'   target = sample(nodes$name, 200, replace = TRUE),
#'   stringsAsFactors = FALSE
#' )
#' 
#' e_charts() %>% 
#'   e_graph() %>% 
#'   e_graph_nodes(nodes, name, value) %>% 
#'   e_graph_edges(edges, source, target) %>% 
#'   e_modularity(
#'     list(
#'       resolution = 5,
#'       sort = TRUE
#'     )
#'   )
#'   
#' @note Does not work in RStudio viewer, open in browser.
#' 
#' @seealso \href{https://github.com/ecomfe/echarts-graph-modularity}{Official documentation}
#' 
#' @export
e_modularity <- function(e, modularity = TRUE){
  
  clu <- list(
    modularity = modularity
  )
  
  e$x$opts$series[[length(e$x$opts$series)]]$modularity <- clu
  e
}