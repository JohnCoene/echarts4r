#' Add nested data
#' 
#' Utility function to add data where the original JavaScript library expects nested data.
#' 
#' @inheritParams e_bar
#' @param param The nested parameter to add data to.
#' 
#' @details For instance, \code{\link{e_funnel}} lets you pass \code{values} and \code{labels} 
#' (from your initial data.frame) which corresponds to \code{name} and \code{value} in the 
#' \href{https://echarts.apache.org/en/option.html#series-heatmap.data}{original library}. 
#' However the latter also takes, \code{label}, \code{itemStyle}, and \code{emphasis} but being JSON arrays 
#' they translate to lists in R and dealing with nested data.frames is not ideal. \code{e_add} remedies to that. 
#' It allows adding those nested data points, see the examples below.
#' 
#' @examples 
#' # funnel can take nested itemStyle
#' # https://echarts.apache.org/en/option.html#series-funnel.data
#' funnel <- data.frame(
#'   stage = c("View", "Click", "Purchase"), 
#'   value = c(80, 30, 20),
#'   color = c("blue", "red", "green")
#' )
#' 
#' funnel %>% 
#'   e_charts() %>% 
#'   e_funnel(value, stage) %>% 
#'   e_add("itemStyle", color)
#' 
#' # Heatmap can take nested label
#' # https://echarts.apache.org/en/option.html#series-heatmap.data
#' v <- LETTERS[1:10]
#' matrix <- data.frame(
#'   x = sample(v, 300, replace = TRUE), 
#'   y = sample(v, 300, replace = TRUE), 
#'   z = rnorm(300, 10, 1),
#'   stringsAsFactors = FALSE
#' ) %>% 
#'   dplyr::group_by(x, y) %>% 
#'   dplyr::summarise(z = sum(z)) %>% 
#'   dplyr::ungroup() %>% 
#'   dplyr::mutate(
#'     show = TRUE,
#'     fontStyle = round(runif(dplyr::n(), 5, 12))
#'   )
#' 
#' matrix %>% 
#'   e_charts(x) %>% 
#'   e_heatmap(y, z) %>% 
#'   e_visual_map(z) %>% 
#'   e_add(
#'     "label",
#'     show,
#'     fontStyle
#'   )
#' 
#' @export
e_add <- function(e, param, ...){
  
  if(missing(e) || missing(param))
    stop("missing e or what", call. = FALSE)
  
  for(i in 1:length(e$x$data)){
    
    data <- e$x$data[[i]] %>% 
      dplyr::select(...) 
    
    data <- apply(data, 1, as.list)
    
    for(j in 1:length(data)){
      
      if(!e$x$tl)
        e$x$opts$series[[i]]$data[[j]][[param]] <- data[[j]]
      else
        e$x$opts$options$series[[i]]$data[[j]][[param]] <- data[[j]]
      
    }
  }
  e
}