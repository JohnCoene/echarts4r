#' Highlight & Downplay
#' 
#' Highlight series
#' 
#' @inheritParams e_bar
#' @param series.index Index of serie to highlight or \code{list}/\code{vector} of series.
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
  if(!is.null(btn)) opts$id <- btn
  
  data <- list(type = "highlight")
  if(!is.null(series.index)) data$seriesIndex <- as.list(series.index)
  if(!is.null(series.name)) data$seriesName <- as.list(series.name)
  opts$data <- data
  
  e$x$events <- append(e$x$events, list(opts))
  
  return(e)
}

#' @name highlight_action
#' @export
e_downplay <- function(e, series.index = NULL, series.name = NULL, btn = NULL){
  
  if(is.null(series.index) && is.null(series.name))
    stop("must set index or name", call. = FALSE)
  
  opts <- list()
  if(!is.null(btn)) opts$id <- btn
  
  data <- list(type = "downplay")
  if(!is.null(series.index)) data$seriesIndex <- as.list(series.index)
  if(!is.null(series.name)) data$seriesName <- as.list(series.name)
  opts$data <- data
  
  e$x$events <- append(e$x$events, list(opts))
  
  return(e)
}