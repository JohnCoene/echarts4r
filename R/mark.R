#' Mark point
#' 
#' Mark a point on the plot.
#' 
#' @inheritParams e_bar
#' @param serie Serie to mark point on, defaults to last added.
#' @param data Point placement.
#' 
#' @examples 
#' max <- list(
#'   name = "Max",
#'   type = "max"
#' )
#' 
#' min <- list(
#'   name = "Min",
#'   type = "min"
#' )
#' 
#' USArrests %>% 
#'   e_charts(Murder) %>% 
#'   e_bar(Rape) %>% 
#'   e_mark_point(data = max) %>% 
#'   e_mark_point(data = min)
#' 
#' @export
e_mark_point <- function(e, serie = NULL, data = NULL, ...){
  
  if(is.null(serie))
    index <- length(e$x$opts$series)
  else 
    index <- grep(serie, e$x$opts$series)
  
  point <- list(
    data = list(data),
    ...
  )
  
  if(is.null(e$x$opts$series[[index]]$markPoint))
    e$x$opts$series[[index]]$markPoint <- append(e$x$opts$series[[index]]$markPoint, point)
  else
    e$x$opts$series[[index]]$markPoint$data <- append(e$x$opts$series[[index]]$markPoint$data, list(data))
  
  
  e
}