#' Mark point
#' 
#' Mark points and lines.
#' 
#' @inheritParams e_bar
#' @param serie Serie or vector of series to mark on, defaults to all series.
#' @param data Placement.
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
#' avg <- list(
#'   type = "average",
#'   name = "AVG"
#' )
#' 
#' mtcars %>% 
#'   e_charts(mpg) %>% 
#'   e_line(wt) %>% 
#'   e_line(drat) %>% 
#'   e_line(cyl) %>% 
#'   e_mark_point("wt", data = max) %>% 
#'   e_mark_point(c("cyl", "drat"), data = min) %>% 
#'   e_mark_line(data = avg) %>% # applies to all
#'   e_mark_area(serie = "wt", data = list(
#'     list(xAxis = "min", yAxis = "min"), 
#'     list(xAxis = "max", yAxis = "max"))
#'  )
#' 
#' @seealso \href{https://echarts.apache.org/en/option.html#series-line.markPoint}{Additional point arguments},
#' \href{https://echarts.apache.org/en/option.html#series-line.markLine}{Additional line arguments}
#' 
#' @rdname mark
#' @export
e_mark_point <- function(e, serie = NULL, data = NULL, ...){
  
  if(missing(e))
    stop("must pass e", call. = FALSE)
  
  if(is.null(serie))
    index <- 1:length(e$x$opts$series)
  else 
    index <- .get_index(e, serie)
  
  for(i in index){
    point <- list(...)
    
    if(!is.null(data))
      point$data <- list(data)
    
    if(is.null(e$x$opts$series[[i]]$markPoint))
      e$x$opts$series[[i]]$markPoint <- append(e$x$opts$series[[i]]$markPoint, point)
    else
      e$x$opts$series[[i]]$markPoint$data <- append(e$x$opts$series[[i]]$markPoint$data, list(data))
  }
  
  e
}

#' @rdname mark
#' @export
e_mark_line <- function(e, serie = NULL, data = NULL, ...){
  
  if(missing(e))
    stop("must pass e", call. = FALSE)
  
  if(is.null(serie))
    index <- 1:length(e$x$opts$series)
  else 
    index <- .get_index(e, serie)
  
  for(i in index){
    point <- list(...)
    
    if(!is.null(data))
      point$data <- list(data)
    
    if(is.null(e$x$opts$series[[i]]$markLine))
      e$x$opts$series[[i]]$markLine <- append(e$x$opts$series[[i]]$markLine, point)
    else
      e$x$opts$series[[i]]$markLine$data <- append(e$x$opts$series[[i]]$markLine$data, list(data))
  }
  
  e
}

#' @rdname mark
#' @export
e_mark_area <- function(e, serie = NULL, data = NULL, ...){
  
  if(missing(e))
    stop("must pass e", call. = FALSE)
  
  if(is.null(serie))
    index <- 1:length(e$x$opts$series)
  else 
    index <- .get_index(e, serie)
  
  for(i in index){
    point <- list(...)
    
    if(!is.null(data))
      point$data <- list(data)
    
    if(is.null(e$x$opts$series[[i]]$markArea))
      e$x$opts$series[[i]]$markArea <- append(e$x$opts$series[[i]]$markArea, point)
    else
      e$x$opts$series[[i]]$markArea$data <- append(e$x$opts$series[[i]]$markArea$data, list(data))
  }
  
  e
}