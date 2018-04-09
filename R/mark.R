#' Mark point
#' 
#' Mark points and lines.
#' 
#' @inheritParams e_bar
#' @param serie Serie to mark on passed to \code{\link{grep}}, defaults to last added.
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
#' USArrests %>% 
#'   e_charts(Murder) %>% 
#'   e_line(Rape) %>% 
#'   e_line(UrbanPop) %>% 
#'   e_mark_point(data = max) %>% 
#'   e_mark_point(data = min) %>% 
#'   e_mark_line(serie = "Rape", data = avg)
#' 
#' @seealso \href{Additional point arguments}{https://ecomfe.github.io/echarts-doc/public/en/option.html#series-line.markPoint},
#' \href{Additional line arguments}{https://ecomfe.github.io/echarts-doc/public/en/option.html#series-line.markLine}
#' 
#' @rdname mark
#' @export
e_mark_point <- function(e, serie = NULL, data = NULL, ...){
  
  if(missing(e))
    stop("must pass e", call. = FALSE)
  
  if(is.null(serie))
    index <- length(e$x$opts$series)
  else 
    index <- .get_index(e, serie)
  
  point <- list(...)
  
  if(!is.null(data))
    point$data <- list(data)
  
  if(is.null(e$x$opts$series[[index]]$markPoint))
    e$x$opts$series[[index]]$markPoint <- append(e$x$opts$series[[index]]$markPoint, point)
  else
    e$x$opts$series[[index]]$markPoint$data <- append(e$x$opts$series[[index]]$markPoint$data, list(data))
  
  e
}

#' @rdname mark
#' @export
e_mark_line <- function(e, serie = NULL, data = NULL, ...){
  
  if(missing(e))
    stop("must pass e", call. = FALSE)
  
  if(is.null(serie))
    index <- length(e$x$opts$series)
  else 
    index <- .get_index(e, serie)
  
  point <- list(...)
  
  if(!is.null(data))
    point$data <- list(data)
  
  if(is.null(e$x$opts$series[[index]]$markLine))
    e$x$opts$series[[index]]$markLine <- append(e$x$opts$series[[index]]$markLine, point)
  else
    e$x$opts$series[[index]]$markLine$data <- append(e$x$opts$series[[index]]$markLine$data, list(data))
  
  e
}