#' Bar & Line chart
#' 
#' Add bar or line serie.
#' 
#' @param e An \code{echarts4} object as returned by \code{e_charts}.
#' @param serie Column name of serie to plot on y axis.
#' @param name name of the serie.
#' @param ... Any other option to pass to \code{bar} or \code{line} char types.
#' 
#' @examples 
#' USArrests %>% 
#'   dplyr::mutate(
#'     state = row.names(.)
#'   ) %>% 
#'   e_charts(state) %>% 
#'   e_bar(Murder, name = "Euwww") %>% 
#'   e_line(Rape)
#' 
#' @rdname barline
#' @export
e_bar <- function(e, serie, name = NULL, ...){

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