#' Bar and Line chart
#' 
#' Add bar serie.
#' 
#' @param e An \code{echarts4r} object as returned by \code{\link{e_charts}}.
#' @param serie Column name of serie to plot.
#' @param bind Binding between datasets, namely for use of \code{\link{e_brush}}.
#' @param name name of the serie.
#' @param ... Any other option to pass to \code{bar} or \code{line} char types.
#' @param x.index,y.index Indexes of x and y axis.
#' 
#' @examples 
#' USArrests %>% 
#'   dplyr::mutate(
#'     State = row.names(.),
#'     Rape = -Rape
#'   ) %>% 
#'   e_charts(State) %>% 
#'   e_bar(Murder) %>% 
#'   e_bar(Rape, name = "Sick basterd", x.index = 1) # secondary x axis
#' 
#' @noRd
#' @keywords internal
e_bar2 <- function(e, serie, bind, name = NULL, y.index = 0, x.index = 0, ...){
  
  if(missing(e))
    stop("must pass e", call. = FALSE)
  
  if(missing(serie))
    stop("must pass serie", call. = FALSE)
  
  serie <- deparse(substitute(serie))
  
  if(is.null(name)) # defaults to column name
    name <- serie
  
  if(y.index != 0)
    e <- .set_y_axis(e, serie, y.index)
  
  if(x.index != 0)
    e <- .set_x_axis(e, x.index)
  
  # build JSON data
  .build_data2(e, e$x$mapping$x, serie) -> vector
  
  if(!missing(bind))
    vector <- .add_bind(e, vector, deparse(substitute(bind)))
  
  serie <- list(
    name = name,
    type = "bar",
    data = vector,
    yAxisIndex = y.index,
    xAxisIndex = x.index,
    ...
  )
  
  e$x$opts$legend$data <- append(e$x$opts$legend$data, list(name))
  
  e$x$opts$series <- append(e$x$opts$series, list(serie))
  e
}

.build_data2 <- function(e, ...){
  e$x$data %>% 
    dplyr::select_(...) -> data
  
  apply(unname(data), 1, function(x){
    list(value = unlist(x))
  }) 
  
}