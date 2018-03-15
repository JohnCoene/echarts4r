#' Choropleth
#' 
#' Draw a choropleth map.
#' 
#' @inheritParams e_bar
#' 
#' @inheritParams e_bar
#' @param serie Values to plot.
#' @param type Map type.
#' 
#' @examples 
#' \dontrun{
#' choropleth <- data.frame(
#'   countries = c("France", "Brazil", "China", "Russia", "Canada", "India"),
#'   values = round(runif(6, 10, 25))
#' )
#' 
#' choropleth %>% 
#'   e_charts(countries) %>% 
#'   e_choropleth(values)
#' }
#' 
#' @export
e_choropleth <- function(e, serie, type = "world", name = NULL, ...){
  if(missing(e))
    stop("must pass e", call. = FALSE)
  
  if(missing(serie))
    stop("must pass serie", call. = FALSE)
  
  if(is.null(name)) # defaults to column name
    name <- deparse(substitute(serie))
  
  e$x$opts$xAxis <- NULL # remove
  e$x$opts$yAxis <- NULL # remove
  
  # build JSON data
  data <- .build_pie(e, deparse(substitute(serie)))
  
  serie <- list(
    name = name,
    type = "map",
    mapType = type,
    data = data,
    ...
  )
  
  e$x$opts$series <- append(e$x$opts$series, list(serie))
  e
}