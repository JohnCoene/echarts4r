#' Choropleth
#' 
#' Draw a choropleth map.
#' 
#' @inheritParams e_bar
#' 
#' @inheritParams e_bar
#' @param serie Values to plot.
#' @param region Names of regions/countries or states.
#' 
#' @examples 
#' choropleth <- data.frame(
#'   countries = c("France", "Brazil", "China", "Russia", "Canada", "India"),
#'   values = round(runif(6, 10, 25))
#' )
#' 
#' choropleth %>% 
#'   e_charts() %>% 
#'   e_choropleth(values, countries)
#' 
#' @export
e_choropleth <- function(e, serie, region, type = "world", name = NULL, ...){
  if(missing(e))
    stop("must pass e", call. = FALSE)
  
  if(missing(serie) || missing(region))
    stop("must pass serie and region", call. = FALSE)
  
  if(is.null(name)) # defaults to column name
    name <- deparse(substitute(serie))
  
  e$x$opts$xAxis <- NULL # remove
  e$x$opts$yAxis <- NULL # remove
  
  if(is.null(name)) # defaults to column name
    name <- deparse(substitute(serie))
  
  # build JSON data
  data <- .build_pie(e$x$data, dplyr::enquo(serie), dplyr::enquo(region))
  
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