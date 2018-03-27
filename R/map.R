#' Choropleth
#' 
#' Draw maps.
#' 
#' @inheritParams e_bar
#' @param serie Values to plot.
#' @param map Map type.
#' 
#' @examples 
#' \dontrun{
#' choropleth <- data.frame(
#'   countries = c("France", "Brazil", "China", "Russia", "Canada", "India", "United States",
#'                 "Argentina", "Australia"),
#'   values = round(runif(9, 10, 25))
#' )
#' 
#' choropleth %>% 
#'   e_charts(countries) %>% 
#'   e_choropleth(values) %>% 
#'   e_visual_map(min = 10, max = 25)
#' 
#' choropleth %>% 
#'   e_charts(countries) %>% 
#'   e_map_3d(values, shading = "lambert") %>% 
#'   e_visual_map(min = 10, max = 30)
#' }
#' 
#' @seealso \code{\link{e_country_names}}
#' 
#' @rdname map
#' @export
e_choropleth <- function(e, serie, map = "world", name = NULL, ...){
  
  if(missing(e))
    stop("must pass e", call. = FALSE)
  
  if(missing(serie))
    stop("must pass serie", call. = FALSE)
  
  if(is.null(name)) # defaults to column name
    name <- deparse(substitute(serie))
  
  e$x$opts$xAxis <- NULL # remove
  e$x$opts$yAxis <- NULL # remove
  
  # build JSON data
  data <- .build_data(e, e$x$mapping$x, deparse(substitute(serie)), names = c("name", "value"))
  
  serie <- list(
    name = name,
    type = "map",
    mapType = map,
    data = data,
    ...
  )
  
  e$x$opts$series <- append(e$x$opts$series, list(serie))
  
  e
}

#' @rdname map
#' @export
e_map_3d <- function(e, serie, map = "world", name = NULL, ...){
  if(missing(e))
    stop("must pass e", call. = FALSE)
  
  if(missing(serie))
    stop("must pass serie", call. = FALSE)
  
  if(is.null(name)) # defaults to column name
    name <- deparse(substitute(serie))
  
  e$x$opts$xAxis <- NULL # remove
  e$x$opts$yAxis <- NULL # remove
  
  # build JSON data
  data <- .build_data(e, e$x$mapping$x, deparse(substitute(serie)), names = c("name", "value"))
  
  serie <- list(
    name = name,
    type = "map3D",
    map = map,
    data = data,
    ...
  )
  
  e$x$opts$series <- append(e$x$opts$series, list(serie))
  
  e
}
