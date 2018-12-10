#' Geo 3D
#' 
#' Initialise geo 3D.
#' 
#' @inheritParams e_bar
#' @param color Color.
#' @param type Map type.
#' @param rm_x,rm_y Whether to remove x and y axis, defaults to \code{TRUE}.
#' 
#' @examples 
#' choropleth <- data.frame(
#'   countries = c("France", "Brazil", "China", "Russia", "Canada", "India", "United States",
#'                 "Argentina", "Australia"),
#'   height = runif(9, 1, 5),
#'   color = c("#F7FBFF", "#DEEBF7", "#C6DBEF", "#9ECAE1", "#6BAED6", "#4292C6", 
#'             "#2171B5", "#08519C", "#08306B")
#' )
#' 
#' choropleth %>% 
#'   e_charts(countries) %>% 
#'   e_geo_3d(height, color)
#'   
#' @seealso \code{\link{e_country_names}}, 
#' \href{http://echarts.baidu.com/option-gl.html#geo3D}{Additional arguments}
#' 
#' @rdname e_geo_3d
#' @export
e_geo_3d <- function(e, serie, color, type = "world", rm_x = TRUE, rm_y = TRUE, ...){
  if(missing(e))
    stop("must pass e", call. = FALSE)
  
  if(!missing(serie))
    sr <- deparse(substitute(serie))
  else
    sr <- NULL
  
  if(!missing(color))
    cl <- deparse(substitute(color))
  else
    cl <- NULL
  
  e_geo_3d_(e, sr, cl, type, rm_x, rm_y, ...)
}

#' @rdname e_geo_3d
#' @export
e_geo_3d_ <- function(e, serie = NULL, color = NULL, type = "world", rm_x = TRUE, rm_y = TRUE, ...){
  
  if(missing(e))
    stop("must pass e", call. = FALSE)
  
  e <- .rm_axis(e, rm_x, "x")
  e <- .rm_axis(e, rm_y, "y")
  
  series <- list(
    map = type,
    ...
  )
  
  if(!is.null(color) && !is.null(serie))
    series$regions <- .build_height(e, serie, color, j = 1)
  else if(is.null(color) && !is.null(serie))
    series$regions <- .build_height(e, serie, j = 1)
  
  if(!e$x$tl)
    e$x$opts$geo3D <- series
  else
    e$x$opts$baseOption$geo3D <- series
  
  e
}

#' Geo
#' 
#' Initialise geo.
#' 
#' @inheritParams e_bar
#' @param map Map type.
#' 
#' @examples 
#' flights <- read.csv(
#'   paste0("https://raw.githubusercontent.com/plotly/datasets/",
#'          "master/2011_february_aa_flight_paths.csv")
#' )
#' 
#' flights %>% 
#'   e_charts() %>% 
#'   e_geo() %>% 
#'   e_lines(
#'     start_lon, 
#'     start_lat, 
#'     end_lon, 
#'     end_lat,
#'     name = "flights",
#'     lineStyle = list(normal = list(curveness = 0.3))
#'    )
#' 
#' @seealso \href{https://ecomfe.github.io/echarts-doc/public/en/option.html#geo}{Additional arguments}
#' 
#' @export
e_geo <- function(e, map = "world", ...){
  
  opts <- list(
    map = map,
    ...
  )
  
  if(!e$x$tl)
    e$x$opts$geo <- opts
  else
    e$x$opts$baseOption$geo <- opts
  e
}