#' Globe
#' 
#' @inheritParams e_bar
#' @param lon,lat Coordinates.
#' @param value Bar height.
#' 
#' @examples 
#' \dontrun{
#' url <- paste0("https://ecomfe.github.io/echarts-examples/",
#'               "public/data-gl/asset/data/population.json")
#' data <- jsonlite::fromJSON(url)
#' data <- as.data.frame(data)
#' names(data) <- c("lon", "lat", "value")
#' 
#' data %>% 
#'   e_charts() %>% 
#'   e_globe_bar(lon, lat, value)
#' }
#' 
#' @export
e_globe_bar <- function(e, lon, lat, value, name = NULL, ...){
  if(missing(e))
    stop("must pass e", call. = FALSE)
  
  if(missing(value) || missing(lon) || missing(lat))
    stop("must pass coordinates and value", call. = FALSE)
  
  if(is.null(name)) # defaults to column name
    name <- deparse(substitute(value))
  
  if(!length(e$x$opts$globe))
    e$x$opts$globe <- list(
      baseTexture = e_map_texture(),
      heightTexture = e_map_texture(),
      environment = e_stars_texture()
    )
  
  e$x$opts$xAxis <- NULL # remove
  e$x$opts$yAxis <- NULL # remove
  
  # build JSON data
  data <- .build_globexyz(e, deparse(substitute(lon)), deparse(substitute(lat)), deparse(substitute(value)))
  
  serie <- list(
    name = name,
    type = "bar3D",
    coordinateSystem = "globe",
    data = data,
    ...
  )
  
  e$x$opts$series <- append(e$x$opts$series, list(serie))
  
  e
}