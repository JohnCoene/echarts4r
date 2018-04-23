#' Leaflet
#' 
#' Leaflet extension.
#' 
#' @inheritParams e_bar
#' @param roam Whether to allow the user to roam.
#' @param template \code{urlTemplate}, should not be changed.
#' @param options List of options, including \code{attribution} and \code{label}.
#' 
#' @examples 
#' \dontrun{
#' url <- paste0("https://ecomfe.github.io/echarts-examples/",
#'               "public/data-gl/asset/data/population.json")
#' data <- jsonlite::fromJSON(url)
#' data <- as.data.frame(data)
#' names(data) <- c("lon", "lat", "value")
#' data$value <- log(data$value)
#' 
#' data %>% 
#'   e_charts(lon) %>% 
#'   e_leaflet() %>%
#'   e_leaflet_tile() %>%  
#'   e_scatter(lat, size = value, coord.system = "leaflet") 
#' }
#' 
#' @note Will not render in the RStudio, open in browser.
#' 
#' @rdname leaflet
#' @export
e_leaflet <- function(e, roam = TRUE, ...){
  
  leaf <- list(...)
  leaf$roam <- roam
  leaf$tiles <- list()
  
  e$x$opts$leaflet <- leaf
  e
}

#' @rdname leaflet
#' @export
e_leaflet_tile <- function(e, template = "https://{s}.tile.openstreetmap.fr/hot/{z}/{x}/{y}.png", 
                           options = NULL, ...){
  
  if(!length(e$x$opts$leaflet$tiles))
    e <- e_leaflet(e)
  
  tile <- list(...)
  tile$urlTemplate <- template
  
  if(!is.null(options))
    tile$options <- options
    
  e$x$opts$leaflet$tiles <- list(tile)
  
  e
}
