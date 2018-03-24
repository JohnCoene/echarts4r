#' Globe
#' 
#' Add globe.
#' 
#' @inheritParams e_bar
#' @param environment Texture of background.
#' @param base.texture Base texture of globe.
#' @param height.texture Texture of height.
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
#'   e_globe(
#'     environment = e_stars_texture(),
#'     base.texture = e_globe_texture()
#'   ) %>% 
#'   e_bar_3d(lon, lat, value, "globe") %>% 
#'   e_visual_map()
#' }
#' 
#' @export
e_globe <- function(e, environment = NULL, base.texture = NULL, height.texture = NULL, ...){
  
  ls <- list(
    environment = environment,
    heightTexture = height.texture,
    baseTexture = base.texture,
    series = list(),
    ...
  )
  
  e$x$opts$globe <- ls
  e
}