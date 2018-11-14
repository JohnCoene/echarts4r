#' Globe
#' 
#' Add globe.
#' 
#' @inheritParams e_bar
#' @param environment Texture of background.
#' @param base_texture Base texture of globe.
#' @param height_texture Texture of height.
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
#'   e_charts(lon) %>% 
#'   e_globe(
#'     displacementScale = 0.04
#'   ) %>% 
#'   e_bar_3d(lat, value, "globe") %>% 
#'   e_visual_map(show = FALSE)
#' }
#' 
#' @seealso \code{\link{e_country_names}}, \href{http://echarts.baidu.com/option-gl.html#globe}{Additional arguments}
#' 
#' @export
e_globe <- function(e, environment = NULL, base_texture = NULL, height_texture = NULL, ...){
  
  ls <- list(
    environment = environment,
    heightTexture = height_texture,
    baseTexture = base_texture,
    series = list(),
    ...
  )
  
  e$x$opts$globe <- ls
  e
}