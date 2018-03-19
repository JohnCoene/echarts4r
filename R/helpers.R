#' Textures
#' 
#' Path to textures.
#' 
#' @rdname textures 
#' @export
e_map_texture <- function(){
  system.file("assets/world.topo.bathy.200401.jpg", package = "echarts4r")
}

#' @rdname textures 
#' @export
e_stars_texture <- function(){
  system.file("assets/starfield.jpg", package = "echarts4r")
}
