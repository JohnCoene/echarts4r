#' Textures
#' 
#' Path to textures.
#' 
#' @param convert Converts image to JSON formatted arrays. 
#' 
#' @details
#' Due to browser
#' "same origin policy" security restrictions, loading textures
#' from a file system in three.js may lead to a security exception,
#' see
#' \url{https://github.com/mrdoob/three.js/wiki/How-to-run-things-locally}.
#' References to file locations work in Shiny apps, but not in stand-alone
#' examples. The \code{*texture} functions facilitates transfer of image
#' texture data from R into textures when \code{convert} is set to \code{TRUE}.
#' 
#' @rdname textures 
#' @export
e_map_texture <- function(convert = TRUE){
  .get_file("assets/world.topo.bathy.200401.jpg", convert)
}

#' @rdname textures 
#' @export
e_stars_texture <- function(convert = TRUE){
  .get_file("assets/starfield.jpg", convert)
}
