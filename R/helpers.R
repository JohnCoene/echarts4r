#' Textures
#' 
#' Path to textures.
#' 
#' @param file Path to file.
#' @param convert Converts image to JSON formatted arrays. 
#' 
#' @section Functions:
#' \itemize{
#'   \item{\code{e_map_texture} globe texture}
#'   \item{\code{e_stars_texture} starts texture}
#'   \item{\code{e_mconvert_texture} convert file to JSON formatted array}
#' }
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
#' @examples 
#' \dontrun{
#' e_map_texture(FALSE) # returns path to file
#' e_convert_texture("path/to/file.png") # converts file
#' }
#' 
#' @rdname textures 
#' @export
e_map_texture <- function(convert = TRUE){
  .get_file("assets/world.topo.bathy.200401.jpg", convert)
}

#' @rdname textures 
#' @export
e_globe_texture <- e_map_texture

#' @rdname textures 
#' @export
e_composite_texture <- function(convert = TRUE){
  .get_file("assets/bathymetry_bw_composite_4k.jpg", convert)
}

#' @rdname textures 
#' @export
e_stars_texture <- function(convert = TRUE){
  .get_file("assets/starfield.jpg", convert)
}

#' @rdname textures 
#' @export
e_convert_texture <- function(file){
  if(missing(file))
    stop("missing file", call. = FALSE)
  paste0("data:image/png;base64,", base64enc::base64encode(file))
}
