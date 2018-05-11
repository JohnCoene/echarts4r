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
e_globe_dark_texture <- function(convert = TRUE){
  .get_file("assets/world_dark.jpg", convert)
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

#' Country names
#' 
#' Convert country names to echarts format.
#' 
#' @param data Data.frame in which to find column names.
#' @param input,output Input and output columns.
#' @param type Passed to \link[countrycode]{countrycode} \code{origin} parameter.
#' @param ... Any other parameter to pass to \link[countrycode]{countrycode}.
#' 
#' @details Taiwan and Hong Kong cannot be plotted.
#' 
#' @examples 
#' cns <- data.frame(country = c("US", "BE"))
#' 
#' # replace
#' e_country_names(cns, country)
#'   
#' # specify output
#' e_country_names(cns, country, country.name)
#'   
#' 
#' @rdname e_country_names
#' @export
e_country_names <- function(data, input, output, type = "iso2c", ...){
  
  if(missing(data) || missing(input))
    stop("must pass data and input", call. = FALSE)
  
  if(missing(output))
    output <- NULL
  else
    output <- deparse(substitute(output))
  
  e_country_names_(data, deparse(substitute(input)), output, type, ...)
}

#' @rdname e_country_names
#' @export
e_country_names_ <- function(data, input, output = NULL, type = "iso2c", ...){
  
  if(missing(data) || missing(input))
    stop("must pass data and input", call. = FALSE)
  
  src <- input
  cn <- countrycode::countrycode(data[[src]], origin = type, destination = "country.name", ...)
  
  if(is.null(output))
    output <- src
  
  data[[output]] <- .correct_countries(cn)
  data
}

#' Color range
#' 
#' Build manual color range
#' 
#' @param data Data.frame in which to find column names.
#' @param input,output Input and output columns.
#' @param colors Colors to pass to \code{\link{colorRampPalette}}.
#' @param ... Any other argument to pass to \code{\link{colorRampPalette}}.
#' 
#' @examples 
#' df <- data.frame(val = 1:10)
#' 
#' e_color_range(df, val, colors)
#'   
#' @rdname e_color_range
#' @export
e_color_range <- function(data, input, output, colors = c("#bf444c", "#d88273", "#f6efa6"), ...){
  
  if(missing(data) || missing(input) || missing(output))
    stop("must pass data, input and output", call. = FALSE)
  
  e_color_range_(data, deparse(substitute(input)), deparse(substitute(output)), colors, ...)
}

#' @rdname e_color_range
#' @export
e_color_range_ <- function(data, input, output, colors = c("#bf444c", "#d88273", "#f6efa6"), ...){
  
  if(missing(data) || missing(input) || missing(output))
    stop("must pass data, input and output", call. = FALSE)
  
  serie <- data[[input]]
  
  data[[output]] <- colorRampPalette(colors, ...)(length(serie))
  data
}

#' Get data
#' 
#' Get data passed to \code{\link{e_charts}}.
#' 
#' @inheritParams e_bar
#' 
#' @export
e_get_data <- function(e){
  e$x$data
}