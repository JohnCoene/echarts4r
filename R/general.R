#' General options
#' 
#' @param font_family Font family.
#' @param theme A theme.
#' 
#' @export
e_common <- function(font_family = NULL, theme = NULL){
  
  options(
    "ECHARTS4R_THEME" = theme,
    "ECHARTS4R_FONT_FAMILY" = font_family
  )
  
}