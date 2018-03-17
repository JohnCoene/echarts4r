#' Crowsstalk
#' 
#' @examples 
#' \dontrun{
#' library(crosstalk)
#' 
#' sd <- sharedData$new(mtcars)
#' bscols(
#'   mtcars %>% 
#'     e_charts(mpg, width="100%", height=300) %>% 
#'     e_line(qsec) %>% 
#'     e_brush(),
#'  mtcars %>% 
#'    e_charts(qsec, width="100%", height=300) %>% 
#'    e_line(mpg) %>% 
#'    e_brush()
#' )
#' }
#' 
#' @noRd
#' @keywords internal
NULL