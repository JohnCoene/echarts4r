#' Visual Map
#' 
#' @inheritParams e_bar
#' @param calculable Whether show handles, which can be dragged to adjust "selected range".
#' @param type One of \code{continuous} or \code{piecewise}.
#' 
#' @export
e_visual_map <- function(e, calculable = TRUE, type = c("continuous", "piecewise"), ...){
  
  vm <- list(...)
  vm$calculable <- calculable
  vm$calculable <- type[1]
  
  e$x$opts$visualMap <- vm
  e
}