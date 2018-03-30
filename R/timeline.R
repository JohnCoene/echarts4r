#' Timeline
#' 
#' @examples 
#' d <- Sys.Date()
#' 
#' mtcars %>% 
#'   e_charts(mpg) %>% 
#'   e_line(disp) %>% 
#'   e_line(hp) %>% 
#'   e_timeline(c(d - 1, d))
#' 
#' @noRd
#' @keywords internal
e_timeline <- function(e, timeline, ...){
  
  if(missing(e) || missing(timeline))
    stop("missing e or timeline", call. = FALSE)
  
  tl <- list(...)
  tl$data <- timeline
  
  e$x$opts$timeline <- tl
  
  e
}