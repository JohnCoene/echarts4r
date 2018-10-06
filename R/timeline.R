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
e_timeline <- function(e){
  
  if(missing(e))
    stop("missing e", call. = FALSE)
  
  e$x$timeline <- list(
    
  )
}