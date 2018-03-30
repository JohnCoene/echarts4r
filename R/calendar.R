#' Calendar
#' 
#' @inheritParams e_bar
#' 
#' @export
e_calendar <- function(e, range, ...){
  
  if(missing(e) || missing(range))
    stop("missing e or range", call. = FALSE)
  
  # initialise
  if(!length(e$x$opts$calendar))
    e$x$opts$calendar <- list()
  
  cal <- list(range = range, ...)
  
  e$x$opts$calendar <- append(e$x$opts$calendar, list(cal))
  e
}