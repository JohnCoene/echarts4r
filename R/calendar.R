#' Calendar
#' 
#' @inheritParams e_bar
#' @param range Range of calendar format, string or vector.
#' 
#' @examples 
#' # year
#' mtcars %>% 
#'   e_charts() %>% 
#'   e_calendar(range = "2017")
#'   
#' # month
#' mtcars %>% 
#'   e_charts() %>% 
#'   e_calendar(range = "2018-01")
#'   
#' # range
#' mtcars %>% 
#'   e_charts() %>% 
#'   e_calendar(range = c("2018-01", "2018-07"))
#' 
#' @seealso \href{Additional arguments}{https://ecomfe.github.io/echarts-doc/public/en/option.html#calendar}
#' 
#' @export
e_calendar <- function(e, range, ...){
  
  if(missing(e) || missing(range))
    stop("missing e or range", call. = FALSE)
  
  # initialise
  if(!length(e$x$opts$calendar))
    e$x$opts$calendar <- list()
  
  cal <- list(range = range, ...)
  
  if(!e$x$tl)
    e$x$opts$calendar <- append(e$x$opts$calendar, list(cal))
  else
    e$x$opts$baseOption$calendar <- append(e$x$opts$baseOption$calendar, list(cal))
  
  e
}