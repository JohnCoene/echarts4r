#' Timeline 
#' 
#' Set timeline options
#' 
#' @inheritParams e_bar
#' 
#' @examples 
#' iris %>% 
#'   group_by(Species) %>% 
#'   e_charts(Sepal.Length, timeline = TRUE) %>% 
#'   e_line(Sepal.Width) %>% 
#'   e_timeline_opts(
#'     autoPlay = TRUE,
#'     rewind = TRUE
#'   )
#'   
#' @seealso \href{https://ecomfe.github.io/echarts-doc/public/en/option.html#timeline}{official documentation}
#' 
#' @export
e_timeline_opts <- function(e, ...){
  
  if(missing(e))
    stop("missing e", call. = FALSE)
  
  if(!e$x$tl)
    warning("timeline not enabled in e_chart", call. = FALSE)
  
  e$x$opts$baseOption$timeline <- append(e$x$opts$baseOption$timeline, list(...))
  
  e
}