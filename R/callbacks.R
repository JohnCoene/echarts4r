#' Callbacks
#' 
#' Binds events to chart interactions.
#' 
#' @inheritParams e_bar
#' @param query Condistion that triggers the handler
#' @param handler JavaSscript handler, passed to \link[htmlwidgets]{JS}.
#' @param event Event that triggers the handler.
#' 
#' @examples 
#' cars %>% 
#'   e_charts(speed) %>% 
#'   e_scatter(dist) %>% 
#'   e_on(
#'     list(seriesName = "dist"),
#'     "function(){alert('Serie clicked')}"
#'   )
#' 
#' @seealso \href{https://ecomfe.github.io/echarts-doc/public/en/api.html#echartsInstance.on}{official documentation}
#' 
#' @name callbacks
#' @export
e_on <- function(e, query, handler, event = "click"){
  
  if(missing(query) || missing(handler))
    stop("must pass query and handler", call. = FALSE)
  
  if(!length(e$x$on))
    e$x$on <- list()
  
  opts <- list(
    event = event,
    query = query,
    handler = htmlwidgets::JS(handler)
  )
  
  e$x$on <- append(e$x$on, list(opts))
  
  e
}

#' @rdname callbacks
#' @export
e_off <- function(e, query, handler, event = "click"){
  
  if(missing(query) || missing(handler))
    stop("must pass query and handler", call. = FALSE)
  
  if(!length(e$x$off))
    e$x$off <- list()
  
  opts <- list(
    event = event,
    query = query,
    handler = htmlwidgets::JS(handler)
  )
  
  e$x$off <- append(e$x$off, list(opts))
  
  e
}