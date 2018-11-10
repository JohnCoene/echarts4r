echarts_build <- function(e) {
  e$x$data <- NULL
  e$x$mapping <- NULL
  e
} 

#' Initialise
#'
#' Initialise a chart.
#'
#' @param data A \code{data.frame}.
#' @param e An object of class \code{echarts4r} as returned by \code{e_charts}.
#' @param x Column name containing x axis.
#' @param width,height Must be a valid CSS unit (like \code{'100\%'},
#'   \code{'400px'}, \code{'auto'}) or a number, which will be coerced to a
#'   string and have \code{'px'} appended.
#' @param elementId Id of element.
#' @param dispose Set to \code{TRUE} to force redraw of chart, set to \code{FALSE} to update.
#' @param renderer Renderer, takes \code{canvas} (default) or \code{svg}.
#' @param ... Any other argument.
#' 
#' @examples 
#' mtcars %>% 
#'   e_charts_("qsec") %>%
#'   e_line(mpg)
#'
#' @import htmlwidgets
#' @importFrom grDevices boxplot.stats
#' @importFrom grDevices colorRampPalette
#' @importFrom stats as.formula lm glm loess predict
#' @importFrom graphics hist
#' 
#' @rdname init
#' @export
e_charts <- function(data, x, width = NULL, height = NULL, elementId = NULL, dispose = TRUE, renderer = "canvas", ...) {

  xmap <- NULL
  if(!missing(x))
    xmap <- deparse(substitute(x))

  # forward options using x
  x = list(
    theme = "",
    renderer = tolower(renderer),
    mapping = list(),
    events = list(),
    buttons = list(),
    opts = list(
      ...,
      yAxis = list(
        list(show = TRUE)
      )
    )
  )
  
  if(!missing(data)){
    
    row.names(data) <- NULL
    
    if(!is.null(xmap))
      data <- .arrange_data_x(data, xmap)
    
    x$data <- map_grps_(data)
  }
  
  if(!is.null(xmap)){
    x$mapping$x <- xmap[1]
    x$mapping$x_class <- class(data[[xmap]])
    x <- .assign_axis(x, data)
  }
  
  x$dispose <- dispose
  
  # create widget
  htmlwidgets::createWidget(
    name = 'echarts4r',
    x,
    width = width,
    height = height,
    package = 'echarts4r',
    elementId = elementId,
    preRenderHook = echarts_build,
    sizingPolicy = htmlwidgets::sizingPolicy(
      defaultWidth = "100%",
      knitr.figure = FALSE,
      browser.fill = TRUE
    )
  )
}

#' @rdname init
#' @export
e_charts_ <- function(data, x = NULL, width = NULL, height = NULL, elementId = NULL, dispose = TRUE, renderer = "canvas", ...) {
  
  xmap <- x
  
  # forward options using x
  x = list(
    theme = "",
    renderer = tolower(renderer),
    mapping = list(),
    events = list(),
    buttons = list(),
    opts = list(
      ...,
      yAxis = list(
        list(show = TRUE)
      )
    )
  )
  
  if(!missing(data)){
    
    row.names(data) <- NULL
    
    if(!is.null(xmap))
      data <- .arrange_data_x(data, xmap)
    
    x$data <- map_grps_(data)
  }
  
  if(!is.null(xmap)){
    x$mapping$x <- xmap
    x$mapping$x_class <- class(data[[xmap]])
    x <- .assign_axis(x, data)
  }
  
  x$dispose <- dispose
  
  # create widget
  htmlwidgets::createWidget(
    name = 'echarts4r',
    x,
    width = width,
    height = height,
    package = 'echarts4r',
    elementId = elementId,
    sizingPolicy = htmlwidgets::sizingPolicy(
      defaultWidth = "100%",
      knitr.figure = FALSE,
      browser.fill = TRUE
    )
  )
}

#' @rdname init
#' @export
e_chart <- e_charts

#' @rdname init
#' @export
e_data <- function(e, data, x){
  
  if(missing(data))
    stop("must pass data")
  
  if(!missing(x))
    xmap <- deparse(substitute(x))
  
  if(!missing(x)){
    e$x$mapping$x <- xmap
    e$x$mapping$x_class <- class(data[[xmap]])
    e$x <- .assign_axis(e$x, data)
  }
  
  row.names(data) <- NULL
  
  if(!missing(x))
    data <- .arrange_data_x(data, xmap)
  
  data <- map_grps_(data)
  e$x$data <- data
  
  e
}

#' Shiny bindings for echarts4r
#'
#' Output and render functions for using echarts4r within Shiny
#' applications and interactive Rmd documents.
#'
#' @param outputId output variable to read from.
#' @param width,height Must be a valid CSS unit (like \code{'100\%'},
#'   \code{'400px'}, \code{'auto'}) or a number, which will be coerced to a
#'   string and have \code{'px'} appended.
#' @param expr An expression that generates a echarts4r
#' @param env The environment in which to evaluate \code{expr}.
#' @param quoted Is \code{expr} a quoted expression (with \code{quote()})? This
#'   is useful if you want to save an expression in a variable.
#' @param id Target chart id.
#' @param session Shiny session.
#' 
#' @section Callbacks:
#' \itemize{
#'   \item{\code{id_brush}: returns data on brushed data points.}
#'   \item{\code{id_legend_change}: returns series name of legend selected/unselected.}
#'   \item{\code{id_clicked_data}: returns data of clicked data point.}
#'   \item{\code{id_clicked_data_value}: returns value of clicked data point.}
#'   \item{\code{id_clicked_row}: returns row number of clicked data point.}
#'   \item{\code{id_clicked_serie}: returns name of serie of clicked data point.}
#'   \item{\code{id_mouseover_data}: returns data on hovered data point.}
#'   \item{\code{id_mouseover_data_value}: returns value of hovered data point.}
#'   \item{\code{id_mouseover_row}: returns row o hovered data point.}
#'   \item{\code{id_mouseover_serie}: returns name of serie of hovered data point.}
#' }
#' 
#' @section Proxies:
#' \itemize{
#'   \item{\code{\link{e_append1_p}} & \code{\link{e_append2_p}}}
#'   \item{\code{\link{e_showtip_p}} & \code{\link{e_hidetip_p}}}
#'   \item{\code{\link{e_highlight_p}} & \code{\link{e_downplay_p}}}
#'   \item{\code{\link{e_focus_adjacency}} & \code{\link{e_unfocus_adjacency}}}
#'   \item{\code{\link{e_dispatch_action_p}}}
#' }
#' 
#'
#' @name echarts4r-shiny
#'
#' @export
echarts4rOutput <- function(outputId, width = '100%', height = '400px'){
  htmlwidgets::shinyWidgetOutput(outputId, 'echarts4r', width, height, package = 'echarts4r')
}

#' @rdname echarts4r-shiny
#' @export
renderEcharts4r <- function(expr, env = parent.frame(), quoted = FALSE) {
  if (!quoted) { expr <- substitute(expr) } # force quoted
  htmlwidgets::shinyRenderWidget(expr, echarts4rOutput, env, quoted = TRUE)
}

#' @rdname echarts4r-shiny
#' @export
echarts4rProxy <- function(id, session = shiny::getDefaultReactiveDomain()){
  
  proxy <- list(id = id, session = session)
  class(proxy) <- "echarts4rProxy"
  
  return(proxy)
}