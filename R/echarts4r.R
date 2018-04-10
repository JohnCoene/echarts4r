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
#' @param ... Any other argument.
#' 
#' @examples 
#' USArrests %>% 
#'   dplyr::mutate(
#'     state = row.names(.)
#'   ) %>% 
#'   e_charts(state) %>%
#'   e_line(Rape)
#'
#' @import htmlwidgets
#' @importFrom grDevices boxplot.stats
#' @importFrom grDevices colorRampPalette
#' 
#' @rdname init
#' @export
e_charts <- function(data, x, width = NULL, height = NULL, elementId = NULL, dispose = TRUE, ...) {

  key <- NULL
  group <- NULL
  xmap <- NULL
  if(!missing(x))
    xmap <- deparse(substitute(x))
  
  if(!missing(data)){
    if(crosstalk::is.SharedData(data)) {
      key <- data$key()
      group <- data$groupName()
      data <- data$origData()
    } 
  }

  # forward options using x
  x = list(
    theme = "",
    mapping = list(),
    settings = list(
      crosstalk_key = key,
      crosstalk_group = group
    ),
    opts = list(
      ...,
      yAxis = list(
        list(show = TRUE)
      )
    )
  )
  
  if(!missing(data)){
    
    # ungroup
    if(dplyr::is.grouped_df(data))
      data <- data %>% dplyr::ungroup()
    
    row.names(data) <- NULL
    x$data <- data
  }
  
  if(!is.null(xmap)){
    x$mapping$x <- xmap
    x$mapping$x_class <- class(data[[xmap]])
    x <- .assign_axis(x)
  }
  
  x$dispose <- dispose
  
  # create widget
  htmlwidgets::createWidget(
    name = 'echarts4r',
    x,
    width = width,
    height = height,
    dependencies = crosstalk::crosstalkLibs(),
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
  
  if(!missing(x))
    xmap <- deparse(substitute(x))
  
  if(!is.null(xmap)){
    e$x$mapping$x <- xmap
    e$x$mapping$x_class <- class(data[[xmap]])
    e$x <- .assign_axis(e$x)
  }
  # ungroup
  if(dplyr::is.grouped_df(data))
    data <- data %>% dplyr::ungroup()
  
  row.names(data) <- NULL
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