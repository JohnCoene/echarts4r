#' Initialise
#'
#' Initialise a chart.
#'
#' @param data A \code{data.frame}.
#' @param width,height Must be a valid CSS unit (like \code{'100\%'},
#'   \code{'400px'}, \code{'auto'}) or a number, which will be coerced to a
#'   string and have \code{'px'} appended.
#' @param elementId Id of element.
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
#' @import rlang
#'
#' @export
e_charts <- function(data, x = NULL, width = NULL, height = NULL, elementId = NULL) {

  xAxis <- list()
  if(!is.null(rlang::enexpr(x)))
    xAxis <- list(
      data = .build_vector(data, dplyr::enquo(x))
    )

  # forward options using x
  x = list(
    opts = list(
      xAxis = xAxis,
      yAxis = list(
        show = TRUE
      )
    )
  )
  
  if(!missing(data))
    x$data <- data

  # create widget
  htmlwidgets::createWidget(
    name = 'echarts4',
    x,
    width = width,
    height = height,
    package = 'echarts4',
    elementId = elementId
  )
}

#' Shiny bindings for echarts4
#'
#' Output and render functions for using echarts4 within Shiny
#' applications and interactive Rmd documents.
#'
#' @param outputId output variable to read from.
#' @param width,height Must be a valid CSS unit (like \code{'100\%'},
#'   \code{'400px'}, \code{'auto'}) or a number, which will be coerced to a
#'   string and have \code{'px'} appended.
#' @param expr An expression that generates a echarts4
#' @param env The environment in which to evaluate \code{expr}.
#' @param quoted Is \code{expr} a quoted expression (with \code{quote()})? This
#'   is useful if you want to save an expression in a variable.
#'
#' @name echarts4-shiny
#'
#' @export
echarts4Output <- function(outputId, width = '100%', height = '400px'){
  htmlwidgets::shinyWidgetOutput(outputId, 'echarts4', width, height, package = 'echarts4')
}

#' @rdname echarts4-shiny
#' @export
renderEcharts4 <- function(expr, env = parent.frame(), quoted = FALSE) {
  if (!quoted) { expr <- substitute(expr) } # force quoted
  htmlwidgets::shinyRenderWidget(expr, echarts4Output, env, quoted = TRUE)
}
