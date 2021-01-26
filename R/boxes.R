#' Box
#'
#' Renders a data box in shiny.
#'
#' @param data A dataframe containing data to plot.
#' @param x,y Bare column name of variables to draw.
#' @param text,subtext Title and subtitle of box.
#' @param type Chart type to draw.
#' @param color Color of chart in box.
#' @param text_color Color of text.
#' @param background_color Color of box.
#' @param step Step method, only used if \code{type = "step"}.
#' @param title_args Additional arguments to add to the title.
#' @param tooltip Tooltip to use.
#' @param ... Additional arguments to pass to the serie.
#'
#' @examples
#' library(shiny)
#'
#' ui <- fluidPage(
#'   fluidRow(
#'     column(3, echarts4rBoxOutput("box1"))
#'   )
#' )
#'
#' server <- function(input, output) {
#'   output$box1 <- renderEcharts4rBox({
#'     echarts4rBox(cars, speed, dist, "Cars", type = "bar")
#'   })
#' }
#' \dontrun{
#' shinyApp(ui, server)
#' }
#' @seealso \code{\link{renderEcharts4rBox}}, \code{\link{echarts4rBoxOutput}}
#'
#' @export
echarts4rBox <- function(
  data,
  x,
  y,
  text = "",
  subtext = "",
  type = c("bar", "line", "scatter", "area", "step"),
  ...,
  color = "#ffffff",
  text_color = "#ffffff",
  background_color = "#293c55",
  step = c("start", "middle", "end"),
  title_args = list(),
  tooltip = list(trigger = "axis")
) {

  # for area, keep to use in series opts
  original_type <- type

  # inputs
  type <- match.arg(type)
  step <- match.arg(step)

  # override area
  if (type %in% c("area", "step")) type <- "line"

  # build expected data format
  data <- .build_data2(data, {{ x }}, {{ y }})

  # build serie
  serie <- list(
    color = color,
    type = type,
    data = data,
    ...
  )

  # make are
  if (original_type == "area") serie$areaStyle <- list()

  # make step
  if (original_type == "step") serie$step <- step

  # title
  title <- list(
    text = text,
    subtext = subtext
  )
  title <- append(title, title_args)
  title$textStyle$color <- text_color

  # build options
  opts <- list(
    title = title,
    tooltip = tooltip,
    xAxis = list(show = FALSE),
    yAxis = list(show = FALSE),
    grid = list(
      left = 0,
      top = 0,
      bottom = 0,
      right = 0
    ),
    series = list(serie)
  )

  list(
    options = opts,
    bgColor = background_color
  )
}

#' Render box
#'
#' Render an echarts4r box.
#'
#' @param expr An expression that produces as \code{\link{echarts4rBox}}.
#' @param env The environment in which to evaluate ‘expr’.
#' @param quoted Is ‘expr’ a quoted expression (with ‘quote()’)? This is useful if you want to save an expression in a variable.
#'
#' @export
renderEcharts4rBox <- function(expr, env = parent.frame(), quoted = FALSE) {
  func <- shiny::exprToFunction(expr, env = env, quoted = quoted)

  function() {
    val <- func()

    path <- system.file("htmlwidgets/lib/echarts-4.8.0", package = "echarts4r")

    deps <- htmltools::htmlDependency(
      name = "countup",
      version = "4.9.0",
      src = c(file = path),
      script = c("echarts-en.min.js")
    )

    val$deps <- lapply(
      htmltools::resolveDependencies(list(deps)),
      shiny::createWebDependency
    )

    return(val)
  }
}

#' Box Output
#'
#' Place box output in Shiny ui.
#'
#' @param id Id of box.
#' @param height Height of box, any valid CSS value, numerics are treated as pixels.
#'
#' @export
echarts4rBoxOutput <- function(id, height = 150) {
  if (missing(id)) {
    stop("Missing id", call. = FALSE)
  }

  # num to px
  if (inherits(height, "numeric")) {
    height <- paste0(height, "px")
  }

  # make style string
  style <- sprintf("width:100%%;height:%s;", height)

  el <- htmltools::div(
    class = "panel",
    id = sprintf("echarts4r-box-container-%s", id),
    htmltools::div(id = id, class = "echarts4rBox", style = style)
  )

  # add binding and css
  path <- system.file("box", package = "echarts4r")

  deps <- list(
    htmltools::htmlDependency(
      name = "echarts4rBox",
      version = "1.0.0",
      src = c(file = path),
      script = c("box-binding.js")
    )
  )

  htmltools::attachDependencies(el, deps)
}
