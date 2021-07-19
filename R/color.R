#' Color
#'
#' Customise chart and background colors.
#'
#' @inheritParams e_bar
#' @param color Vector of colors.
#' @param background Background color.
#' @param append Only applicable to `echarts4rProxy`.
#' Whether to append the `color` to the existing
#' array (vector) or colors or to replace it.
#'
#' @examples
#' mtcars |>
#'   e_charts(drat) |>
#'   e_line(mpg) |>
#'   e_area(qsec) |>
#'   e_color(
#'     c("red", "blue"),
#'     "#d3d3d3"
#'   )
#' @seealso \code{\link{e_theme}},
#' \href{https://echarts.apache.org/en/option.html#color}{Official color documentation},
#' \href{https://echarts.apache.org/en/option.html#backgroundColor}{Official background documentation}
#' 
#' @name e_color
#' @export
e_color <- function(e, color = NULL, background = NULL, append = TRUE) UseMethod("e_color")

#' @rdname e_color
#' @export 
e_color.echarts4r <- function(e, color = NULL, background = NULL, append = TRUE) {
  if (!e$x$tl) {
    if (!is.null(color)) e$x$opts$color <- as.list(color)
    if (!is.null(background)) e$x$opts$backgroundColor <- background
  } else {
    if (!is.null(color)) e$x$opts$baseOption$color <- color
    if (!is.null(background)) e$x$opts$baseOption$backgroundColor <- background
  }
  e
}

#' @rdname e_color
#' @export 
e_color.echarts4rProxy <- function(e, color = NULL, background = NULL, append = TRUE) {
  if (!is.null(color)) e$chart$x$opts$color <- as.list(color)
  if (!is.null(background)) e$chart$x$opts$backgroundColor <- background
  e$chart$x$opts$appendColor <- append

  e
}
