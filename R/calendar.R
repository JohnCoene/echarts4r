#' Calendar
#'
#' @inheritParams e_bar
#' @param range Range of calendar format, string or vector.
#'
#' @examples
#' dates <- seq.Date(as.Date("2017-01-01"), as.Date("2019-12-31"), by = "day")
#' values <- rnorm(length(dates), 20, 6)
#' year <- data.frame(date = dates, values = values)
#'
#' year |>
#'    e_charts(date) |>
#'    e_calendar(range = "2017") |>
#'    e_visual_map(max = 30) |>
#'    e_heatmap(values, coord_system = "calendar")
#'
#' # month
#' year |>
#'    e_charts(date) |>
#'    e_calendar(range = "2017-01") |>
#'    e_visual_map(max = 30) |>
#'    e_heatmap(values, coord_system = "calendar")
#'
#' # range
#' year |>
#'    e_charts(date) |>
#'    e_calendar(range = c("2018-01", "2018-07")) |>
#'    e_visual_map(max = 30) |>
#'    e_heatmap(values, coord_system = "calendar")
#'
#' @seealso \href{https://echarts.apache.org/en/option.html#calendar}{Additional arguments}
#'
#' @export
e_calendar <- function(e, range, ...) {
  if (missing(e) || missing(range)) {
    stop("missing e or range", call. = FALSE)
  }

  # initialise
  if (!length(e$x$opts$calendar)) {
    e$x$opts$calendar <- list()
  }

  cal <- list(range = range, ...)

  if (!e$x$tl) {
    e$x$opts$calendar <- append(e$x$opts$calendar, list(cal))
  } else {
    e$x$opts$baseOption$calendar <- append(e$x$opts$baseOption$calendar, list(cal))
  }

  e
}
