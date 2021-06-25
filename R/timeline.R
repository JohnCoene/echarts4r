#' Timeline
#'
#' Set timeline options
#'
#' @inheritParams e_bar
#' @param axis_type Type of axis, \code{time}, \code{value}, or \code{category}.
#' @param index The index of the element to apply options to, see examples.
#' @param serie_index The index of the serie to add elements to.
#' @param ... Named options.
#'
#' @section Functions:
#' \itemize{
#'   \item{\code{e_timeline_opts}: Pass general timeline options, see \href{https://echarts.apache.org/en/option.html#timeline}{official documentation}.}
#'   \item{\code{e_timeline_serie}: Pass options to each serie, each options \emph{must}  be a vector or list the same length as their are steps, see examples.}
#'   \item{\code{e_timeline_make}: Helper function that wraps your data and \code{e_timeline_serie} to dynamically add options to series.}
#' }
#'
#' @examples
#' # general options
#' iris |>
#'   group_by(Species) |>
#'   e_charts(Sepal.Length, timeline = TRUE) |>
#'   e_line(Sepal.Width) |>
#'   e_timeline_opts(
#'     autoPlay = TRUE,
#'     rewind = TRUE
#'   )
#'
#' # serie options
#' iris |>
#'   group_by(Species) |>
#'   e_charts(Sepal.Length, timeline = TRUE) |>
#'   e_line(Sepal.Width) |>
#'   e_timeline_serie(
#'     title = list(
#'       list(text = "setosa"),
#'       list(text = "versicolor"),
#'       list(text = "virginica")
#'     )
#'   )
#' @name timeline-opts
#' @export
e_timeline_opts <- function(e, axis_type = "category", ...) {
  if (missing(e)) {
    stop("missing e", call. = FALSE)
  }

  if (!e$x$tl) {
    warning("timeline not enabled in e_chart", call. = FALSE)
  }

  e$x$opts$baseOption$timeline$axisType <- axis_type

  e$x$opts$baseOption$timeline <- append(e$x$opts$baseOption$timeline, list(...))

  e
}

#' @name timeline-opts
#' @export
e_timeline_serie <- function(e, ..., index = 1) {
  if (missing(e)) {
    stop("missing e", call. = FALSE)
  }

  args <- list(...)

  if (length(args) == 0) {
    stop("no arguments passed", call. = FALSE)
  }

  for (i in 1:length(e$x$opts$options)) {
    for (j in 1:length(args)) {
      if (!length(e$x$opts$options[[i]][[names(args)[j]]]) < index) {
        e$x$opts$options[[i]][[names(args)[j]]][[index]] <- list()
      }
      e$x$opts$options[[i]][[names(args)[j]]][[index]] <- args[[j]][[i]]
    }
  }

  e
}

#' @name timeline-opts
#' @export
e_timeline_on_serie <- function(e, ..., serie_index) {
  if (missing(e) || missing(serie_index)) {
    stop("missing e or serie_index", call. = FALSE)
  }

  args <- list(...)

  if (length(args) == 0) {
    stop("no arguments passed", call. = FALSE)
  }

  for (i in 1:length(e$x$opts$options)) {
    for (j in 1:length(args)) {
      if (!length(e$x$opts$options[[i]]$series[[serie_index]][[names(args)[j]]]) < serie_index) {
        e$x$opts$options[[i]]$series[[serie_index]][[names(args)[j]]] <- list()
      }
      e$x$opts$options[[i]]$series[[serie_index]][[names(args)[j]]] <- args[[j]][[i]]
    }
  }

  e
}
