#' Mark point
#'
#' Mark points and lines.
#'
#' @inheritParams e_bar
#' @param serie Serie or vector of series to mark on, defaults to all series.
#' @param data Placement of point, line or area.
#' @param title A convenience argument to easily set label, see details.
#' @param title_position Position of title.
#'
#' @details To set a label you need to either use the \code{title}
#' argument or pass a list specifying the label formatter.
#' \code{label = list(formatter = "label")}. The former is more convenient
#' but more limited, e.g.: you cannot specify the placement of the label.
#'
#' @examples
#' max <- list(
#'   name = "Max",
#'   type = "max"
#' )
#'
#' min <- list(
#'   name = "Min",
#'   type = "min"
#' )
#'
#' avg <- list(
#'   type = "average",
#'   name = "AVG"
#' )
#'
#' mtcars %>%
#'   e_charts(mpg) %>%
#'   e_line(wt) %>%
#'   e_line(drat) %>%
#'   e_line(cyl) %>%
#'   e_mark_point("wt", data = max) %>%
#'   e_mark_point(c("cyl", "drat"), data = min) %>%
#'   e_mark_line(data = avg) %>% # applies to all
#'   e_mark_area(serie = "wt", data = list(
#'     list(xAxis = "min", yAxis = "min"),
#'     list(xAxis = "max", yAxis = "max")
#'   ))
#'
#' # serie options
#'
#' iris %>%
#'   group_by(Species) %>%
#'   e_charts(Sepal.Length, timeline = TRUE) %>%
#'   e_line(Sepal.Width) %>%
#'   e_timeline_serie(
#'     title = list(
#'       list(text = "setosa"),
#'       list(text = "versicolor"),
#'       list(text = "virginica")
#'     )
#'   ) %>%
#'   e_mark_area(serie = "setosa") %>%
#'   e_mark_area(
#'     serie = "versicolor",
#'     data = list(
#'       list(xAxis = 2),
#'       list(xAxis = 4)
#'     ),
#'     itemStyle = list(color = "lightblue")
#'   ) %>%
#'   e_mark_area(serie = "virginica")
#' @seealso \href{https://echarts.apache.org/en/option.html#series-line.markPoint}{Additional point arguments},
#' \href{https://echarts.apache.org/en/option.html#series-line.markLine}{Additional line arguments}
#'
#' @rdname mark
#' @export
e_mark_point <- function(e, serie = NULL, data = NULL, ..., title = NULL, title_position = NULL) {
  if (missing(e)) {
    stop("must pass e", call. = FALSE)
  }

  if (is.null(serie)) {
    index <- 1:length(if (e$x$tl) e$x$opts$options else e$x$opts$series)
  } else {
    index <- .get_index(e, serie)
  }

  for (i in index) {
    point <- list(...)

    if (!is.null(data)) {
      point$data <- list(data)
    }

    if (!is.null(title) && !is.null(data)) {
      point$data[[1]]$label <- list(formatter = title, position = title_position)
    }

    if (e$x$tl) {
      if (is.null(e$x$opts$options[[i]]$series[[1]]$markPoint)) {
        e$x$opts$options[[i]]$series[[1]]$markPoint <- append(e$x$opts$options[[i]]$series[[1]]$markPoint, point)
      } else {
        e$x$opts$options[[i]]$series[[1]]$markPoint$data <- append(e$x$opts$options[[i]]$series[[1]]$markPoint$data, point$data)
      }
    }
    else {
      if (is.null(e$x$opts$series[[i]]$markPoint)) {
        e$x$opts$series[[i]]$markPoint <- append(e$x$opts$series[[i]]$markPoint, point)
      } else {
        e$x$opts$series[[i]]$markPoint$data <- append(e$x$opts$series[[i]]$markPoint$data, point$data)
      }
    }
  }

  e
}

#' @rdname mark
#' @export
e_mark_line <- function(e, serie = NULL, data = NULL, ..., title = NULL, title_position = NULL) {
  if (missing(e)) {
    stop("must pass e", call. = FALSE)
  }

  if (is.null(serie)) {
    index <- 1:length(if (e$x$tl) e$x$opts$options else e$x$opts$series)
  } else {
    index <- .get_index(e, serie)
  }

  for (i in index) {
    point <- list(...)

    if (!is.null(data)) {
      point$data <- list(data)
    }

    if (!is.null(title) && !is.null(data)) {
      point$data[[1]]$label <- list(formatter = title, position = title_position)
    }

    if (e$x$tl) {
      if (is.null(e$x$opts$options[[i]]$series[[1]]$markLine)) {
        e$x$opts$options[[i]]$series[[1]]$markLine <- append(e$x$opts$options[[i]]$series[[1]]$markLine, point)
      } else {
        e$x$opts$options[[i]]$series[[1]]$markLine$data <- append(e$x$opts$options[[i]]$series[[1]]$markLine$data, point$data)
      }
    }
    else {
      if (is.null(e$x$opts$series[[i]]$markLine)) {
        e$x$opts$series[[i]]$markLine <- append(e$x$opts$series[[i]]$markLine, point)
      } else {
        e$x$opts$series[[i]]$markLine$data <- append(e$x$opts$series[[i]]$markLine$data, point$data)
      }
    }
  }

  e
}

#' @rdname mark
#' @export
e_mark_area <- function(e, serie = NULL, data = NULL, ..., title = NULL, title_position = NULL) {
  if (missing(e)) {
    stop("must pass e", call. = FALSE)
  }

  if (is.null(serie)) {
    index <- 1:length(if (e$x$tl) e$x$opts$options else e$x$opts$series)
  } else {
    index <- .get_index(e, serie)
  }

  for (i in index) {
    point <- list(...)

    if (!is.null(data)) {
      point$data <- list(data)
    }

    if (!is.null(title) && !is.null(data)) {
      point$data[[1]]$label <- list(formatter = title, position = title_position)
    }

    if (e$x$tl) {
      if (is.null(e$x$opts$options[[i]]$series[[1]]$markArea)) {
        e$x$opts$options[[i]]$series[[1]]$markArea <- append(e$x$opts$options[[i]]$series[[1]]$markArea, point)
      } else {
        e$x$opts$options[[i]]$series[[1]]$markArea$data <- append(e$x$opts$options[[i]]$series[[1]]$markArea$data, point$data)
      }
    }
    else {
      if (is.null(e$x$opts$series[[i]]$markArea)) {
        e$x$opts$series[[i]]$markArea <- append(e$x$opts$series[[i]]$markArea, point)
      } else {
        e$x$opts$series[[i]]$markArea$data <- append(e$x$opts$series[[i]]$markArea$data, point$data)
      }
    }
  }

  e
}
