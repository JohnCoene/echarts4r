#' Axis
#'
#' Customise axis.
#'
#' @inheritParams e_bar
#' @param axis Axis to customise.
#' @param serie Column name of serie to range the axis. If used the range of the serie is used as,
#' \code{min} an \code{max}.
#' @param margin Margin to apply to \code{serie}: \eqn{min = serie - margin} and
#' \eqn{max = serie + margin}
#' @param index Index of axis to customise.
#' @param formatter An axis formatter as returned by \code{\link{e_axis_formatter}}.
#' @param style Formatter style, one of \code{decimal}, \code{percent}, or \code{currency}.
#' @param currency Currency to to display.
#' @param digits Number of decimals.
#' @param locale Locale, if \code{NULL} then it is inferred from \code{Sys.getlocale}.
#'
#' @section Functions:
#' \itemize{
#'   \item{\code{e_axis} to customise axis}
#'   \item{\code{e_rm_axis} to remove axis}
#' }
#'
#' @details The \code{e_axis_formatter} may not work in RStudio,
#' open the plot in your browser. It will display just fine in
#' Rmarkdown and Shiny.
#'
#' @examples
#' # range axis based on serie
#' cars |>
#'   e_charts(speed) |>
#'   e_line(dist) |>
#'   e_x_axis(speed) |>
#'   e_y_axis(dist)
#'
#' # use formatter
#' cars |>
#'   dplyr::mutate(
#'     speed = speed / 25
#'   ) |>
#'   e_charts(speed) |>
#'   e_scatter(dist) |>
#'   e_y_axis(
#'     formatter = e_axis_formatter("currency")
#'   ) |>
#'   e_x_axis(
#'     formatter = e_axis_formatter("percent", digits = 0)
#'   )
#'
#' # plot all labels & rotate
#' USArrests |>
#'   head(10) |>
#'   tibble::rownames_to_column(var = "State") |> 
#'   e_charts(State) |>
#'   e_area(Murder) |>
#'   e_x_axis(axisLabel = list(interval = 0, rotate = 45)) # rotate
#' @seealso \href{https://echarts.apache.org/en/option.html#xAxis}{Additional x arguments},
#' \href{https://echarts.apache.org/en/option.html#yAxis}{Additional y arguments}
#'
#' @rdname axis
#' @export
e_axis <- function(e, serie, axis = c("x", "y", "z"), index = 0, formatter = NULL, margin = 0, ...) {
  if (missing(serie)) {
    serie <- NULL
  } else {
    serie <- deparse(substitute(serie))
  }

  e_axis_(e, serie = serie, axis = axis, index = index, formatter = formatter, margin = margin, ...)
}

#' @rdname axis
#' @export
e_axis_ <- function(e, serie = NULL, axis = c("x", "y", "z"), index = 0, formatter = NULL, margin = 0, ...) {
  if (missing(e)) {
    stop("missing e", call. = FALSE)
  }

  axis <- .r2axis(axis[1])

  r.index <- index + 1
  max <- length(e$x$opts[[axis]])

  attrs <- list(...)

  if (!is.null(serie)) {
    dat <- .get_data(e, serie)
    if (inherits(dat, "numeric") || inherits(dat, "integer")) {
      rng <- range(dat)
      attrs$min <- rng[1] - margin
      attrs$max <- rng[2] + margin
    }
  }

  if (!is.null(formatter)) {
    attrs$axisLabel$formatter <- formatter
  }

  if (!length(attrs)) {
    stop("no attribute", call. = FALSE)
  }

  # initiatlise if wrong index
  if (r.index > max) {
    r.index <- 1
  }

  if (!e$x$tl) {
    dp <- .list_depth(e$x$opts[[axis]])
  } else {
    dp <- .list_depth(e$x$opts$baseOption[[axis]])
  }

  if (dp >= 2) {
    for (i in seq_along(attrs)) {
      arg <- names(attrs)[i]
      if (!e$x$tl) {
        e$x$opts[[axis]][[r.index]][[arg]] <- attrs[[i]]
      } else {
        e$x$opts$baseOption[[axis]][[r.index]][[arg]] <- attrs[[i]]
      }
    }
  } else {
    for (i in seq_along(attrs)) {
      arg <- names(attrs)[i]
      if (!e$x$tl) {
        e$x$opts[[axis]][[arg]] <- attrs[[i]]
      } else {
        e$x$opts$baseOption[[axis]][[arg]] <- attrs[[i]]
      }
    }
  }

  e
}

#' @rdname axis
#' @export
e_x_axis_ <- function(e, serie = NULL, index = 0, formatter = NULL, margin = 0, ...) {
  if (missing(e)) {
    stop("missing e", call. = FALSE)
  }
  e_axis_(e, serie, "x", index, formatter, margin = margin, ...)
}

#' @rdname axis
#' @export
e_y_axis_ <- function(e, serie = NULL, index = 0, formatter = NULL, margin = 0, ...) {
  if (missing(e)) {
    stop("missing e", call. = FALSE)
  }
  e_axis_(e = e, serie = serie, axis = "y", index = index, formatter, margin = margin, ...)
}

#' @rdname axis
#' @export
e_z_axis_ <- function(e, serie = NULL, index = 0, margin = 0, ...) {
  if (missing(e)) {
    stop("missing e", call. = FALSE)
  }
  e_axis_(e = e, serie = serie, axis = "z", index = index, margin = margin, ...)
}


#' @rdname axis
#' @export
e_x_axis <- function(e, serie, index = 0, formatter = NULL, margin = 0, ...) {
  if (missing(e)) {
    stop("missing e", call. = FALSE)
  }

  if (missing(serie)) {
    serie <- NULL
  } else {
    serie <- deparse(substitute(serie))
  }

  e_axis_(e, serie, "x", index, formatter, margin = margin, ...)
}

#' @rdname axis
#' @export
e_y_axis <- function(e, serie, index = 0, formatter = NULL, margin = 0, ...) {
  if (missing(e)) {
    stop("missing e", call. = FALSE)
  }

  if (missing(serie)) {
    serie <- NULL
  } else {
    serie <- deparse(substitute(serie))
  }

  e_axis_(e = e, serie = serie, axis = "y", index = index, formatter, margin = margin, ...)
}

#' @rdname axis
#' @export
e_z_axis <- function(e, serie, index = 0, margin = 0, ...) {
  if (missing(e)) {
    stop("missing e", call. = FALSE)
  }

  if (missing(serie)) {
    serie <- NULL
  } else {
    serie <- deparse(substitute(serie))
  }

  e_axis(e = e, serie = serie, axis = "z", index = index, margin = margin, ...)
}

#' @rdname axis
#' @export
e_rm_axis <- function(e, axis = c("x", "y", "z")) {
  axis <- .r2axis(axis[1])

  e$x$opts[[axis]] <- NULL

  return(e)
}

#' @rdname axis
#' @export
e_axis_formatter <- function(
  style = c("decimal", "percent", "currency"),
  digits = 0,
  locale = NULL,
  currency = "USD"
) {
  if (rstudioapi::isAvailable()) {
    warning("`e_axis_formatter` breaks the plot in RStudio, open it in your browser.", call. = FALSE)
  }

  if (is.null(locale)) {
    locale <- .get_locale()
  }

  style <- match.arg(style)
  opts <- list(
    style = style,
    minimumFractionDigits = digits,
    maximumFractionDigits = digits,
    currency = currency
  )
  htmlwidgets::JS(sprintf("function(value, index) {
        var fmt = new Intl.NumberFormat('%s', %s);
        return fmt.format(value);
    }", locale, jsonlite::toJSON(opts, auto_unbox = TRUE)))
}

#' Grid
#'
#' Customise grid.
#'
#' @inheritParams e_bar
#' @inheritParams e_axis
#'
#' @examples
#' USArrests |>
#'   e_charts(UrbanPop) |>
#'   e_line(Assault, smooth = TRUE) |>
#'   e_area(Murder, y.index = 1, x.index = 1) |>
#'   e_y_axis(gridIndex = 1) |>
#'   e_x_axis(gridIndex = 1) |>
#'   e_grid(height = "40%") |>
#'   e_grid(height = "40%", top = "55%")
#' @seealso \href{https://echarts.apache.org/en/option.html#grid}{Additional arguments}
#'
#' @export
e_grid <- function(e, index = NULL, ...) {
  if (missing(e)) {
    stop("missing e", call. = FALSE)
  }

  if (is.null(index)) {
    index <- length(e$x$opts[["grid"]]) + 1
  }

  attrs <- list(...)

  # initialise of not existing
  if (!e$x$tl) {
    if (!length(e$x$opts[["grid"]])) {
      e$x$opts$grid <- list()
    }

    e$x$opts$grid <- append(e$x$opts$grid, list(attrs))
  } else {
    if (!length(e$x$opts$baseOption[["grid"]])) {
      e$x$opts$baseOption$grid <- list()
    }

    e$x$opts$baseOption$grid <- append(e$x$opts$baseOption$grid, list(attrs))
  }

  e
}

#' Radius axis
#'
#' Customise radius axis.
#'
#' @inheritParams e_bar
#' @param serie Serie to use as axis labels.
#' @param show Whether to display the axis.
#'
#' @examples
#' df <- data.frame(x = LETTERS[1:10], y = seq(1, 20, by = 2))
#'
#' df |>
#'   e_charts(x) |>
#'   e_polar() |>
#'   e_angle_axis() |>
#'   e_radius_axis(x) |>
#'   e_bar(y, coord.system = "polar")
#' @seealso \href{https://echarts.apache.org/en/option.html#radiusAxis}{Additional arguments}
#'
#' @export
#' @name radius_axis
e_radius_axis <- function(e, serie, show = TRUE, ...) {
  if (missing(e)) {
    stop("missing e", call. = FALSE)
  }

  opts <- list(show = show, ...)

  if (!missing(serie)) {
    sr <- deparse(substitute(serie))
  }

  if (!missing(serie)) {
    opts$data <- e$x$data |>
      purrr::map(sr) |>
      unlist() |>
      unname() |>
      unique() |>
      as.list()
  }

  if (!e$x$tl) {
    e$x$opts$radiusAxis <- opts
  } else {
    e$x$opts$baseOption$radiusAxis <- opts
  }

  e
}

#' @rdname radius_axis
#' @export
e_radius_axis_ <- function(e, serie = NULL, show = TRUE, ...) {
  if (missing(e)) {
    stop("missing e", call. = FALSE)
  }

  opts <- list(show = show, ...)

  if (!is.null(serie)) {
    opts$data <- e$x$data |>
      purrr::map(serie) |>
      unlist() |>
      unname() |>
      unique() |>
      as.list()
  }

  if (e$x$tl) {
    e$x$opts$radiusAxis <- opts
  } else {
    e$x$opts$baseOption$radiusAxis <- opts
  }

  e
}

#' Angle axis
#'
#' Customise angle axis.
#'
#' @inheritParams e_bar
#' @param serie Serie to use as axis labels.
#' @param show Whether to display the axis.
#'
#' @examples
#' df <- data.frame(x = 1:100, y = seq(1, 200, by = 2))
#'
#' df |>
#'   e_charts(x) |>
#'   e_polar(FALSE) |>
#'   e_angle_axis(FALSE) |>
#'   e_radius_axis(FALSE) |>
#'   e_line(y, coord.system = "polar", smooth = TRUE) |>
#'   e_legend(show = FALSE)
#'
#' df <- data.frame(x = LETTERS[1:5], y = runif(5))
#'
#' df |>
#'   e_charts(x) |>
#'   e_polar() |>
#'   e_angle_axis(x) |>
#'   e_radius_axis() |>
#'   e_line(y, coord.system = "polar", smooth = TRUE)
#' @seealso \href{https://echarts.apache.org/en/option.html#angleAxis}{Additional arguments}
#'
#' @name angle_axis
#' @export
e_angle_axis <- function(e, serie, show = TRUE, ...) {
  if (missing(e)) {
    stop("missing e", call. = FALSE)
  }

  opts <- list(show = show, ...)

  if (!missing(serie)) {
    sr <- deparse(substitute(serie))
  }

  if (!missing(serie)) {
    opts$data <- e$x$data |>
      purrr::map(sr) |>
      unlist() |>
      unname() |>
      unique() |>
      as.list()
  }

  if (!e$x$tl) {
    e$x$opts$angleAxis <- opts
  } else {
    e$x$opts$baseOption$angleAxis <- opts
  }

  e
}

#' @rdname angle_axis
#' @export
e_angle_axis_ <- function(e, serie = NULL, show = TRUE, ...) {
  if (missing(e)) {
    stop("missing e", call. = FALSE)
  }

  opts <- list(show = show, ...)

  if (!is.null(serie)) {
    opts$data <- e$x$data |>
      purrr::map(serie) |>
      unlist() |>
      unname() |>
      unique() |>
      as.list()
  }

  if (!e$x$tl) {
    e$x$opts$angleAxis <- opts
  } else {
    e$x$opts$baseOption$angleAxis <- opts
  }

  e
}

#' Radar axis
#'
#' Radar axis setup and options.
#'
#' @inheritParams e_bar
#' @param index Index of axis to customise.
#'
#' @examples
#' df <- data.frame(
#'   x = LETTERS[1:5],
#'   y = runif(5, 1, 5),
#'   z = runif(5, 3, 7)
#' )
#'
#' df |>
#'   e_charts(x) |>
#'   e_radar(y, max = 7) |>
#'   e_radar(z) |>
#'   e_radar_opts(center = c("25%", "25%")) |>
#'   e_tooltip(trigger = "item")
#' @export
e_radar_opts <- function(e, index = 0, ...) {
  if (missing(e)) {
    stop("missing e", call. = FALSE)
  }

  r.index <- index + 1
  max <- length(e$x$opts$radar)

  attrs <- list(...)

  if (!length(attrs)) {
    stop("no attribute", call. = FALSE)
  }

  # initiatlise if wrong index
  if (r.index > max) {
    r.index <- 1
    if (!e$x$tl) {
      e$x$opts$radar <- list(list())
    } else {
      e$x$opts$baseOption$radar <- list(list())
    }
  }

  for (i in seq_along(attrs)) {
    arg <- names(attrs)[i]
    if (!e$x$tl) {
      e$x$opts$radar[[r.index]][[arg]] <- attrs[[i]]
    } else {
      e$x$opts$baseOption$radar[[r.index]][[arg]] <- attrs[[i]]
    }
  }

  e
}

#' Single Axis
#'
#' Setup single axis.
#'
#' @inheritParams e_bar
#' @param index Index of axis to customise.
#'
#' @examples
#' df <- data.frame(
#'   axis = LETTERS[1:10],
#'   value = runif(10, 3, 20),
#'   size = runif(10, 3, 20)
#' )
#'
#' df |>
#'   e_charts(axis) |>
#'   e_single_axis() |> # add the single axis
#'   e_scatter(
#'     value,
#'     size,
#'     coord_system = "singleAxis"
#'   )
#' @export
e_single_axis <- function(e, index = 0, ...) {
  if (missing(e)) {
    stop("missing e", call. = FALSE)
  }

  r.index <- index + 1

  e$x$opts$xAxis <- NULL
  e$x$opts$yAxis <- NULL

  if (!e$x$tl) {
    if (!length(e$x$opts$singleAxis)) {
      e$x$opts$singleAxis <- list(...)
    } else {
      e$x$opts$singleAxis <- append(e$x$opts$singleAxis, list(...))
    }
  } else {
    if (!length(e$x$opts$baseOption$singleAxis)) {
      e$x$opts$baseOption$singleAxis <- list(...)
    } else {
      e$x$opts$baseOption$singleAxis <- append(e$x$opts$baseOption$singleAxis, list(...))
    }
  }

  if (!is.null(e$x$mapping$x)) {
    type <- .get_type(e, e$x$mapping$x)

    if (!e$x$tl) {
      e$x$opts$singleAxis$type <- type
    } else {
      e$x$opts$baseOption$singleAxis$type <- type
    }

    if (type == "category" || type == "time") {
      vect <- c()
      for (i in seq_along(e$x$data)) {
        dat <- e$x$data[[i]] |>
          dplyr::select(e$x$mapping$x) |>
          unlist()
        dat <- unname(dat)
        dat <- as.character(dat)

        vect <- append(vect, dat)
      }

      vect <- unique(vect)

      if (!e$x$tl) {
        e$x$opts$singleAxis$data <- vect
      } else {
        e$x$opts$baseOption$singleAxis$data <- vect
      }
    }
  } else {
    warning("x not pass to e_charts", call. = FALSE)
  }

  e
}

#' Axis Labels
#'
#' Convenience function to add axis labels.
#'
#' @inheritParams e_bar
#' @param x,y Labels of axes.
#'
#' @examples
#' cars |>
#'   e_charts(speed) |>
#'   e_scatter(dist) |>
#'   e_axis_labels(
#'     x = "speed",
#'     y = "distance"
#'   )
#' @export
e_axis_labels <- function(e, x = "", y = "") {
  e |>
    e_x_axis(name = x) |>
    e_y_axis(name = y)
}

#'' Hide Grid Lines
#'
#' A convenience function to easily hide grid lines.
#'
#' @inheritParams e_bar
#' @param which Which axis grid lines to hide.
#'
#' @examples
#' cars |>
#'   e_charts(speed) |>
#'   e_scatter(dist) |>
#'   e_hide_grid_lines()
#' @export
e_hide_grid_lines <- function(e, which = c("x", "y")) {
  if ("x" %in% which) {
    if (!e$x$tl) {
      e$x$opts[["xAxis"]][[1]]$splitLine$show <- FALSE
    } else {
      e$x$opts$baseOption[["xAxis"]][[1]]$splitLine$show <- FALSE
    }
  }

  if ("y" %in% which) {
    if (!e$x$tl) {
      e$x$opts[["yAxis"]][[1]]$splitLine$show <- FALSE
    } else {
      e$x$opts$baseOption[["yAxis"]][[1]]$splitLine$show <- FALSE
    }
  }

  return(e)
}

#' Stagger Axis Labels
#'
#' Stagger axis labels.
#'
#' @inheritParams e_bar
#'
#' @examples
#' df <- data.frame(
#'   x = c("a very long label", "Another long label"),
#'   y = 1:2
#' )
#'
#' df |>
#'   e_charts(x, width = 150) |>
#'   e_bar(y) |>
#'   e_axis_stagger()
#' @export
e_axis_stagger <- function(e) {
  form <- "function(value, index){
    if(index % 2){
      return('\\n' + value)
    }

    return(value)
  }" |>
    htmlwidgets::JS()

  if (!e$x$tl) {
    e$x$opts[["xAxis"]][[1]]$axisLabel$formatter <- form
  } else {
    e$x$opts$baseOption[["xAxis"]][[1]]$axisLabel$formatter <- form
  }

  return(e)
}
