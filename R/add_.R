#' @rdname e_bar
#' @export
e_bar_ <- function(
  e,
  serie,
  bind = NULL,
  name = NULL,
  legend = TRUE,
  y_index = 0,
  x_index = 0,
  coord_system = "cartesian2d",
  ...
) {
  if (missing(e)) {
    stop("must pass e", call. = FALSE)
  }

  if (missing(serie)) {
    stop("must pass serie", call. = FALSE)
  }

  for (i in seq_along(e$x$data)) {
    .build_data2(e$x$data[[i]], e$x$mapping$x, serie) -> vector

    if (!is.null(bind)) {
      vector <- .add_bind2(e, vector, bind, i = i)
    }

    e_serie <- list(data = vector)

    if (y_index != 0) {
      e <- .set_y_axis(e, serie, y_index, i)
    }

    if (x_index != 0) {
      e <- .set_x_axis(e, x_index, i)
    }

    if (coord_system == "polar") {
      e_serie$data <- e$x$data[[i]] |>
        dplyr::select(serie) |>
        unlist() |>
        unname() |>
        as.list()
    }

    # timeline
    if (!e$x$tl) {
      nm <- .name_it(e, serie, name, i)

      opts <- list(
        name = nm,
        type = "bar",
        yAxisIndex = y_index,
        xAxisIndex = x_index,
        coordinateSystem = coord_system,
        ...
      )

      e_serie <- append(e_serie, opts)

      if (isTRUE(legend)) {
        e$x$opts$legend$data <- append(e$x$opts$legend$data, list(nm))
      }

      e$x$opts$series <- append(e$x$opts$series, list(e_serie))
    } else {
      if (isTRUE(legend)) {
        e$x$opts$legend$data <- append(e$x$opts$legend$data, list(name))
      }

      e$x$opts$options[[i]]$series <- append(e$x$opts$options[[i]]$series, list(e_serie))
    }
  }

  if (isTRUE(e$x$tl)) {
    if (is.null(name)) name <- serie

    series_opts <- list(
      name = name,
      type = "bar",
      yAxisIndex = y_index,
      xAxisIndex = x_index,
      coordinateSystem = coord_system,
      ...
    )

    if (isTRUE(legend)) {
      e$x$opts$baseOption$legend$data <- append(e$x$opts$baseOption$legend$data, list(name))
    }

    e$x$opts$baseOption$series <- append(e$x$opts$baseOption$series, list(series_opts))
  }

  e
}

#' @rdname e_line
#' @export
e_line_ <- function(
  e,
  serie,
  bind = NULL,
  name = NULL,
  legend = TRUE,
  y_index = 0,
  x_index = 0,
  coord_system = "cartesian2d",
  ...
) {
  if (missing(e)) {
    stop("must pass e", call. = FALSE)
  }

  if (missing(serie)) {
    stop("must pass serie", call. = FALSE)
  }

  if (length(e$x$facets)) {
    x_index <- e$x$facets$current
    y_index <- e$x$facets$current
    e$x$facets$current <- e$x$facets$current + 1
  }

  for (i in seq_along(e$x$data)) {

    # build JSON data
    .build_data2(e$x$data[[i]], e$x$mapping$x, serie) -> vector

    if (!is.null(bind)) {
      vector <- .add_bind2(e, vector, bind, i = i)
    }

    l <- list(
      data = vector
    )

    if (coord_system == "cartesian2d") {
      if (y_index != 0) {
        e <- .set_y_axis(e, serie, y_index, i)
      }

      if (x_index != 0) {
        e <- .set_x_axis(e, x_index, i)
      }

      if (!e$x$tl) {
        l$yAxisIndex <- y_index
        l$xAxisIndex <- x_index
      }
    } else if (coord_system == "polar") {
      l$data <- e$x$data[[i]] |>
        dplyr::select(serie) |>
        unlist() |>
        unname() |>
        as.list()
    }

    if (!e$x$tl) {
      nm <- .name_it(e, serie, name, i)

      opts <- list(
        name = nm,
        type = "line",
        coordinateSystem = coord_system,
        ...
      )

      l <- append(l, opts)

      if (isTRUE(legend)) {
        e$x$opts$legend$data <- append(e$x$opts$legend$data, list(nm))
      }

      e$x$opts$series <- append(e$x$opts$series, list(l))
    } else {
      if (isTRUE(legend)) {
        e$x$opts$legend$data <- append(e$x$opts$legend$data, list(name))
      }

      e$x$opts$options[[i]]$series <- append(e$x$opts$options[[i]]$series, list(l))
    }
  }

  if (isTRUE(e$x$tl)) {
    if (is.null(name)) name <- serie

    series_opts <- list(
      name = name,
      type = "line",
      yAxisIndex = y_index,
      xAxisIndex = x_index,
      coordinateSystem = coord_system,
      ...
    )

    if (isTRUE(legend)) {
      e$x$opts$baseOption$legend$data <- append(e$x$opts$baseOption$legend$data, list(name))
    }

    e$x$opts$baseOption$series <- append(e$x$opts$baseOption$series, list(series_opts))
  }

  e
}

#' @rdname e_area
#' @export
e_area_ <- function(
  e,
  serie,
  bind = NULL,
  name = NULL,
  legend = TRUE,
  y_index = 0,
  x_index = 0,
  coord_system = "cartesian2d",
  ...
) {
  if (missing(e)) {
    stop("must pass e", call. = FALSE)
  }

  if (missing(serie)) {
    stop("must pass serie", call. = FALSE)
  }

  for (i in seq_along(e$x$data)) {

    # build JSON data
    .build_data2(e$x$data[[i]], e$x$mapping$x, serie) -> vector

    if (!is.null(bind)) {
      vector <- .add_bind2(e, vector, bind, i = i)
    }

    l <- list(
      data = vector
    )

    if (coord_system == "cartesian2d") {
      if (y_index != 0) {
        e <- .set_y_axis(e, serie, y_index, i)
      }

      if (x_index != 0) {
        e <- .set_x_axis(e, x_index, i)
      }

      l$yAxisIndex <- y_index
      l$xAxisIndex <- x_index
    } else if (coord_system == "polar") {
      l$data <- e$x$data[[i]] |>
        dplyr::select(serie) |>
        unlist() |>
        unname() |>
        as.list()
    }

    if (!e$x$tl) {
      nm <- .name_it(e, serie, name, i)

      opts <- list(
        name = nm,
        type = "line",
        coordinateSystem = coord_system,
        areaStyle = list(),
        ...
      )

      l <- append(l, opts)

      if (isTRUE(legend)) {
        e$x$opts$legend$data <- append(e$x$opts$legend$data, list(nm))
      }

      e$x$opts$series <- append(e$x$opts$series, list(l))
    } else {
      e$x$opts$options[[i]]$series <- append(e$x$opts$options[[i]]$series, list(l))
    }
  }

  if (isTRUE(e$x$tl)) {
    if (is.null(name)) name <- serie

    series_opts <- list(
      name = name,
      type = "line",
      yAxisIndex = y_index,
      xAxisIndex = x_index,
      coordinateSystem = coord_system,
      areaStyle = list(),
      ...
    )

    if (isTRUE(legend)) {
      e$x$opts$baseOption$legend$data <- append(e$x$opts$baseOption$legend$data, list(name))
    }

    e$x$opts$baseOption$series <- append(e$x$opts$baseOption$series, list(series_opts))
  }

  e
}

#' @rdname e_step
#' @export
e_step_ <- function(
  e,
  serie,
  bind = NULL,
  step = c("start", "middle", "end"),
  fill = FALSE,
  name = NULL,
  legend = TRUE,
  y_index = 0,
  x_index = 0,
  coord_system = "cartesian2d",
  ...
) {
  if (missing(e)) {
    stop("must pass e", call. = FALSE)
  }

  if (missing(serie)) {
    stop("must pass serie", call. = FALSE)
  }

  if (!step[1] %in% c("start", "middle", "end")) {
    stop("wrong step", call. = FALSE)
  }

  for (i in seq_along(e$x$data)) {

    # build JSON data
    .build_data2(e$x$data[[i]], e$x$mapping$x, serie) -> vector

    if (!is.null(bind)) {
      vector <- .add_bind2(e, vector, bind, i = i)
    }

    l <- list(
      data = vector
    )

    if (coord_system == "cartesian2d") {
      if (y_index != 0) {
        e <- .set_y_axis(e, serie, y_index, i)
      }

      if (x_index != 0) {
        e <- .set_x_axis(e, x_index, i)
      }

      l$yAxisIndex <- y_index
      l$xAxisIndex <- x_index
    } else if (coord_system == "polar") {
      l$data <- e$x$data[[i]] |>
        dplyr::select(serie) |>
        unlist() |>
        unname() |>
        as.list()
    }

    if (!e$x$tl) {
      nm <- .name_it(e, serie, name, i)

      opts <- list(
        name = nm,
        type = "line",
        coordinateSystem = coord_system,
        step = step[1],
        ...
      )

      l <- append(l, opts)

      if (isTRUE(legend)) {
        e$x$opts$legend$data <- append(e$x$opts$legend$data, list(nm))
      }

      e$x$opts$series <- append(e$x$opts$series, list(l))
    } else {
      e$x$opts$options[[i]]$series <- append(e$x$opts$options[[i]]$series, list(l))
    }
  }

  if (isTRUE(e$x$tl)) {
    if (is.null(name)) name <- serie

    series_opts <- list(
      name = name,
      type = "line",
      yAxisIndex = y_index,
      xAxisIndex = x_index,
      coordinateSystem = coord_system,
      areaStyle = list(),
      step = step[1],
      ...
    )

    if (isTRUE(legend)) {
      e$x$opts$baseOption$legend$data <- append(e$x$opts$baseOption$legend$data, list(name))
    }

    e$x$opts$baseOption$series <- append(e$x$opts$baseOption$series, list(series_opts))
  }

  e
}

#' @rdname scatter
#' @export
e_scale <- function(x) {
  if (!inherits(x, "numeric")) {
    stop("x must be numeric")
  }

  scales::rescale(x, to = c(1, 20))
}

#' @rdname scatter
#' @export
e_scatter_ <- function(
  e,
  serie,
  size = NULL,
  bind = NULL,
  symbol = NULL,
  symbol_size = 1,
  scale = e_scale,
  scale_js = "function(data){ return data[3];}",
  name = NULL,
  coord_system = "cartesian2d",
  jitter_factor = 0,
  jitter_amount = NULL,
  legend = TRUE,
  y_index = 0,
  x_index = 0,
  rm_x = TRUE,
  rm_y = TRUE,
  ...
) {
  if (missing(serie)) {
    stop("must pass serie", call. = FALSE)
  }

  for (i in seq_along(e$x$data)) {
    if (y_index != 0) {
      e <- .set_y_axis(e, serie, y_index, i)
    }

    if (x_index != 0) {
      e <- .set_x_axis(e, x_index, i)
    }

    if (!is.null(size)) {
      xy <- .build_data_size(
        e$x$data[[i]],
        e$x$mapping$x,
        serie,
        size,
        scale,
        symbol_size,
        jitter_factor,
        jitter_amount
      )
    } else {
      xy <- .build_data_jitter(e$x$data[[i]], e$x$mapping$x, serie, jitter_factor, jitter_amount)
    }

    if (!is.null(bind)) {
      xy <- .add_bind2(e, xy, bind, i = i)
    }

    e.serie <- list(data = xy)

    if (coord_system == "polar") {
      e.serie$data <- e$x$data[[i]] |>
        dplyr::select(serie) |>
        unlist() |>
        unname() |>
        as.list()
    }

    opts <- list(
      coordinateSystem = coord_system,
      ...
    )

    if (coord_system == "cartesian2d") {
      opts$yAxisIndex <- y_index
      opts$xAxisIndex <- x_index
    } else if (coord_system == "singleAxis") {
      opts$singleAxisIndex <- x_index
    }

    if (!is.null(size)) {
      opts$symbolSize <- htmlwidgets::JS(
        scale_js
      )
    } else {
      size_total <- sum(symbol_size)

      if (size_total == 1 || size_total == 0) {
        symbol_size <- 3
      }

      opts$symbolSize <- symbol_size
    }

    if (!e$x$tl) {
      nm <- .name_it(e, serie, name, i)

      if (is.null(name) && jitter_factor != 0) {
        nm <- paste0(nm, " jitter")
      }

      add_opts <- list(
        name = nm,
        type = "scatter",
        symbol = symbol,
        ...
      )

      e.serie <- append(e.serie, add_opts)
      e.serie <- append(e.serie, opts)

      if (!coord_system %in% c("cartesian2d", "singleAxis", "polar")) {
        e <- .rm_axis(e, rm_x, "x")
        e <- .rm_axis(e, rm_y, "y")
      }

      if (isTRUE(legend)) {
        e$x$opts$legend$data <- append(e$x$opts$legend$data, list(nm))
      }

      e$x$opts$series <- append(e$x$opts$series, list(e.serie))
    } else {
      e$x$opts$options[[i]]$series <- append(e$x$opts$options[[i]]$series, list(e.serie))
    }
  }

  if (isTRUE(e$x$tl)) {
    if (is.null(name)) name <- serie

    add_opts <- list(
      name = name,
      type = "scatter",
      symbol = symbol,
      ...
    )

    if (isTRUE(legend)) {
      e$x$opts$baseOption$legend$data <- append(e$x$opts$baseOption$legend$data, list(name))
    }

    series_opts <- append(opts, add_opts)

    e$x$opts$baseOption$series <- append(e$x$opts$baseOption$series, list(series_opts))
  }

  e
}

#' @rdname scatter
#' @export
e_effect_scatter_ <- function(
  e,
  serie,
  size = NULL,
  bind = NULL,
  symbol = NULL,
  symbol_size = 1,
  scale = e_scale,
  scale_js = "function(data){ return data[3];}",
  name = NULL,
  coord_system = "cartesian2d",
  legend = TRUE,
  y_index = 0,
  x_index = 0,
  rm_x = TRUE,
  rm_y = TRUE,
  ...
) {
  if (missing(serie)) {
    stop("must pass serie", call. = FALSE)
  }

  for (i in seq_along(e$x$data)) {
    if (y_index != 0) {
      e <- .set_y_axis(e, serie, y_index, i)
    }

    if (x_index != 0) {
      e <- .set_x_axis(e, x_index, i)
    }

    if (!is.null(size)) {
      xy <- .build_data_size(e$x$data[[i]], e$x$mapping$x, serie, size, scale, symbol_size)
    } else {
      xy <- .build_data2(e$x$data[[i]], e$x$mapping$x, serie)
    }

    if (!is.null(bind)) {
      xy <- .add_bind2(e, xy, bind, i = i)
    }

    e.serie <- list(data = xy)

    if (coord_system == "polar") {
      e.serie$data <- e$x$data[[i]] |>
        dplyr::select(serie) |>
        unlist() |>
        unname() |>
        as.list()
    }

    opts <- list(
      coordinateSystem = coord_system,
      ...
    )

    if (coord_system == "cartesian2d") {
      opts$yAxisIndex <- y_index
      opts$xAxisIndex <- x_index
    } else if (coord_system == "singleAxis") {
      opts$singleAxisIndex <- x_index
    }

    if (!is.null(size)) {
      opts$symbolSize <- htmlwidgets::JS(
        scale_js
      )
    } else {
      size_total <- sum(symbol_size)

      if (size_total == 1) {
        symbol_size <- 10
      }

      opts$symbolSize <- symbol_size
    }

    if (!e$x$tl) {
      nm <- .name_it(e, serie, name, i)

      add_opts <- list(
        name = nm,
        type = "effectScatter",
        symbol = symbol,
        ...
      )

      e.serie <- append(e.serie, add_opts)
      e.serie <- append(e.serie, opts)

      if (!coord_system %in% c("cartesian2d", "singleAxis", "polar")) {
        e <- .rm_axis(e, rm_x, "x")
        e <- .rm_axis(e, rm_y, "y")
      }

      if (isTRUE(legend)) {
        e$x$opts$legend$data <- append(e$x$opts$legend$data, list(nm))
      }

      e$x$opts$series <- append(e$x$opts$series, list(e.serie))
    } else {
      e$x$opts$options[[i]]$series <- append(e$x$opts$options[[i]]$series, list(e.serie))
    }
  }

  if (isTRUE(e$x$tl)) {
    if (is.null(name)) name <- serie

    add_opts <- list(
      name = name,
      type = "effectScatter",
      symbol = symbol,
      ...
    )

    if (isTRUE(legend)) {
      e$x$opts$baseOption$legend$data <- append(e$x$opts$baseOption$legend$data, list(name))
    }

    series_opts <- append(opts, add_opts)

    e$x$opts$baseOption$series <- append(e$x$opts$baseOption$series, list(series_opts))
  }

  e
}

#' @rdname e_candle
#' @export
e_candle_ <- function(e, opening, closing, low, high, bind = NULL, name = NULL, legend = TRUE, ...) {
  if (missing(opening) || missing(closing) || missing(low) || missing(high)) {
    stop("missing inputs", call. = FALSE)
  }

  for (i in seq_along(e$x$data)) {
    data <- .build_data2(e$x$data[[i]], closing, opening, low, high)

    if (!is.null(bind)) {
      data <- .add_bind2(e, data, bind, i = i)
    }

    e.serie <- list(
      data = data
    )

    if (!e$x$tl) {
      nm <- .name_it(e, NULL, name, i)

      opts <- list(
        name = nm,
        type = "candlestick",
        ...
      )

      e.serie <- append(e.serie, opts)

      if (isTRUE(legend)) {
        e$x$opts$legend$data <- append(e$x$opts$legend$data, list(nm))
      }

      e$x$opts$series <- append(e$x$opts$series, list(e.serie))
    } else {
      e$x$opts$options[[i]]$series <- append(e$x$opts$options[[i]]$series, list(e.serie))
    }
  }


  if (isTRUE(e$x$tl)) {
    add_opts <- list(
      name = name,
      type = "candlestick",
      ...
    )

    if (isTRUE(legend)) {
      e$x$opts$baseOption$legend$data <- append(e$x$opts$baseOption$legend$data, list(name))
    }

    e$x$opts$baseOption$series <- append(e$x$opts$baseOption$series, list(add_opts))
  }

  e
}

#' @rdname e_radar
#' @export
e_radar_ <- function(
  e,
  serie,
  max = 100,
  name = NULL,
  legend = TRUE,
  rm_x = TRUE,
  rm_y = TRUE,
  ...,
  radar = list()
) {
  r.index <- 0

  if (missing(e)) {
    stop("must pass e", call. = FALSE)
  }

  if (missing(serie)) {
    stop("must pass serie", call. = FALSE)
  }

  # remove axis
  e <- .rm_axis(e, rm_x, "x")
  e <- .rm_axis(e, rm_y, "y")

  if (is.null(name)) { # defaults to column name
    name <- serie
  }

  # build JSON data
  .get_data(e, serie) -> vector

  series <- purrr::map(e$x$opts$series, "type") |>
    unlist()

  if (!"radar" %in% series) {
    serie <- list(
      type = "radar",
      data = list(list(value = vector, name = name)),
      radarIndex = r.index,
      ...
    )

    # add indicators
    e <- .add_indicators(e, r.index, max, radar = radar)

    # add serie
    e$x$opts$series <- append(e$x$opts$series, list(serie))
  } else { # append to radar
    e$x$opts$series[[grep("radar", series)]]$data <- append(
      e$x$opts$series[[grep("radar", series)]]$data,
      list(list(value = vector, name = name))
    )
  }

  if (isTRUE(legend)) {
    e$x$opts$legend$data <- append(e$x$opts$legend$data, list(name))
  }

  e
}

#' @rdname e_funnel
#' @export
e_funnel_ <- function(e, values, labels, name = NULL, legend = TRUE, rm_x = TRUE, rm_y = TRUE, ...) {
  if (missing(values) || missing(labels)) {
    stop("missing values or labels", call. = FALSE)
  }

  for (i in seq_along(e$x$data)) {

    # build JSON data
    funnel <- .build_data2(e$x$data[[i]], values)

    funnel <- .add_bind2(e, funnel, labels, i = i)

    serie <- list(data = funnel)

    opts <- list(
      name = name,
      type = "funnel",
      ...
    )

    # remove axis
    e <- .rm_axis(e, rm_x, "x")
    e <- .rm_axis(e, rm_y, "y")

    if (!e$x$tl) {
      serie <- append(serie, opts)

      # addlegend
      if (isTRUE(legend)) {
        legend <- .get_data(e, labels) |>
          as.character() |>
          unique()
        e$x$opts$legend$data <- append(e$x$opts$legend$data, legend)
      }

      e$x$opts$series <- append(e$x$opts$series, list(serie))
    } else {
      e$x$opts$options[[i]]$series <- append(e$x$opts$options[[i]]$series, list(serie))
    }
  }

  if (isTRUE(e$x$tl)) {
    if (isTRUE(legend)) {
      legend <- .get_data(e, labels) |>
        as.character() |>
        unique()
      e$x$opts$baseOption$legend$data <- append(e$x$opts$baseOption$legend$data, legend)
    }

    e$x$opts$baseOption$series <- append(e$x$opts$baseOption$series, list(opts))
  }

  e
}

#' @rdname e_sankey
#' @export
e_sankey_ <- function(e, source, target, value, layout = "none", rm_x = TRUE, rm_y = TRUE, ...) {
  if (missing(source) || missing(target) || missing(value)) {
    stop("missing source, target or values", call. = FALSE)
  }

  e <- .rm_axis(e, rm_x, "x")
  e <- .rm_axis(e, rm_y, "y")

  # build JSON data
  nodes <- .build_sankey_nodes(e$x$data[[1]], source, target)

  # build JSON data
  edges <- .build_sankey_edges(e$x$data[[1]], source, target, value)

  serie <- list(
    type = "sankey",
    orient = layout,
    data = nodes,
    links = edges,
    ...
  )

  e$x$opts$series <- append(e$x$opts$series, list(serie))
  e
}

#' @rdname e_heatmap
#' @export
e_heatmap_ <- function(
  e,
  y,
  z = NULL,
  bind = NULL,
  name = NULL,
  coord_system = "cartesian2d",
  rm_x = TRUE,
  rm_y = TRUE,
  calendar = NULL,
  ...
) {
  if (missing(y)) {
    stop("must pass y", call. = FALSE)
  }

  for (i in seq_along(e$x$data)) {
    if (!is.null(z)) {
      xyz <- .build_data2(e$x$data[[i]], e$x$mapping$x, y, z)
    } else {
      xyz <- .build_data2(e$x$data[[i]], e$x$mapping$x, y)
    }

    if (!is.null(bind)) {
      xyz <- .add_bind2(e, xyz, bind, i = i)
    }

    serie <- list(data = xyz)

    series_opts <- list(
      name = name,
      type = "heatmap",
      coordinateSystem = coord_system,
      ...
    )

    if (coord_system == "calendar") {
      if (!is.null(calendar)) {
        series_opts$calendarIndex <- calendar[i]
      } else if (length(e$x$opts$calendar) == length(e$x$data)) {
        series_opts$calendarIndex <- i - 1
      }
    }

    if (coord_system != "cartesian2d") {
      e <- .rm_axis(e, rm_x, "x")
      e <- .rm_axis(e, rm_y, "y")
    } else {
      xdata <- unique(.get_data(e, e$x$mapping$x, i))

      if (length(xdata) == 1) {
        xdata <- list(xdata)
      }

      if (!e$x$tl) {
        e$x$opts$xAxis <- list(list(data = xdata))
      } else {
        e$x$opts$baseOption$xAxis <- list(list(data = xdata))
      }

      ydata <- unique(.get_data(e, y, i))

      if (length(ydata) == 1) {
        ydata <- list(ydata)
      }

      if (!e$x$tl) {
        e$x$opts$yAxis <- list(list(data = ydata))
      } else {
        e$x$opts$baseOption$yAxis <- list(list(data = ydata))
      }
    }

    if (!e$x$tl) {
      serie <- append(serie, series_opts)

      e$x$opts$series <- append(e$x$opts$series, list(serie))
    } else { # timeline

      e$x$opts$options[[i]]$series <- append(e$x$opts$options[[i]]$series, list(serie))
    }
  }

  if (isTRUE(e$x$tl)) {
    e$x$opts$baseOption$series <- append(e$x$opts$baseOption$series, list(series_opts))
  }

  e
}

#' @rdname e_parallel
#' @export
e_parallel_ <- function(e, ..., name = NULL, rm_x = TRUE, rm_y = TRUE, opts = list()) {
  if (missing(e)) {
    stop("must pass e", call. = FALSE)
  }

  e <- .rm_axis(e, rm_x, "x")
  e <- .rm_axis(e, rm_y, "y")

  e$x$data[[1]] |>
    dplyr::select(...) -> df

  # remove names
  data <- df

  row.names(data) <- NULL
  data <- unname(data)

  data <- apply(data, 1, as.list)

  serie <- list(
    name = name,
    type = "parallel",
    data = data
  )

  serie <- append(serie, opts)

  para <- list()
  for (i in 1:ncol(df)) {
    line <- list()
    line$dim <- i - 1
    line$name <- names(df)[i]
    if (inherits(df[, i], "character") || inherits(df[, i], "factor")) {
      line$type <- "category"
      line$data <- unique(df[, i])
    }

    para[[i]] <- line
  }

  e$x$opts$series <- append(e$x$opts$series, serie)
  e$x$opts$parallelAxis <- para
  e
}

#' @rdname e_pie
#' @export
e_pie_ <- function(e, serie, name = NULL, legend = TRUE, rm_x = TRUE, rm_y = TRUE, ...) {
  if (missing(e)) {
    stop("must pass e", call. = FALSE)
  }

  if (missing(serie)) {
    stop("must pass serie", call. = FALSE)
  }

  e <- .rm_axis(e, rm_x, "x")
  e <- .rm_axis(e, rm_y, "y")

  for (i in seq_along(e$x$data)) {

    # build JSON data
    data <- .build_data2(e$x$data[[i]], serie)
    data <- .add_bind2(e, data, e$x$mapping$x, i = i)

    serie_data <- list(data = data)

    serie_opts <- list(
      name = name,
      type = "pie",
      ...
    )

    if (!e$x$tl) {
      if (is.null(name)) { # defaults to column name
        name <- serie
      }

      serie_opts$name <- name

      serie_opts <- append(serie_opts, serie_data)

      if (isTRUE(legend)) {
        e$x$opts$legend$data <- append(e$x$opts$legend$data, e$x$data[[i]][[e$x$mapping$x]])
      }

      e$x$opts$series <- append(e$x$opts$series, list(serie_opts))
    } else {
      e$x$opts$options[[i]]$series <- append(e$x$opts$options[[i]]$series, list(serie_data))
    }
  }

  if (isTRUE(e$x$tl)) {
    if (isTRUE(legend)) {
      e$x$opts$baseOption$legend$data <- append(
        e$x$opts$baseOption$legend$data,
        purrr::map(e$x$data, "model") |>
          unlist() |>
          unique()
      )
    }

    e$x$opts$baseOption$series <- append(e$x$opts$baseOption$series, list(serie_opts))
  }

  e
}

#' @rdname e_sunburst
#' @export
e_sunburst_ <- function(e, styles = NULL, names = NULL, levels = NULL, rm_x = TRUE, rm_y = TRUE, ...) {
  if (missing(e)) {
    stop("must pass e", call. = FALSE)
  }

  e <- .rm_axis(e, rm_x, "x")
  e <- .rm_axis(e, rm_y, "y")

  # build JSON data
  data <- .build_sun(e, styles, names, levels)

  serie <- list(
    type = "sunburst",
    data = data,
    ...
  )

  e$x$opts$series <- append(e$x$opts$series, list(serie))
  e
}

#' @rdname e_treemap
#' @export
e_treemap_ <- function(e, styles = NULL, names = NULL, levels = NULL, rm_x = TRUE, rm_y = TRUE, ...) {
  if (missing(e)) {
    stop("must pass e", call. = FALSE)
  }

  e <- .rm_axis(e, rm_x, "x")
  e <- .rm_axis(e, rm_y, "y")

  # build JSON data
  data <- .build_sun(e, styles, names, levels)

  serie <- list(
    type = "treemap",
    data = data,
    ...
  )

  e$x$opts$series <- append(e$x$opts$series, list(serie))
  e
}

#' @rdname e_river
#' @export
e_river_ <- function(e, serie, name = NULL, legend = TRUE, rm_x = TRUE, rm_y = TRUE, ...) {
  if (missing(e)) {
    stop("must pass e", call. = FALSE)
  }

  if (missing(serie)) {
    stop("must pass serie", call. = FALSE)
  }

  for (i in seq_along(e$x$data)) {
    nm <- .name_it(e, serie, name, i)

    if (length(e$x$opts$xAxis$data)) {
      e$X <- e$x$opts$xAxis$data
    }

    # build JSON data
    data <- .build_river(e, serie, nm, i)

    if (!length(e$x$opts$series)) {
      e.serie <- list(
        type = "themeRiver",
        data = data,
        ...
      )
      e$x$opts$series <- append(e$x$opts$series, list(e.serie))
    } else {
      e$x$opts$series[[1]]$data <- append(e$x$opts$series[[1]]$data, data)
    }

    e <- .rm_axis(e, rm_x, "x")
    e <- .rm_axis(e, rm_y, "y")

    e$x$opts$singleAxis <- list(type = "time")

    if (isTRUE(legend)) {
      e$x$opts$legend$data <- append(e$x$opts$legend$data, list(nm))
    }
  }

  e
}

#' @rdname e_boxplot
#' @export
e_boxplot_ <- function(e, serie, name = NULL, outliers = TRUE, ...) {
  if (missing(serie)) {
    stop("must pass serie", call. = FALSE)
  }

  for (i in seq_along(e$x$data)) {

    # build JSON data
    vector <- .build_boxplot(e, serie, i)

    if (!e$x$tl) {
      nm <- .name_it(e, serie, name, i)

      if (length(e$x$opts$series) >= 1) {
        e$x$opts$series[[1]]$data <- append(
          e$x$opts$series[[1]]$data,
          list(vector)
        )
      } else {
        # boxplot + opts
        box <- list(
          name = nm,
          type = "boxplot",
          data = list(vector),
          ...
        )
        e$x$opts$series <- append(e$x$opts$series, list(box))
      }

      # data/outliers
      if (isTRUE(outliers)) {
        e <- .add_outliers(e, serie, i)
      }

      # xaxis
      xs <- names(e$x$data)[i]
      if (is.null(xs)) {
        xs <- serie
      }
      e$x$opts$xAxis[[1]]$data <- append(e$x$opts$xAxis[[1]]$data, list(xs))
      e$x$opts$xAxis[[1]]$type <- "category"
    } else {
      e$x$opts$options[[i]]$series <- append(e$x$opts$options[[i]]$series, list(list(data = vector)))
    }
  }

  if (isTRUE(e$x$tl)) {
    serie_opts <- list(
      type = "boxplot",
      ...
    )

    e$x$opts$baseOption$series <- append(e$x$opts$baseOption$series, list(serie_opts))
  }

  e
}

#' @rdname e_tree
#' @export
e_tree_ <- function(e, rm_x = TRUE, rm_y = TRUE, ...) {
  if (missing(e)) {
    stop("must pass e", call. = FALSE)
  }

  # remove axis
  e <- .rm_axis(e, rm_x, "x")
  e <- .rm_axis(e, rm_y, "y")

  # build JSON data
  data <- .build_tree(e)

  serie <- list(
    type = "tree",
    data = data,
    ...
  )

  e$x$opts$series <- append(e$x$opts$series, list(serie))
  e
}

#' @rdname line3D
#' @export
e_lines_3d_ <- function(
  e,
  source_lon,
  source_lat,
  target_lon,
  target_lat,
  source_name = NULL,
  target_name = NULL,
  value = NULL,
  name = NULL,
  coord_system = "globe",
  rm_x = TRUE,
  rm_y = TRUE,
  ...
) {
  if (missing(e)) {
    stop("must pass e", call. = FALSE)
  }

  if (missing(source_lat) || missing(source_lon) || missing(target_lat) || missing(target_lon)) {
    stop("missing coordinates", call. = FALSE)
  }

  if (missing(source_name)) {
    source_name <- NULL
  }

  if (missing(target_name)) {
    target_name <- NULL
  }

  if (missing(value)) {
    value <- NULL
  }

  # remove axis
  e <- .rm_axis(e, rm_x, "x")
  e <- .rm_axis(e, rm_y, "y")

  for (i in seq_along(e$x$data)) {
    data <- .map_lines(
      e,
      source_lon,
      source_lat,
      target_lon,
      target_lat,
      source_name,
      target_name,
      value,
      i = i
    )

    serie_data <- list(data = data)

    serie_opts <- list(
      type = "lines3D",
      coordinateSystem = coord_system,
      ...
    )

    if (!e$x$tl) {
      serie <- append(serie_data, serie_opts)

      nm <- .name_it(e, NULL, name, i)

      if (!is.null(nm)) {
        e$x$opts$legend$data <- append(e$x$opts$legend$data, list(nm))
        serie$name <- nm
      }

      e$x$opts$series <- append(e$x$opts$series, list(serie))
    } else {
      if (!is.null(name)) {
        serie_opts$name <- name
      }

      e$x$opts$options[[i]]$series <- append(e$x$opts$options[[i]]$series, list(serie_data))
    }
  }

  if (isTRUE(e$x$tl)) {
    e$x$opts$baseOption$series <- append(e$x$opts$baseOption$series, list(serie_opts))
    e$x$opts$baseOption$legend$data <- append(e$x$opts$baseOption$legend$data, list(name))
  }

  # add dependency
  path <- system.file("htmlwidgets/lib/echarts-4.8.0", package = "echarts4r")
  dep <- htmltools::htmlDependency(
    name = "echarts-gl",
    version = "1.1.2",
    src = c(file = path),
    script = "echarts-gl.min.js"
  )

  e$dependencies <- append(e$dependencies, list(dep))

  e
}

#' @rdname line3D
#' @export
e_line_3d_ <- function(e, y, z, name = NULL, coord_system = NULL, rm_x = TRUE, rm_y = TRUE, ...) {
  if (missing(e)) {
    stop("must pass e", call. = FALSE)
  }

  if (missing(y) || missing(z)) {
    stop("missing coordinates", call. = FALSE)
  }

  # remove axis
  e <- .rm_axis(e, rm_x, "x")
  e <- .rm_axis(e, rm_y, "y")

  for (i in seq_along(e$x$data)) {

    # build JSON data
    data <- .build_data2(e$x$data[[i]], e$x$mapping$x, y, z)

    e <- .set_axis_3D(e, "x", e$x$mapping$x, 0)
    e <- .set_axis_3D(e, "y", y, 0)
    e <- .set_axis_3D(e, "z", z, 0)

    if (!e$x$tl) {
      nm <- .name_it(e, NULL, name, i)

      if (!is.null(nm)) {
        e$x$opts$legend$data <- append(e$x$opts$legend$data, nm)
      }

      if (!length(e$x$opts$zAxis3D)) {
        e$x$opts$zAxis3D <- list(list(show = TRUE))
      }

      if (!length(e$x$opts$grid3D)) {
        e$x$opts$grid3D <- list(list(show = TRUE))
      }

      e.serie <- list(
        type = "line3D",
        data = data,
        name = nm,
        ...
      )

      if (!is.null(coord_system)) {
        e.serie$coordinateSystem <- coord_system
      }

      e$x$opts$series <- append(e$x$opts$series, list(e.serie))
    } else {
      if (!length(e$x$opts$zAxis3D)) {
        e$x$opts$baseOption$zAxis3D <- list(list(show = TRUE))
      }

      if (!length(e$x$opts$grid3D)) {
        e$x$opts$baseOption$grid3D <- list(list(show = TRUE))
      }

      e_serie <- list(data = data)

      e$x$opts$options[[i]]$series <- append(e$x$opts$options[[i]]$series, list(e_serie))
    }
  }

  if (e$x$tl) {
    serie_opts <- list(
      type = "line3D",
      name = name,
      ...
    )

    if (!is.null(coord_system)) {
      serie_opts$coordinateSystem <- coord_system
    }

    if (!is.null(name)) {
      e$x$opts$baseOption$legend$data <- append(e$x$opts$baseOption$legend$data, name)
    }

    e$x$opts$baseOption$series <- append(e$x$opts$baseOption$series, list(serie_opts))
  }

  # add dependency
  path <- system.file("htmlwidgets/lib/echarts-4.8.0", package = "echarts4r")
  dep <- htmltools::htmlDependency(
    name = "echarts-gl",
    version = "1.1.2",
    src = c(file = path),
    script = "echarts-gl.min.js"
  )

  e$dependencies <- append(e$dependencies, list(dep))

  e
}

#' @rdname e_bar_3d
#' @export
e_bar_3d_ <- function(e, y, z, bind = NULL, coord_system = "cartesian3D", name = NULL, rm_x = TRUE, rm_y = TRUE, ...) {
  if (missing(e)) {
    stop("must pass e", call. = FALSE)
  }

  if (missing(y) || missing(z)) {
    stop("must pass y and z", call. = FALSE)
  }

  # remove axis
  e <- .rm_axis(e, rm_x, "x")
  e <- .rm_axis(e, rm_y, "y")
  e <- .set_axis_3D(e, "x", e$x$mapping$x, 0)
  e <- .set_axis_3D(e, "y", y, 0)
  e <- .set_axis_3D(e, "z", z, 0)

  for (i in seq_along(e$x$data)) {
    if (coord_system != "cartesian3D") {
      data <- .build_data2(e$x$data[[i]], e$x$mapping$x, y, z)

      if (!is.null(bind)) {
        data <- .add_bind2(e, data, bind, i = i)
      }
    } else {
      data <- .build_cartesian3D(e, e$x$mapping$x, y, z, i = i)
    }

    if (!e$x$tl) {

      # globe
      if (coord_system == "cartesian3D") { # cartesian

        if (!length(e$x$opts$zAxis3D)) {
          e$x$opts$zAxis3D <- list(list(show = TRUE))
        }

        if (!length(e$x$opts$grid3D)) {
          e$x$opts$grid3D <- list(list(show = TRUE))
        }
      }

      nm <- .name_it(e, NULL, name, i)

      if (!is.null(nm)) {
        e$x$opts$legend$data <- append(e$x$opts$legend$data, nm)
      }

      e.serie <- list(
        name = nm,
        type = "bar3D",
        coordinateSystem = coord_system,
        data = data,
        ...
      )

      e$x$opts$series <- append(e$x$opts$series, list(e.serie))
    } else {

      # globe
      if (coord_system == "cartesian3d") { # cartesian

        if (!length(e$x$opts$zAxis3D)) {
          e$x$opts$baseOption$zAxis3D <- list(list(show = TRUE))
        }

        if (!length(e$x$opts$grid3D)) {
          e$x$opts$baseOption$grid3D <- list(list(show = TRUE))
        }
      }

      e_serie <- list(data = data)

      e$x$opts$options[[i]]$series <- append(e$x$opts$options[[i]]$series, list(e_serie))
    }
  }

  if (e$x$tl) {
    serie_opts <- list(
      name = name,
      type = "bar3D",
      coordinateSystem = coord_system,
      ...
    )

    if (!is.null(name)) {
      e$x$opts$baseOption$legend$data <- append(e$x$opts$legend$data, name)
    }

    e$x$opts$baseOption$series <- append(e$x$opts$baseOption$series, list(serie_opts))
  }

  # add dependency
  path <- system.file("htmlwidgets/lib/echarts-4.8.0", package = "echarts4r")
  dep <- htmltools::htmlDependency(
    name = "echarts-gl",
    version = "1.1.2",
    src = c(file = path),
    script = "echarts-gl.min.js"
  )

  e$dependencies <- append(e$dependencies, list(dep))

  e
}

#' @rdname e_surface
#' @export
e_surface_ <- function(e, y, z, bind = NULL, name = NULL, rm_x = TRUE, rm_y = TRUE, ...) {
  if (missing(e)) {
    stop("must pass e", call. = FALSE)
  }

  if (missing(y) || missing(z)) {
    stop("must pass y and z", call. = FALSE)
  }

  # remove axis
  e <- .rm_axis(e, rm_x, "x")
  e <- .rm_axis(e, rm_y, "y")

  if (!length(e$x$opts$zAxis3D)) {
    e$x$opts$zAxis3D <- list(type = "value")
  }

  if (!length(e$x$opts$yAxis3D)) {
    e$x$opts$yAxis3D <- list(type = "value")
  }

  if (!length(e$x$opts$xAxis3D)) {
    e$x$opts$xAxis3D <- list(type = "value")
  }

  if (!length(e$x$opts$grid3D)) {
    e$x$opts$grid3D <- list(show = TRUE)
  }

  for (i in seq_along(e$x$data)) {
    row.names(e$x$data[[i]]) <- NULL

    data <- e$x$data[[i]] |>
      dplyr::select(e$x$mapping$x, y, z)
    
    data <- unname(data)

    data <- apply(data, 1, as.list)

    e.serie <- list(
      type = "surface",
      data = data,
      ...
    )

    if (!is.null(name)) e.serie$name <- name

    e$x$opts$series <- append(e$x$opts$series, list(e.serie))
  }

  # add dependency
  path <- system.file("htmlwidgets/lib/echarts-4.8.0", package = "echarts4r")
  dep <- htmltools::htmlDependency(
    name = "echarts-gl",
    version = "1.1.2",
    src = c(file = path),
    script = "echarts-gl.min.js"
  )

  e$dependencies <- append(e$dependencies, list(dep))

  e
}

#' @rdname e_lines
#' @export
e_lines_ <- function(
  e,
  source_lon,
  source_lat,
  target_lon,
  target_lat,
  source_name = NULL,
  target_name = NULL,
  value = NULL,
  coord_system = "geo",
  name = NULL,
  rm_x = TRUE,
  rm_y = TRUE,
  ...
) {
  if (missing(e)) {
    stop("must pass e", call. = FALSE)
  }

  if (missing(source_lat) || missing(source_lon) || missing(target_lat) || missing(target_lon)) {
    stop("missing coordinates", call. = FALSE)
  }

  # remove axis
  e <- .rm_axis(e, rm_x, "x")
  e <- .rm_axis(e, rm_y, "y")


  for (i in seq_along(e$x$data)) {
    data <- .map_lines(e, source_lon, source_lat, target_lon, target_lat, source_name, target_name, value, i)

    e.serie <- list(data = data)

    e.opts <- list(
      type = "lines",
      coordinateSystem = coord_system,
      ...
    )

    if (!e$x$tl) {
      e.opts$name <- .name_it(e, NULL, name, i)

      e_serie <- append(e.serie, e.opts)

      e$x$opts$series <- append(e$x$opts$series, list(e_serie))
    } else {
      e$x$opts$options[[i]]$series <- append(e$x$opts$options[[i]]$series, list(e.serie))
    }
  }

  if (e$x$tl) {
    e$x$opts$baseOption$series <- append(e$x$opts$baseOption$series, list(e.opts))
  }

  e
}

#' @rdname e_scatter_3d
#' @export
e_scatter_3d_ <- function(
  e,
  y,
  z,
  color = NULL,
  size = NULL,
  bind = NULL,
  coord_system = "cartesian3D",
  name = NULL,
  rm_x = TRUE,
  rm_y = TRUE,
  legend = FALSE,
  ...
) {
  if (missing(e)) {
    stop("must pass e", call. = FALSE)
  }

  if (missing(y) || missing(z)) {
    stop("must pass y and z", call. = FALSE)
  }

  # remove axis
  e <- .rm_axis(e, rm_x, "x")
  e <- .rm_axis(e, rm_y, "y")

  for (i in seq_along(e$x$data)) {

    # globe
    if (coord_system != "cartesian3D") {
      data <- .build_data2(e$x$data[[i]], e$x$mapping$x, y, z)

      if (!is.null(bind)) {
        data <- .add_bind2(e, data, bind, i = i)
      }
    } else { # cartesian

      if (!e$x$tl) {
        if (!length(e$x$opts$zAxis3D)) {
          e$x$opts$zAxis3D <- list(list(show = TRUE))
        }

        if (!length(e$x$opts$grid3D)) {
          e$x$opts$grid3D <- list(list(show = TRUE))
        }
      } else {
        if (!length(e$x$opts$zAxis3D)) {
          e$x$opts$baseOption$zAxis3D <- list(list(show = TRUE))
        }

        if (!length(e$x$opts$grid3D)) {
          e$x$opts$baseOption$grid3D <- list(list(show = TRUE))
        }
      }

      e <- .set_axis_3D(e, "x", e$x$mapping$x, 0)
      e <- .set_axis_3D(e, "y", y, 0)
      e <- .set_axis_3D(e, "z", z, 0)

      if (is.null(color)) {
        data <- .build_data2(e$x$data[[i]], e$x$mapping$x, y, z)
      } else if (!is.null(color) && is.null(size)) {
        data <- .build_data2(e$x$data[[i]], e$x$mapping$x, y, z, color)
      } else if (!is.null(color) && !is.null(size)) {
        data <- .build_data2(e$x$data[[i]], e$x$mapping$x, y, z, color, size)
      }

      if (!is.null(bind)) {
        data <- .add_bind2(e, data, bind, i = i)
      }
    }

    e_data <- list(data = data)

    e_serie <- list(
      name = name,
      type = "scatter3D",
      coordinateSystem = coord_system,
      ...
    )

    if (!e$x$tl) {
      nm <- .name_it(e, NULL, name, i)
      e_serie$name <- nm

      e_serie <- append(e_serie, e_data)

      e$x$opts$series <- append(e$x$opts$series, list(e_serie))
    } else {
      e$x$opts$options[[i]]$series <- append(e$x$opts$options[[i]]$series, list(e_data))
    }
  }

  if (e$x$tl) {
    if (isTRUE(legend)) {
      e$x$opts$baseOption$legend$data <- append(e$x$opts$baseOption$legend$data, list(nm))
    }

    e$x$opts$baseOption$series <- append(e$x$opts$baseOption$series, list(e_serie))
  }

  # add dependency
  path <- system.file("htmlwidgets/lib/echarts-4.8.0", package = "echarts4r")
  dep <- htmltools::htmlDependency(
    name = "echarts-gl",
    version = "1.1.2",
    src = c(file = path),
    script = "echarts-gl.min.js"
  )

  e$dependencies <- append(e$dependencies, list(dep))

  e
}

#' @rdname e_flow_gl
#' @export
e_flow_gl_ <- function(e, y, sx, sy, color = NULL, name = NULL, coord_system = NULL, rm_x = TRUE, rm_y = TRUE, ...) {
  if (missing(e)) {
    stop("must pass e", call. = FALSE)
  }

  e$x$mainOpts$renderer <- "webgl"

  if (missing(y) || missing(sx) || missing(sy)) {
    stop("must pass y, sx, and sy", call. = FALSE)
  }

  e$x$opts$xAxis <- list(type = "value")
  e$x$opts$yAxis <- list(type = "value")

  if (is.null(color)) {
    data <- .build_data(e, e$x$mapping$x, y, sx, sy)
  } else {
    data <- .build_data(e, e$x$mapping$x, y, sx, sy, color)
  }

  serie <- list(
    type = "flowGL",
    data = data,
    ...
  )

  if (!is.null(name)) {
    serie$name <- name
  }

  e$x$opts$series <- append(e$x$opts$series, list(serie))

  # add dependency
  path <- system.file("htmlwidgets/lib/echarts-4.8.0", package = "echarts4r")
  dep <- htmltools::htmlDependency(
    name = "echarts-gl",
    version = "1.1.2",
    src = c(file = path),
    script = "echarts-gl.min.js"
  )

  e$dependencies <- append(e$dependencies, list(dep))

  e
}

#' @rdname e_scatter_gl
#' @export
e_scatter_gl_ <- function(e, y, z, name = NULL, coord_system = "geo", rm_x = TRUE, rm_y = TRUE, ...) {
  if (missing(e)) {
    stop("must pass e", call. = FALSE)
  }

  e$x$mainOpts$renderer <- "webgl"

  if (missing(y) || missing(z)) {
    stop("must pass y and z", call. = FALSE)
  }

  for (i in seq_along(e$x$data)) {

    # remove axis
    e <- .rm_axis(e, rm_x, "x")
    e <- .rm_axis(e, rm_y, "y")

    data <- .build_data2(e$x$data[[i]], e$x$mapping$x, y, z)

    serie <- list(data = data)

    serie_opts <- list(
      name = name,
      type = "scatterGL",
      coordinateSystem = coord_system,
      ...
    )

    # globe
    if (coord_system == "cartesian3D") {
      if (!e$x$tl) {
        if (!length(e$x$opts$zAxis3D)) {
          e$x$opts$zAxis3D <- list(list(show = TRUE))
        }

        if (!length(e$x$opts$grid3D)) {
          e$x$opts$grid3D <- list(list(show = TRUE))
        }
      } else {
        if (!length(e$x$opts$zAxis3D)) {
          e$x$opts$baseOption$zAxis3D <- list(list(show = TRUE))
        }

        if (!length(e$x$opts$grid3D)) {
          e$x$opts$baseOption$grid3D <- list(list(show = TRUE))
        }
      }

      e <- .set_axis_3D(e, "x", e$x$mapping$x, 0)
      e <- .set_axis_3D(e, "y", y, 0)
      e <- .set_axis_3D(e, "z", z, 0)
    }

    if (!e$x$tl) {
      serie_opts$name <- .name_it(e, z, name, i)

      serie <- append(serie, serie_opts)

      e$x$opts$series <- append(e$x$opts$series, list(serie))
    } else {
      e$x$opts$options[[i]]$series <- append(e$x$opts$options[[i]]$series, list(serie))
    }
  }

  if (e$x$tl) {
    e$x$opts$baseOption$series <- append(e$x$opts$baseOption$series, list(serie_opts))
  }

  # add dependency
  path <- system.file("htmlwidgets/lib/echarts-4.8.0", package = "echarts4r")
  dep <- htmltools::htmlDependency(
    name = "echarts-gl",
    version = "1.1.2",
    src = c(file = path),
    script = "echarts-gl.min.js"
  )

  e$dependencies <- append(e$dependencies, list(dep))

  e
}

#' @rdname e_pictorial
#' @export
e_pictorial_ <- function(e, serie, symbol, bind = NULL, name = NULL, legend = TRUE, y_index = 0, x_index = 0, ...) {
  if (missing(e)) {
    stop("must pass e", call. = FALSE)
  }

  if (missing(serie) || missing(symbol)) {
    stop("must pass serie and symbol", call. = FALSE)
  }

  if (y_index != 0) {
    e <- .set_y_axis(e, serie, y_index)
  }

  if (x_index != 0) {
    e <- .set_x_axis(e, x_index)
  }

  for (i in seq_along(e$x$data)) {
    if (is.null(name)) { # defaults to column name
      name <- serie
    }

    # build JSON data
    vector <- .build_data2(e$x$data[[i]], e$x$mapping$x, serie)

    if (!is.null(bind)) {
      vector <- .add_bind2(e, vector, bind, i = i)
    }

    if (symbol %in% colnames(e$x$data[[1]])) {
      vector <- .add_bind2(e, vector, symbol, col = "symbol", i = i)
    }

    serie_data <- list(data = vector)

    serie_opts <- list(
      name = name,
      type = "pictorialBar",
      yAxisIndex = y_index,
      xAxisIndex = x_index,
      ...
    )

    if (!e$x$tl) {
      serie_opts <- append(serie_data, serie_opts)

      if (!symbol %in% colnames(e$x$data[[i]])) {
        serie_opts$symbol <- symbol
      }

      if (isTRUE(legend)) {
        e$x$opts$legend$data <- append(e$x$opts$legend$data, list(name))
      }

      e$x$opts$series <- append(e$x$opts$series, list(serie_opts))
    } else {
      e$x$opts$options[[i]]$series <- append(e$x$opts$options[[i]]$series, list(serie_data))
    }
  }

  if (isTRUE(e$x$tl)) {
    if (isTRUE(legend)) {
      e$x$opts$baseOption$legend$data <- append(e$x$opts$baseOption$legend$data, list(name))
    }

    e$x$opts$baseOption$series <- append(e$x$opts$baseOption$series, list(serie_opts))
  }


  e
}

#' @rdname histogram
#' @export
e_histogram_ <- function(
  e,
  serie,
  breaks = "Sturges",
  name = NULL,
  legend = TRUE,
  bar_width = "90%",
  x_index = 0,
  y_index = 0,
  ...
) {
  if (missing(e)) {
    stop("must pass e", call. = FALSE)
  }

  if (missing(serie)) {
    stop("must pass serie", call. = FALSE)
  }

  for (i in seq_along(e$x$data)) {
    data <- .get_data(e, serie, i)
    histogram <- hist(data, plot = FALSE, breaks)

    hist <- data.frame(
      histogram$mids,
      histogram$counts
    )

    hist <- apply(unname(hist), 1, as.list)

    if (y_index != 0) {
      e <- .set_y_axis(e, serie, y_index)
    }

    if (x_index != 0) {
      e <- .set_x_axis(e, x_index)
    }

    serie_data <- list(data = hist)

    serie_opts <- list(
      name = name,
      type = "bar",
      barWidth = bar_width,
      yAxisIndex = y_index,
      xAxisIndex = x_index,
      stack = "stackedHistogram",
      ...
    )

    if (!e$x$tl) {
      nm <- .name_it(e, NULL, name, i)

      serie_opts$name <- nm

      if (!length(e$x$opts$xAxis)) {
        e$x$opts$xAxis <- list(
          list(
            type = "value",
            scale = TRUE
          )
        )
      }

      e.serie <- append(serie_data, serie_opts)

      if (isTRUE(legend)) {
        e$x$opts$legend$data <- append(e$x$opts$legend$data, list(nm))
      }

      e$x$opts$series <- append(e$x$opts$series, list(e.serie))
    } else {
      e$x$opts$options[[i]]$series <- append(e$x$opts$options[[i]]$series, list(serie_data))
    }
  }

  if (isTRUE(e$x$tl)) {
    if (!length(e$x$opts$baseOption$xAxis)) {
      e$x$opts$baseOption$xAxis <- list(
        list(
          type = "value",
          scale = TRUE
        )
      )
    }

    if (isTRUE(legend)) {
      e$x$opts$baseOption$legend$data <- append(e$x$opts$baseOption$legend$data, list(name))
    }

    e$x$opts$baseOption$series <- append(e$x$opts$baseOption$series, list(serie_opts))
  }

  e
}

#' @rdname histogram
#' @export
e_density_ <- function(
  e,
  serie,
  breaks = "Sturges",
  name = NULL,
  legend = TRUE,
  x_index = 0,
  y_index = 0,
  smooth = TRUE,
  ...
) {
  if (missing(e)) {
    stop("must pass e", call. = FALSE)
  }

  if (missing(serie)) {
    stop("must pass serie", call. = FALSE)
  }

  for (i in seq_along(e$x$data)) {
    data <- .get_data(e, serie, i = i)
    histogram <- hist(data, plot = FALSE, breaks)

    hist <- data.frame(
      histogram$mids,
      histogram$density
    )

    hist <- apply(unname(hist), 1, as.list)

    if (y_index != 0) {
      e <- .set_y_axis(e, serie, y_index)
    }

    if (x_index != 0) {
      e <- .set_x_axis(e, x_index)
    }

    serie_data <- list(data = hist)

    serie_opts <- list(
      name = name,
      type = "line",
      yAxisIndex = y_index,
      xAxisIndex = x_index,
      smooth = smooth,
      ...
    )

    if (!"areaStyle" %in% names(serie_opts)) {
      serie_opts$areaStyle <- list()
    }

    if (!e$x$tl) {
      nm <- .name_it(e, serie, name, i)

      serie_opts$name <- nm

      if (!length(e$x$opts$xAxis)) {
        e$x$opts$xAxis <- list(
          list(
            type = "value",
            scale = TRUE
          )
        )
      }

      series <- append(serie_opts, serie_data)

      if (isTRUE(legend)) {
        e$x$opts$legend$data <- append(e$x$opts$legend$data, list(nm))
      }

      e$x$opts$series <- append(e$x$opts$series, list(series))
    } else {
      e$x$opts$options[[i]]$series <- append(e$x$opts$options[[i]]$series, list(serie_data))
    }
  }

  if (isTRUE(e$x$tl)) {
    if (!length(e$x$opts$baseOption$xAxis)) {
      e$x$opts$baseOption$xAxis <- list(
        list(
          type = "value",
          scale = TRUE
        )
      )
    }

    if (isTRUE(legend)) {
      e$x$opts$baseOption$legend$data <- append(e$x$opts$baseOption$legend$data, list(name))
    }

    e$x$opts$baseOption$series <- append(e$x$opts$baseOption$series, list(serie_opts))
  }

  e
}

#' @rdname band
#' @export
e_band_ <- function(
  e,
  min,
  max,
  stack = "confidence-band",
  symbol = c("none", "none"),
  areaStyle = list(list(color = "rgba(0,0,0,0)"), list()),
  legend = list(FALSE, FALSE),
  ...
) {
  if (missing(e)) {
    stop("must pass e", call. = FALSE)
  }

  if (missing(min) || missing(max)) {
    stop("must pass min and max", call. = FALSE)
  }

  args <- list(...)

  args$lineStyle <- list(
    list(normal = list(opacity = 0)),
    list(normal = list(opacity = 0))
  )
  args$symbol <- symbol
  args$areaStyle <- areaStyle
  args$legend <- legend

  spl <- function(x, index) {
    x[[index]]
  }

  min_opts <- purrr::map(args, spl, 1)
  max_opts <- purrr::map(args, spl, 2)

  for (i in seq_along(e$x$data)) {

    # min
    min_opts_index <- min_opts

    min_opts_index$e <- e
    min_opts_index$stack <- stack
    min_opts_index$serie <- min

    e <- do.call(e_line_, min_opts_index)

    # max
    e$x$data[[i]][, max] <- e$x$data[[i]][, max] - e$x$data[[i]][, min]
    max_opts_index <- max_opts
    max_opts_index$e <- e
    max_opts_index$stack <- stack
    max_opts_index$serie <- max

    e <- do.call(e_line_, max_opts_index)
  }

  e |>
    e_x_axis(type = "category")
}

#' @rdname band2
#' @export
e_band2_ <- function(
  e,
  lower,
  upper,
  name = NULL,
  legend = TRUE,
  y_index = 0,
  x_index = 0,
  coord_system = "cartesian2d",
  itemStyle = list(borderWidth = 0.5),
  ...
) {
  if (missing(e)) {
    stop("must pass e", call. = FALSE)
  }
  if (missing(lower) || missing(upper)) {
    stop("must pass lower, or upper", call. = FALSE)
  }
  if (coord_system != "cartesian2d") {
    stop("only cartesian2d supported", call. = FALSE)
  }

  args <- list(...)

  for (i in seq_along(e$x$data)) {
    vector <- .build_data2(
      e$x$data[[i]],
      e$x$mapping$x,
      lower,
      upper
    )
    e_serie <- list(data = vector)
    if (y_index != 0) {
      e <- .set_y_axis(e, upper, y_index, i)
    }
    if (x_index != 0) {
      e <- .set_x_axis(e, x_index, i)
    }

    nm <- .name_it(e, paste0(lower, ".", upper), name, i)

    if (!e$x$tl) {
      opts <- list(
        name = nm,
        type = "custom",
        yAxisIndex = y_index,
        xAxisIndex = x_index,
        coordinateSystem = coord_system,
        itemStyle = itemStyle,
        renderItem = htmlwidgets::JS("renderBand"),
        encode = list(x = 0, y = list(1, 2)),
        ...
      )

      e_serie <- append(opts, e_serie) # data after renderItem, data used for Y-sizing only

      if (isTRUE(legend)) {
        e$x$opts$legend$data <- append(e$x$opts$legend$data, list(nm))
      }
      e$x$opts$series <- append(e$x$opts$series, list(e_serie))
    }
    else {
      if (isTRUE(legend)) {
        e$x$opts$legend$data <- append(
          e$x$opts$legend$data,
          list(nm)
        )
      }
      e$x$opts$options[[i]]$series <- append(
        e$x$opts$options[[i]]$series,
        list(e_serie)
      )
    }
  }
  if (isTRUE(e$x$tl)) {
    series_opts <- list(
      name = name,
      type = "custom",
      yAxisIndex = y_index,
      xAxisIndex = x_index,
      coordinateSystem = coord_system,
      itemStyle = itemStyle,
      renderItem = htmlwidgets::JS("renderBand"),
      encode = list(x = 0, y = list(1, 2)),
      ...
    )

    if (isTRUE(legend)) {
      e$x$opts$baseOption$legend$data <- append(e$x$opts$baseOption$legend$data, list(name))
    }
    e$x$opts$baseOption$series <- append(
      e$x$opts$baseOption$series,
      list(series_opts)
    )
  }
  path <- system.file("htmlwidgets/lib/echarts-4.8.0/custom", package = "echarts4r")
  dep <- htmltools::htmlDependency(name = "echarts-renderers", version = "1.0.2", src = c(file = path), script = "renderers.js")

  e$dependencies <- append(e$dependencies, list(dep))
  e
}

#' @rdname errorbar
#' @export
e_error_bar_ <- function(
  e,
  lower,
  upper,
  name = NULL,
  legend = FALSE,
  y_index = 0,
  x_index = 0,
  coord_system = "cartesian2d",
  itemStyle = list(borderWidth = 1.5),
  renderer = "renderErrorBar2",
  ...
) {
  if (missing(e)) {
    stop("must pass e", call. = FALSE)
  }
  if (missing(lower) || missing(upper)) {
    stop("must pass lower and upper", call. = FALSE)
  }

  args <- list(...)

  ser <- if (e$x$tl) e$x$opts$baseOption$series else e$x$opts$series
  # look for a name for timeline only
  if (is.null(name) & e$x$tl) {
    name <- unlist(lapply(ser, function(x) {
      return(x$name)
    }))[1]
  }

  # look for barGap(s), barCategoryGap(s)
  allBarGaps <- lapply(ser, function(x) {
    x$barGap
  })
  allBarCgGaps <- lapply(ser, function(x) {
    x$barCategoryGap
  })
  lbg <- utils::tail(unlist(allBarGaps), 1)
  lbg <- if (is.null(lbg)) "" else lbg
  lcg <- utils::tail(unlist(allBarCgGaps), 1)
  lcg <- if (is.null(lcg)) "" else lcg
  tmp <- NULL
  if (!is.null(name)) {
    tmp <- unlist(lapply(ser, function(x) {
      if (length(grep(name, x)) > 0) x$type else NULL
    }))[1]
  }
  if (!is.null(tmp)) { # attached by name, count same types
    info <- length(unlist(lapply(ser, function(x) grep(tmp, x))))
  } else { # no name - choose bar or line but not both
    info <- length(unlist(lapply(ser, function(x) grep("bar", x))))
    if (info == 0) info <- length(unlist(lapply(ser, function(x) grep("line", x))))
    if (info == 0) info <- length(unlist(lapply(ser, function(x) grep("scatter", x))))
  }

  if (info == 0) {
    return(e)
  } # no bars/lines/scatter, nothing to attach to

  # save minimal info to be read by renderErrorBar2
  # renderers.js works in a very isolated environment, so we send data thru sessionStorage
  # info is last barGap, last barCategoryGap, number of bars
  info <- c(lbg, lcg, as.character(info))

  info <- paste0(
    "sessionStorage.setItem('ErrorBar.oss','",
    jsonlite::toJSON(info),
    "'); ",
    renderer
  )
  renderJS <- htmlwidgets::JS(info)

  for (i in seq_along(e$x$data)) {
    vector <- .build_data2(
      e$x$data[[i]],
      e$x$mapping$x,
      lower,
      upper
    )
    e_serie <- list(data = vector)
    if (y_index != 0) {
      e <- .set_y_axis(e, upper, y_index, i)
    }
    if (x_index != 0) {
      e <- .set_x_axis(e, x_index, i)
    }
    if (coord_system == "polar") {
      e_serie$data <- e$x$data[[i]] |>
        dplyr::select(
          lower,
          upper
        ) |>
        unlist() |>
        unname() |>
        as.list()
    }
    nm <- .name_it(e, ser[[i]]$name, name, i)

    if (!e$x$tl) {
      opts <- list(
        name = nm,
        type = "custom",
        yAxisIndex = y_index,
        xAxisIndex = x_index,
        coordinateSystem = coord_system,
        itemStyle = itemStyle,
        renderItem = renderJS,
        encode = list(x = 0, y = list(1, 2)),
        ...
      )
      if (!("z" %in% names(args))) opts$z <- 3
      if (!("color" %in% names(args))) opts$color <- "black" # set, or it will blend with main bar

      e_serie <- append(e_serie, opts)
      if (isTRUE(legend)) {
        e$x$opts$legend$data <- append(e$x$opts$legend$data, list(nm))
      }
      e$x$opts$series <- append(e$x$opts$series, list(e_serie))
    }
    else {
      if (isTRUE(legend)) {
        e$x$opts$legend$data <- append(e$x$opts$legend$data, list(nm))
      }
      e$x$opts$options[[i]]$series <- append(
        e$x$opts$options[[i]]$series,
        list(e_serie)
      )
    }
  }
  if (isTRUE(e$x$tl)) {
    series_opts <- list(
      type = "custom",
      yAxisIndex = y_index,
      xAxisIndex = x_index,
      coordinateSystem = coord_system,
      itemStyle = itemStyle,
      renderItem = renderJS,
      encode = list(x = 0, y = list(1, 2)),
      ...
    )
    if (!is.null(name)) series_opts$name <- name
    if (!("z" %in% names(args))) series_opts$z <- 3
    if (!("color" %in% names(args))) series_opts$color <- "black" # set, or it will blend with main bar

    if (isTRUE(legend) && !is.null(name)) {
      e$x$opts$baseOption$legend$data <- append(e$x$opts$baseOption$legend$data, list(name))
    }
    e$x$opts$baseOption$series <- append(
      e$x$opts$baseOption$series,
      list(series_opts)
    )
  }
  path <- system.file("htmlwidgets/lib/echarts-4.8.0/custom", package = "echarts4r")
  dep <- htmltools::htmlDependency(name = "echarts-renderers", version = "1.0.2", src = c(file = path), script = "renderers.js")

  e$dependencies <- append(e$dependencies, list(dep))
  e |> e_x_axis(type = "category") # wont work with type 'value'
}
