
#' @rdname e_cloud
#' @export
e_cloud_ <- function(e, word, freq, color = NULL, rm_x = TRUE, rm_y = TRUE, ...) {
  if (missing(e)) {
    stop("must pass e", call. = FALSE)
  }

  e <- .rm_axis(e, rm_x, "x")
  e <- .rm_axis(e, rm_y, "y")

  data <- .build_data(e, freq)
  data <- .add_bind(e, data, word)

  if (!is.null(color)) {
    color <- .get_data(e, color)
    for (i in seq_along(data)) {
      col <- list(
        color = color[i]
      )
      data[[i]]$textStyle <- col
    }
  }

  serie <- list(
    type = "wordCloud",
    data = data,
    ...
  )

  e$x$opts$series <- append(e$x$opts$series, list(serie))

  # add dependency
  path <- system.file("htmlwidgets/lib/echarts-6.1.0/plugins", package = "echarts4r")
  dep <- htmltools::htmlDependency(
    name = "echarts-wordcloud",
    version = "1.0.0",
    src = c(file = path),
    script = "echarts-wordcloud.min.js"
  )

  e$dependencies <- append(e$dependencies, list(dep))

  e
}


#' @rdname e_liquid
#' @export
e_liquid_ <- function(e, serie, color = NULL, rm_x = TRUE, rm_y = TRUE, ...) {
  if (missing(e)) {
    stop("must pass e", call. = FALSE)
  }

  e <- .rm_axis(e, rm_x, "x")
  e <- .rm_axis(e, rm_y, "y")

  data <- .get_data(e, serie) |>
    unlist() |>
    unname()

  serie <- list(
    type = "liquidFill",
    data = data,
    ...
  )

  if (!is.null(color)) {
    serie$color <- .get_data(e, color) |>
      unlist() |>
      unname()
  }

  e$x$opts$series <- append(e$x$opts$series, list(serie))

  # add dependency
  path <- system.file("htmlwidgets/lib/echarts-6.1.0/plugins", package = "echarts4r")
  dep <- htmltools::htmlDependency(
    name = "echarts-liquidfill",
    version = "1.0.0",
    src = c(file = path),
    script = "echarts-liquidfill.min.js"
  )

  e$dependencies <- append(e$dependencies, list(dep))

  e
}

#' @rdname e_barRange
#' @export
e_barRange_ <- function(e,
                       lower,
                       upper,
                       name = "barRange",
                       legend = TRUE,
                       y_index = 0,
                       x_index = 0,
                       barWidth = 10,
                       borderRadius = 5,
                       margin = 10,
                       textSymbol = "F",
                       ...){

  if (missing(e)) {
    stop("must pass e", call. = FALSE)
  }

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

    if (!e$x$tl) {
      # generate series list
      serie <- list(
        type = "custom",
        renderItem = 'barRange',
        name = name,
        encode = list(x= 0,
                      y = c(1,2),
                      tooltip = c(1,2)),
        data = vector,
        itemPayload = list(
          textSymbol = textSymbol,
          barWidth = barWidth,
          borderRadius = borderRadius,
          margin = margin
        ),
        ...
      )
      e$x$opts$series <- append(e$x$opts$series, list(serie))

      if (isTRUE(legend)) {
        current_trace <- length(e$x$opts$series)
        e$x$opts$series[[current_trace]]$name <- name
        e$x$opts$legend$data <- append(e$x$opts$legend$data, list(name))
      }
      e$x$opts$series <- append(e$x$opts$series, list(e_serie))
    }
    else {
      if (isTRUE(legend)) {
        e$x$opts$legend$data <- append(
          e$x$opts$legend$data,
          list(name)
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
      type = "custom",
      renderItem = 'barRange',
      name = name,
      encode = list(x= 0,
                    y = c(1,2),
                    tooltip = c(1,2)),
      data = vector,
      itemPayload = list(
        textSymbol = textSymbol,
        barWidth = barWidth,
        borderRadius = borderRadius,
        margin = margin
      ),
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

  # In the js I replaced "°C" with 't.itemPayload.textSymbol' since the symbol
  # was hard-coded
  path <- system.file("htmlwidgets/lib/echarts-6.1.0/plugins", package = "echarts4r")
  dep <- htmltools::htmlDependency(
    name = "echarts-barRange",
    version = "1.0.0",
    src = c(file = path),
    script = "echarts-barRange.js")

  e$dependencies <- append(e$dependencies, list(dep))

  e
}

#' @rdname e_contour
#' @export
e_contour_ <- function(e,
                      serie,
                      name = "contour",
                      legend = TRUE,
                      thresholds = 8,
                      bandwidth = 20,
                      lineStyle = list(opacity = 0.3, color = "black", width=1),
                      contourOpacity = 0.8,
                      contourColors = list('#5470c6', '#91cc75', '#fac858', '#ee6666'),
                       ...){

  if (missing(e)) {
    stop("must pass e", call. = FALSE)
  }

  for (i in seq_along(e$x$data)) {
    vector <-   .build_data2(e$x$data[[i]], e$x$mapping$x, serie)

    e_serie <- list(data = vector)

    e <- e |> e_axis(type = 'value')

    if (!e$x$tl) {
      opts <- list(
        type = "custom",
        renderItem = 'contour',
        encode = list(x= 0,
                      y = 1,
                      tooltip = 2),
        data = vector,
        name = name,
        itemPayload = list(
          thresholds = thresholds,
          bandwidth = bandwidth,
          lineStyle = lineStyle,
          itemStyle = list(opacity = contourOpacity,
                           color = contourColors)
        ),
        ...
      )

      e_serie <- append(opts, e_serie) # data after renderItem, data used for Y-sizing only

      if (isTRUE(legend)) {
        e$x$opts$legend$data <- append(e$x$opts$legend$data, list(name))
      }
      e$x$opts$series <- append(e$x$opts$series, list(e_serie))

    }
    else {
      if (isTRUE(legend)) {
        e$x$opts$legend$data <- append(
          e$x$opts$legend$data,
          list(name)
        )
      }
      e$x$opts$options[[i]]$series <- append(
        e$x$opts$options[[i]]$series,
        list(e_serie)
      )
    }
  }
  if (isTRUE(e$x$tl)) {
    # generate series list
    series_opts <- list(
      type = "custom",
      renderItem = 'contour',
      encode = list(x= 0,
                    y = 1,
                    tooltip = 2),
      data = vector,
      name = name,
      itemPayload = list(
        thresholds = thresholds,
        bandwidth = bandwidth,
        lineStyle = lineStyle,
        itemStyle = list(opacity = contourOpacity,
                         color = contourColors)
      ),
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

  path_d3 <- system.file("htmlwidgets/lib/d3", package = "echarts4r")
  dep_d3 <- htmltools::htmlDependency(
    name = "d3",
    version = "7.9.0",
    src = c(file = path_d3),
    script = "d3.min.js")

  e$dependencies <- append(e$dependencies, list(dep_d3))

  path <- system.file("htmlwidgets/lib/echarts-6.1.0/plugins", package = "echarts4r")
  dep <- htmltools::htmlDependency(
    name = "echarts-contour",
    version = "1.0.0",
    src = c(file = path),
    script = "echarts-contour.js")

  e$dependencies <- append(e$dependencies, list(dep))

  e
}

#' @rdname e_lineRange
#' @export
e_lineRange_ <- function(e,
                       lower,
                       upper,
                       name = "lineRange",
                       legend = TRUE,
                       y_index = 0,
                       x_index = 0,
                       lineStyle = list(opacity = 0.3, color = "#000", width=1),
                       areaStyle = list(opacity = 0.3, color = "#032", width=1),
                       ...){

  if (missing(e)) {
    stop("must pass e", call. = FALSE)
  }
  if (missing(lower) || missing(upper)) {
    stop("must pass lower, or upper", call. = FALSE)
  }

  if (e$x$tl) {
    stop("timeline not supported", call. = FALSE)
  }

  # Extract the color from areaStyle to use in legend
  legend_color <- if (!is.null(areaStyle$color)) areaStyle$color else "#032"

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

      opts <- list(
        type = "custom",
        name = name,
        renderItem = 'lineRange',
        encode = list(x= 0,
                      y = c(1,2),
                      tooltip = c(1,2)),
        data = vector,
        itemPayload = list(
          areaStyle = areaStyle,
          lineStyle = lineStyle
        ),
         ...
      )

      e_serie <- append(opts, e_serie) # data after renderItem, data used for Y-sizing only

      if (isTRUE(legend)) {
        legend_entry <- list(
          name = name,
          icon = "rect",  # Use rectangle icon
          itemStyle = list(color = legend_color)
        )
        e$x$opts$legend$data <- append(e$x$opts$legend$data, list(legend_entry))
      }
      e$x$opts$series <- append(e$x$opts$series, list(e_serie))

  }
  path <- system.file("htmlwidgets/lib/echarts-6.1.0/plugins", package = "echarts4r")
  dep <- htmltools::htmlDependency(
    name = "echarts-lineRange",
    version = "1.0.0",
    src = c(file = path),
    script = "echarts-lineRange.js")

  e$dependencies <- append(e$dependencies, list(dep))

  e
}

#' @rdname e_stage
#' @export
e_stage_ <- function(e,
                      start,
                      end,
                      stage,
                      name = "stage",
                      legend = TRUE,                      
                      borderRadius = 8,
                      verticalMargin = 10,
                      minHorizontalSize = 3,
                      envelope = list(show = TRUE, color = "#888", opacity = 0.25, externalRadius = 8 ),
                      axisLabel = list(formatter = NULL, color = "#8A8A8A"),
                      ...){

  if (missing(e)) {
    stop("must pass e", call. = FALSE)
  }

  data <- e$x$data

  for (i in seq_along(data)) {

    df <- data[[i]]
    start_col <- start
    end_col <- end
    stage_col <- stage

    vector <- lapply(seq_len(nrow(df)), function(j) {
      list(
        df[[start_col]][[j]],
        df[[end_col]][[j]],
        df[[stage_col]][[j]]
      )
    })

    e_serie <- list(data = vector)

    if (!e$x$tl) {
      opts <- list(
        type = "custom",
        renderItem = 'stage',
        colorBy = 'data',
        encode = list(
            x = c(0, 1),
            y = 2,
            tooltip = c(0, 1)),
        name = name,
        itemPayload = list(
          borderRadius = borderRadius,
          verticalMargin = verticalMargin,
          minHorizontalSize = minHorizontalSize,
          envelope = envelope,
          axisLabel = axisLabel
        ),
         ...
      )
      e$x$opts$series <- append(e$x$opts$series, list(opts))

      if (isTRUE(legend)) {
        e$x$opts$legend$data <- append(e$x$opts$legend$data, list(name))
      }

      e$x$opts$dataset <- list(source = vector, sourceHeader = FALSE)

    }
    else {
      if (isTRUE(legend)) {
        e$x$opts$legend$data <- append(
          e$x$opts$legend$data,
          list(name)
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
      type = "custom",
      renderItem = 'stage',
      colorBy = 'data',
      encode = list(
          x = c(0, 1),
          y = 2,
          tooltip = c(0, 1)),
      name = name,
      itemPayload = list(
        borderRadius = borderRadius,
        verticalMargin = verticalMargin,
        minHorizontalSize = minHorizontalSize,
        envelope = envelope,
        axisLabel = axisLabel
      ),
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

  path <- system.file("htmlwidgets/lib/echarts-6.1.0/plugins", package = "echarts4r")
  dep <- htmltools::htmlDependency(
    name = "echarts-stage",
    version = "1.0.0",
    src = c(file = path),
    script = "echarts-stage.js")

  e$dependencies <- append(e$dependencies, list(dep))

  e
}