#' Wordcloud
#'
#' Draw a wordcloud.
#'
#' @inheritParams e_bar
#' @param word,freq Terms and their frequencies.
#' @param color Word color.
#' @param rm_x,rm_y Whether to remove x and y axis, defaults to \code{TRUE}.
#'
#' @examples
#' words <- function(n = 5000) {
#'   a <- do.call(paste0, replicate(5, sample(LETTERS, n, TRUE), FALSE))
#'   paste0(a, sprintf("%04d", sample(9999, n, TRUE)), sample(LETTERS, n, TRUE))
#' }
#'
#' tf <- data.frame(
#'   terms = words(100),
#'   freq = rnorm(100, 55, 10)
#' ) |>
#'   dplyr::arrange(-freq)
#'
#' tf |>
#'   e_color_range(freq, color) |>
#'   e_charts() |>
#'   e_cloud(terms, freq, color, shape = "circle", sizeRange = c(3, 15))
#' @seealso \href{https://github.com/ecomfe/echarts-wordcloud}{official documentation}
#'
#' @rdname e_cloud
#' @export
e_cloud <- function(e, word, freq, color, rm_x = TRUE, rm_y = TRUE, ...) {
  if (missing(e)) {
    stop("missing e", call. = FALSE)
  }

  if (!missing(color)) {
    cl <- deparse(substitute(color))
  } else {
    cl <- NULL
  }

  e_cloud_(e, deparse(substitute(word)), deparse(substitute(freq)), cl, rm_x, rm_y, ...)
}

#' @rdname e_cloud
#' @export
e_cloud_ <- function(e, word, freq, color = NULL, rm_x = TRUE, rm_y = TRUE, ...) {
  if (missing(e)) {
    stop("missing e", call. = FALSE)
  }

  e <- .rm_axis(e, rm_x, "x")
  e <- .rm_axis(e, rm_y, "y")

  data <- .build_data(e, freq)
  data <- .add_bind(e, data, word)

  if (!is.null(color)) {
    color <- .get_data(e, color)
    for (i in 1:length(data)) {
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
  path <- system.file("htmlwidgets/lib/echarts-6.0.0/plugins", package = "echarts4r")
  dep <- htmltools::htmlDependency(
    name = "echarts-wordcloud",
    version = "1.0.0",
    src = c(file = path),
    script = "echarts-wordcloud.min.js"
  )

  e$dependencies <- append(e$dependencies, list(dep))

  e
}

#' Liquid fill
#'
#' Draw liquid fill.
#'
#' @inheritParams e_bar
#' @param color Color to plot.
#' @param rm_x,rm_y Whether to remove x and y axis, defaults to \code{TRUE}.
#'
#' @examples
#' df <- data.frame(val = c(0.6, 0.5, 0.4))
#'
#' df |>
#'   e_charts() |>
#'   e_liquid(val) |>
#'   e_theme("dark")
#' @seealso \href{https://github.com/ecomfe/echarts-liquidfill}{official documentation}
#'
#' @rdname e_liquid
#' @export
e_liquid <- function(e, serie, color, rm_x = TRUE, rm_y = TRUE, ...) {
  if (missing(e)) {
    stop("missing e", call. = FALSE)
  }

  if (!missing(color)) {
    cl <- deparse(substitute(color))
  } else {
    cl <- NULL
  }

  e_liquid_(e, deparse(substitute(serie)), cl, rm_x, rm_y, ...)
}

#' @rdname e_liquid
#' @export
e_liquid_ <- function(e, serie, color = NULL, rm_x = TRUE, rm_y = TRUE, ...) {
  if (missing(e)) {
    stop("missing e", call. = FALSE)
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
  path <- system.file("htmlwidgets/lib/echarts-6.0.0/plugins", package = "echarts4r")
  dep <- htmltools::htmlDependency(
    name = "echarts-liquidfill",
    version = "1.0.0",
    src = c(file = path),
    script = "echarts-liquidfill.min.js"
  )

  e$dependencies <- append(e$dependencies, list(dep))

  e
}

#' Modularity
#'
#' Graph modularity extension will do community detection and partian a graph's vertices in several subsets.
#' Each subset will be assigned a different color.
#'
#' @inheritParams e_bar
#' @param modularity Either set to \code{TRUE}, or a \code{list}.
#'
#' @section Modularity:
#' \itemize{
#'   \item{\code{resolution} Resolution}
#'   \item{\code{sort} Whether to sort to comunities}
#' }
#'
#' @examples
#' nodes <- data.frame(
#'   name = paste0(LETTERS, 1:100),
#'   value = rnorm(100, 10, 2),
#'   stringsAsFactors = FALSE
#' )
#'
#' edges <- data.frame(
#'   source = sample(nodes$name, 200, replace = TRUE),
#'   target = sample(nodes$name, 200, replace = TRUE),
#'   stringsAsFactors = FALSE
#' )
#'
#' e_charts() |>
#'   e_graph() |>
#'   e_graph_nodes(nodes, name, value) |>
#'   e_graph_edges(edges, source, target) |>
#'   e_modularity(
#'     list(
#'       resolution = 5,
#'       sort = TRUE
#'     )
#'   )
#' @note Does not work in RStudio viewer, open in browser.
#'
#' @seealso \href{https://github.com/ecomfe/echarts-graph-modularity}{Official documentation}
#'
#' @export
e_modularity <- function(e, modularity = TRUE) {
  clu <- list(
    modularity = modularity
  )

  e$x$opts$series[[length(e$x$opts$series)]]$modularity <- clu

  # add dependency
  path <- system.file("htmlwidgets/lib/echarts-6.0.0/plugins", package = "echarts4r")
  dep <- htmltools::htmlDependency(
    name = "echarts-modularity",
    version = "1.0.0",
    src = c(file = path),
    script = "echarts-graph-modularity.min.js"
  )

  e$dependencies <- append(e$dependencies, list(dep))

  e
}

#' Segmented Doughnut
#'
#' Draw segmented doughnut.
#'
#' @inheritParams e_bar
#' @param numerator,denominator numeraetor to provide filled segments and denominator for total segments.
#' @param formatter javascript string formatter for center text of chart.
#' @param fontSize,fontColor font values for center text of chart.
#' @param center,radius center provides relative position of the center of chart while radius provides the radius of your circle for outer segments.
#' @param rm_x,rm_y Whether to remove x and y axis, defaults to \code{TRUE}.
#'
#' @examples
#'
#'
#' e_chart() |>
#'  e_doughnut(numerator = 3, denominator = 6)
#'
#' @seealso \href{https://github.com/apache/echarts-custom-series/tree/main/custom-series/segmentedDoughnut}{official documentation}
#'
#' @rdname e_doughnut
#' @export
e_doughnut <- function(e,
                        numerator = NULL,
                        denominator = NULL,
                        formatter = "{c}/{b}",
                        fontSize = "10em",
                        fontColor = "#555",
                        center = c("50%","50%"),
                        radius = c("50%","65%"),
                        rm_x = TRUE,
                        rm_y = TRUE,
                        ...) {

  if (missing(e)) {
    stop("missing e", call. = FALSE)
  }

  e <- .rm_axis(e, rm_x, "x")
  e <- .rm_axis(e, rm_y, "y")

  serie <- list(
    type = "custom",
    renderItem = "segmentedDoughnut",
    coordinateSystem = "none",
    itemPayload = list(center = as.list(center),
                       radius = as.list(radius),
                       segmentCount = denominator,
                       label = list(
                         show = TRUE,
                         formatter = formatter,
                         fontSize = fontSize,
                         color = fontColor
                       )
    ),
    data = list(numerator),
    ...
  )

  e$x$opts$series <- append(e$x$opts$series, list(serie))

  # add dependency
  path <- system.file("htmlwidgets/lib/echarts-6.0.0/plugins", package = "echarts4r")
  dep <- htmltools::htmlDependency(
    name = "echarts-doughnut",
    version = "1.0.0",
    src = c(file = path),
    script = "echarts-doughnut.js")

  e$dependencies <- append(e$dependencies, list(dep))

  e
}

#' Violin chart
#'
#' Draw a violin chart with scattered dots.
#'
#' @inheritParams e_band2_
#' @param symbolSize overall size of symbol.
#' @param areaOpacity opacity of violin area.
#' @param bandWidthScale scale for the amplitude of violin area
#' @param binCount number of bins for violin plot. More bins will provide a more detailed version of the plot,
#'
#' @examples
#'
#' PlantGrowth |>
#'   e_charts(group) |>
#'   e_scatter(weight) |>
#'   e_violin(binCount = 200)
#'
#' @seealso \href{https://github.com/apache/echarts-custom-series/tree/main/custom-series/violin}{official documentation}
#'
#' @rdname e_violin
#' @export
e_violin <- function(e,
                     name = "barRange",
                     legend = TRUE,
                     y_index = 0,
                     x_index = 0,
                     symbolSize = 10,
                     areaOpacity = 0.5,
                     binCount = 100,
                     bandWidthScale = 1,
                     ...){

  if (missing(e)) {
    stop("missing e", call. = FALSE)
  }

  if(e$x$opts$series[[1]]$type != 'scatter' || is.null(e$x$opts$series[[1]]$type)){
    stop("violin is only supported with scatter plots")
  }

 for (i in seq_along(e$x$opts$series)) {

  # adjust scatter data to necessary format for violin polygons
    vector <- list()
    for(j in 1:length(e$x$opts$series[[i]]$data)){
      vector <- append(vector,list(as.list(e$x$opts$series[[i]]$data[[j]]$value)))
    }
    e_serie <- list(data = vector)

    if (y_index != 0) {
      e <- .set_y_axis(e, substitute(upper), y_index, i)
    }
    if (x_index != 0) {
      e <- .set_x_axis(e, x_index, i)
    }
      serie <- list(
        type = "custom",
        renderItem = 'violin',
        silent = TRUE,
        itemPayload = list(
          symbolSize = symbolSize,
          areaOpacity = areaOpacity,
          binCount = binCount,
          bandWidthScale = bandWidthScale
        ),
        ...
      )

      e$x$opts$series <- append(e$x$opts$series, list(serie))

      if (isTRUE(legend)) {
        # Adding these 2 lines and legend is added
        current_trace <- length(e$x$opts$series)
        e$x$opts$series[[current_trace]]$name = name
        e$x$opts$legend$data <- append(e$x$opts$legend$data, list(name))
      }
      e$x$opts$dataset <- list(source = vector)

  }

    # add dependency
    path <- system.file("htmlwidgets/lib/echarts-6.0.0/plugins", package = "echarts4r")
    dep <- htmltools::htmlDependency(
      name = "echarts-violin",
      version = "1.0.0",
      src = c(file = path),
      script = "echarts-violin.js")

    e$dependencies <- append(e$dependencies, list(dep))

    e
}

# WORKS WITH TIMELINE
# df <- iris |>
#   dplyr::group_by(Species) |>
#   dplyr::summarise(min_length = min(Sepal.Length),
#                    max_length = max(Sepal.Length))
# df |> dplyr::group_by(Species) |> e_chart(Species, timeline = TRUE) |>
#   e_barRange(lower = min_length,
#              upper = max_length,
#              textSymbol = '"'
#   ) |> e_timeline_serie(
#     title = list(
#       list(text = "setosa"),
#       list(text = "versicolor"),
#       list(text = "virginica")
#     )
#   )

#' Bar range chart
#'
#' Draw a bar range plot with labels on each end.
#'
#' @inheritParams e_band2_
#' @param barWidth width of each bar
#' @param borderRadius roundness of the bar ends
#' @param margin space between the text labels and the bar
#' @param textSymbol string for the label to end with
#'
#' @examples
#' df <- iris |>
#'   dplyr::group_by(Species) |>
#'   dplyr::summarise(min_length = min(Sepal.Length),
#'                    max_length = max(Sepal.Length))
#'
#' df |> e_chart(Species) |>
#'   e_barRange(lower = min_length,
#'              upper = max_length,
#'              textSymbol = '"'
#'   )
#' @seealso \href{https://github.com/apache/echarts-custom-series/tree/main/custom-series/barRange}{official documentation}
#'
#' @rdname e_barRange
#' @export
e_barRange <- function(e,
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
    stop("missing e", call. = FALSE)
  }

  for (i in seq_along(e$x$data)) {

    vector <- .build_data2(
      e$x$data[[i]],
      e$x$mapping$x,
      deparse(substitute(lower)),
      deparse(substitute(upper))
    )
    e_serie <- list(data = vector)

    if (y_index != 0) {
      e <- .set_y_axis(e, substitute(upper), y_index, i)
    }
    if (x_index != 0) {
      e <- .set_x_axis(e, x_index, i)
    }

    if (!e$x$tl) {
      # generate series list
      serie <- list(
        type = "custom",
        renderItem = 'barRange',
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
        # Adding these 2 lines and legend is added
        current_trace <- length(e$x$opts$series)
        e$x$opts$series[[current_trace]]$name = name
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
  path <- system.file("htmlwidgets/lib/echarts-6.0.0/plugins", package = "echarts4r")
  dep <- htmltools::htmlDependency(
    name = "echarts-barRange",
    version = "1.0.0",
    src = c(file = path),
    script = "echarts-barRange.js")

  e$dependencies <- append(e$dependencies, list(dep))

  e
}

# timeline works
# mtcars |>
#   dplyr::group_by(vs) |>
#   e_charts(mpg, timeline = TRUE) |>
#   e_contour(serie = mpg) |>
#   e_timeline_serie(
#         title = list(
#           list(text = "a"),
#           list(text = "b")
#         )
#   )

#' Contour chart
#'
#' Draw a contour plot. x and y must each be numbers.
#'
#' @inheritParams e_bar
#' @param thresholds contour density
#' @param bandwidth the size of the contours
#' @param lineStyle list of properties of the line
#' @param contourOpacity opacity of the countours
#' @param contourColors colors used for the contours
#'
#' @examples
#'
#' mtcars |>
#'  e_charts(mpg) |>
#'  e_contour(serie = mpg)
#' @seealso \href{https://github.com/apache/echarts-custom-series/tree/main/custom-series/contour}{official documentation}
#'
#' @rdname e_contour
#' @export
e_contour <- function(e,
                      serie,
                      name = "contour",
                      legend = TRUE,
                      y_index = 0,
                      x_index = 0,
                      thresholds = 8,
                      bandwidth = 20,
                      lineStyle = list(opacity = 0.3, color = "black", width=1),
                      contourOpacity = 0.8,
                      contourColors = list('#5470c6', '#91cc75', '#fac858', '#ee6666'),
                       ...){

  if (missing(e)) {
    stop("missing e", call. = FALSE)
  }

  for (i in seq_along(e$x$data)) {
    vector <-   .build_data2(e$x$data[[i]], e$x$mapping$x, deparse(substitute(serie)))

    e_serie <- list(data = vector)

    e <- e |> e_axis(type = 'value')

    if (y_index != 0) {
      e <- .set_y_axis(e, substitute(upper), y_index, i)
    }
    if (x_index != 0) {
      e <- .set_x_axis(e, x_index, i)
    }

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

        # current_trace <- length(e$x$opts$series)
        # e$x$opts$series[[current_trace]]$name = name

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

  path <- system.file("htmlwidgets/lib/echarts-6.0.0/plugins", package = "echarts4r")
  dep <- htmltools::htmlDependency(
    name = "echarts-contour",
    version = "1.0.0",
    src = c(file = path),
    script = "echarts-contour.js")

  e$dependencies <- append(e$dependencies, list(dep))

  e
}

# TODO legend color not changing
#' Line range chart
#'
#' Draw a line range area plot.
#'
#' @inheritParams e_band2_
#' @param lineStyle properties of the border lines
#' @param areaStyle properties of the area between the lines
#'
#' @examples
#'
#' df <- iris |>
#'   dplyr::group_by(Species) |>
#'   dplyr::summarise(lower = min(Sepal.Length),
#'                    upper = max(Sepal.Length))
#' df |>
#'   e_chart(Species) |>
#'   e_lineRange(lower = lower, upper = upper)
#' @seealso \href{https://github.com/apache/echarts-custom-series/tree/main/custom-series/lineRange}{official documentation}
#'
#' @rdname e_lineRange
#' @export
e_lineRange <- function(e,
                       lower,
                       upper,
                       name = "lineRange",
                       legend = TRUE,
                       y_index = 0,
                       x_index = 0,
                       lineStyle = list(opacity = 0.3, color = "black", width=1),
                       areaStyle = list(opacity = 0.3, color = "red", width=1),
                       ...){

  if (missing(e)) {
    stop("missing e", call. = FALSE)
  }
  if (missing(lower) || missing(upper)) {
    stop("must pass lower, or upper", call. = FALSE)
  }

  if (e$x$tl) {
    stop("timeline not supported", call. = FALSE)
  }

  for (i in seq_along(e$x$data)) {
    vector <- .build_data2(
      e$x$data[[i]],
      e$x$mapping$x,
      deparse(substitute(lower)),
      deparse(substitute(upper))
    )
    e_serie <- list(data = vector)

    if (y_index != 0) {
      e <- .set_y_axis(e, substitute(upper), y_index, i)
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

        # current_trace <- length(e$x$opts$series)
        # e$x$opts$series[[current_trace]]$name = name

        e$x$opts$legend$data <- append(e$x$opts$legend$data, list(name))
      }
      e$x$opts$series <- append(e$x$opts$series, list(e_serie))

  }
  path <- system.file("htmlwidgets/lib/echarts-6.0.0/plugins", package = "echarts4r")
  dep <- htmltools::htmlDependency(
    name = "echarts-lineRange",
    version = "1.0.0",
    src = c(file = path),
    script = "echarts-lineRange.js")

  e$dependencies <- append(e$dependencies, list(dep))

  e
}

#######
# TODO look at .get_index to replace current_trace.
# Do we want to make _ versions, too?
e_stage <- function(e,
                      time_start,
                      time_end,
                      stage,
                      stage_order,
                      legend = TRUE,
                      name = "contour",
                      thresholds = 8,
                      bandwidth = 20,
                      lineStyle = list(opacity = 0.3, color = "black", width=1),
                      contourOpacity = 0.8,
                      contourColors = list('#5470c6', '#91cc75', '#fac858', '#ee6666'),
                      ...){

  if (missing(e)) {
    stop("missing e", call. = FALSE)
  }
# browser()
  data = e$x$data

  for (i in seq_along(data)) { # TODO

    df = data[[i]]
    vector <- lapply(1:nrow(df), function(j) {
      list(
        df[["start"]][j],
        df[["end"]][j],
        df[["stage"]][j]
      )
      #
      # list(
      #   data[[i]][[deparse(substitute(time_start))]],
      #   data[[i]][[deparse(substitute(time_end))]],
      #   data[[i]][[deparse(substitute(stage))]]
      # )
    })

    e <- e |> e_axis(type = 'time', axis = "x")
    e <- e |> e_axis(type = 'category', axis = "y")# data = list(stage_order)) #, serie = stage)

    # if (y_index != 0) {
    #   e <- .set_y_axis(e, substitute(upper), y_index, i)
    # }
    # if (x_index != 0) {
    #   e <- .set_x_axis(e, x_index, i)
    # }

    if (!e$x$tl) {
      opts <- list(
        type = "custom",
        renderItem = 'stage',
        colorBy = 'data',
        encode = list(
          x = c(0, 1),
          y = 2,
          tooltip = c(0, 1)
        ),
        # data = vector
        # name = name,
        itemPayload = list(
          envelope = list()
        )
        # ...
      )

      e$x$opts$series <- append(e$x$opts$series, list(opts))
      # e_serie <- append(opts, e_serie) # data after renderItem, data used for Y-sizing only

      # if (isTRUE(legend)) {
      #
      #   # current_trace <- length(e$x$opts$series)
      #   # e$x$opts$series[[current_trace]]$name = name
      #
      #   e$x$opts$legend$data <- append(e$x$opts$legend$data, list(name))
      # }

      e$x$opts$dataset <- list(source = vector)

    }
    # else {
    #   if (isTRUE(legend)) {
    #     e$x$opts$legend$data <- append(
    #       e$x$opts$legend$data,
    #       list(name)
    #     )
    #   }
    #   e$x$opts$options[[i]]$series <- append(
    #     e$x$opts$options[[i]]$series,
    #     list(e_serie)
    #   )
    # }
  }
  # if (isTRUE(e$x$tl)) {
  #   # generate series list
  #   series_opts <- list(
  #     type = "custom",
  #     renderItem = 'contour',
  #     encode = list(x= 0,
  #                   y = 1,
  #                   tooltip = 2),
  #     data = vector,
  #     name = name,
  #     itemPayload = list(
  #       thresholds = thresholds,
  #       bandwidth = bandwidth,
  #       lineStyle = lineStyle,
  #       itemStyle = list(opacity = contourOpacity,
  #                        color = contourColors)
  #     ),
  #     ...
  #   )
  #
  #   if (isTRUE(legend)) {
  #     e$x$opts$baseOption$legend$data <- append(e$x$opts$baseOption$legend$data, list(name))
  #   }
  #   e$x$opts$baseOption$series <- append(
  #     e$x$opts$baseOption$series,
  #     list(series_opts)
  #   )
  # }

  path <- system.file("htmlwidgets/lib/echarts-6.0.0/plugins", package = "echarts4r")
  dep <- htmltools::htmlDependency(
    name = "echarts-stage",
    version = "1.0.0",
    src = c(file = path),
    script = "echarts-stage.js")

  e$dependencies <- append(e$dependencies, list(dep))

  e
}

df <- data.frame(
  start = as.POSIXct(c(
    "2024-09-07 06:12", "2024-09-07 06:15", "2024-09-07 08:59",
    "2024-09-07 05:45", "2024-09-07 07:37", "2024-09-07 08:56",
    "2024-09-07 09:08", "2024-09-07 05:45",
    "2024-09-07 03:12", "2024-09-07 04:02", "2024-09-07 04:40",
    "2024-09-07 04:57", "2024-09-07 06:12", "2024-09-07 06:18",
    "2024-09-07 07:56", "2024-09-07 09:00", "2024-09-07 09:29",
    "2024-09-07 03:27", "2024-09-07 04:36", "2024-09-07 04:48"
  )),

  end = as.POSIXct(c(
    "2024-09-07 06:12", "2024-09-07 06:18", "2024-09-07 09:00",
    "2024-09-07 06:12", "2024-09-07 07:56", "2024-09-07 08:59",
    "2024-09-07 09:29", "2024-09-07 06:12",
    "2024-09-07 03:27", "2024-09-07 04:36", "2024-09-07 04:48",
    "2024-09-07 05:45", "2024-09-07 06:15", "2024-09-07 07:37",
    "2024-09-07 08:56", "2024-09-07 09:08", "2024-09-07 10:41",
    "2024-09-07 04:02", "2024-09-07 04:40", "2024-09-07 04:57"
  )),

  stage = c(
    "Awake", "Awake", "Awake",
    "REM", "REM", "REM", "REM", "REM",
    "Core", "Core", "Core", "Core", "Core", "Core", "Core", "Core", "Core",
    "Deep", "Deep", "Deep"
  ),
  stringsAsFactors = FALSE
)


# e <- df |> e_charts()
# e |> e_stage(time_start = start, time_end = end, stage = stage, stage_order = c( "Deep", "Core","REM", "Awake"))
#
# e <- e |> e_axis(type = 'category', axis = "y", data = list( "Deep", "Core","REM", "Awake"), splitLine = list(show = TRUE), axisLabel = list(show = FALSE))
#
