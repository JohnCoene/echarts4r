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

# TODO do we want to use snake_case to be consistent?

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
#' e_chart() |> e_doughnut(numerator = 3, denominator = 6)
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

# TODO document more
#' Violin chart
#'
#' Draw a violin chart with scattered dots.
#'
#' @param e
#' @param data
#' @param x
#' @param y
#' @param show_scatter TRUE/FALSE to show scatter dots
#' @param violin_color violin color
#' @param violin_opacity opacity of the violin, between 0-1.
#' @param scatter_color scatter color
#' @param scatter_size size of scatter dots
#' @param bin_count Count of bins the data is divided into when calculating the distribution.
#' @param bandwidth_scale smoothness of the violin's shape by adjusting the kernel density estimation (KDE) bandwidth. A higher value smoothes the shape.
#' @param ...
#'
#' @seealso \href{https://github.com/apache/echarts-custom-series/tree/main/custom-series/segmentedDoughnut}{official documentation}
#'
#' @rdname e_violin
#' @export
#' @examples
#' e_chart() |> e_violin(data = iris, x =  "Species", y = "Sepal.Length") |> e_jitter(jitter = 100)
#'
e_violin <- function(e,
                     data,
                     x,
                     y,
                     show_scatter = TRUE,
                     violin_color = NULL,
                     violin_opacity = 0.8,
                     scatter_color = NULL,
                     scatter_size = 6,
                     bin_count = 20,
                     bandwidth_scale = 1.5,
                       ...) {

  if (missing(e)) {
    stop("missing e", call. = FALSE)
  }

  e <- e_chart()

  xData = data[[x]]

  # Get unique values for x-axis
  xData <- unique(data[[x]]) |> as.character()

  # Create dataSource with header
  dataSource <- list(c(x, y))  # Start with header row

  # Add all data points
  for (i in 1:nrow(data)) {
    dataSource <- append(dataSource, list(c(as.character(data[[x]][i]), data[[y]][i])))
  }

  e$x$opts$xAxis = list(type = 'category')

  e$x$opts$dataset <- list(source = dataSource)

  violin <- list(
    color = violin_color,
    type = "custom",
    renderItem = 'violin',
    colorBy = 'item',
    silent = TRUE,
    itemPayload = list(
      areaOpacity = violin_opacity,
      binCount = bin_count,
      bandWidthScale = bandwidth_scale
    ))

  if(show_scatter){

    scatter <- list(
      color = scatter_color,
      type = "scatter",
      colorBy = 'item',
      silent = TRUE,
      symbolSize = scatter_size
    )} else {
    scatter <- NULL
    }

  e$x$opts$series <- list(violin, scatter)

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
