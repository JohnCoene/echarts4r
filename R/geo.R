#' Geo 3D
#'
#' Initialise geo 3D.
#'
#' @inheritParams e_bar
#' @param color Color.
#' @param type Map type.
#' @param rm_x,rm_y Whether to remove x and y axis, defaults to \code{TRUE}.
#'
#' @examples
#' choropleth <- data.frame(
#'   countries = c(
#'     "France",
#'     "Brazil",
#'     "China",
#'     "Russia",
#'     "Canada",
#'     "India",
#'     "United States",
#'     "Argentina",
#'     "Australia"
#'   ),
#'   height = runif(9, 1, 5),
#'   color = c(
#'     "#F7FBFF",
#'     "#DEEBF7",
#'     "#C6DBEF",
#'     "#9ECAE1",
#'     "#6BAED6",
#'     "#4292C6",
#'     "#2171B5",
#'     "#08519C",
#'     "#08306B"
#'   )
#' )
#'
#' choropleth %>%
#'   e_charts(countries) %>%
#'   e_geo_3d(height, color)
#' @seealso \code{\link{e_country_names}},
#' \href{https://echarts.apache.org/en/option-gl.html#geo3D}{Additional arguments}
#'
#' @rdname e_geo_3d
#' @export
e_geo_3d <- function(e, serie, color, type = "world", rm_x = TRUE, rm_y = TRUE, ...) {
  if (missing(e)) {
    stop("must pass e", call. = FALSE)
  }

  if (!missing(serie)) {
    sr <- deparse(substitute(serie))
  } else {
    sr <- NULL
  }

  if (!missing(color)) {
    cl <- deparse(substitute(color))
  } else {
    cl <- NULL
  }

  e_geo_3d_(e, sr, cl, type, rm_x, rm_y, ...)
}

#' @rdname e_geo_3d
#' @export
e_geo_3d_ <- function(e, serie = NULL, color = NULL, type = "world", rm_x = TRUE, rm_y = TRUE, ...) {
  if (missing(e)) {
    stop("must pass e", call. = FALSE)
  }

  e <- .rm_axis(e, rm_x, "x")
  e <- .rm_axis(e, rm_y, "y")

  series <- list(
    map = type,
    ...
  )

  if (!is.null(color) && !is.null(serie)) {
    series$regions <- .build_height(e, serie, color, j = 1)
  } else if (is.null(color) && !is.null(serie)) {
    series$regions <- .build_height(e, serie, j = 1)
  }

  if (!e$x$tl) {
    e$x$opts$geo3D <- series
  } else {
    e$x$opts$baseOption$geo3D <- series
  }

  path <- system.file("htmlwidgets/lib/echarts-4.8.0", package = "echarts4r")
  dep <- htmltools::htmlDependency(
    name = "echarts-gl",
    version = "1.1.2",
    src = c(file = path),
    script = "echarts-gl.min.js"
  )

  e$dependencies <- append(e$dependencies, list(dep))

  if (type == "world") {
    # add dependency
    path <- system.file("htmlwidgets/lib/echarts-4.8.0", package = "echarts4r")
    dep <- htmltools::htmlDependency(
      name = "echarts-world",
      version = "1.0.0",
      src = c(file = path),
      script = "world.js"
    )

    e$dependencies <- append(e$dependencies, list(dep))
  }

  e
}

#' Geo
#'
#' Initialise geo.
#'
#' @inheritParams e_bar
#' @param map Map type.
#' @param rm_x,rm_y Whether to remove x and y axis, defaults to \code{TRUE}.
#'
#' @examples
#' flights <- read.csv(
#'   paste0(
#'     "https://raw.githubusercontent.com/plotly/datasets/",
#'     "master/2011_february_aa_flight_paths.csv"
#'   )
#' )
#'
#' flights %>%
#'   e_charts() %>%
#'   e_geo() %>%
#'   e_lines(
#'     start_lon,
#'     start_lat,
#'     end_lon,
#'     end_lat,
#'     name = "flights",
#'     lineStyle = list(normal = list(curveness = 0.3))
#'   )
#' @seealso \href{https://echarts.apache.org/en/option.html#geo}{Additional arguments}
#'
#' @export
e_geo <- function(e, map = "world", ..., rm_x = TRUE, rm_y = TRUE) {
  opts <- list(
    map = map,
    ...
  )

  e <- .rm_axis(e, rm_x, "x")
  e <- .rm_axis(e, rm_y, "y")

  if (!e$x$tl) {
    e$x$opts$geo <- opts
  } else {
    e$x$opts$baseOption$geo <- opts
  }

  if (map == "world") {
    # add dependency
    path <- system.file("htmlwidgets/lib/echarts-4.8.0", package = "echarts4r")
    dep <- htmltools::htmlDependency(
      name = "echarts-world",
      version = "1.0.0",
      src = c(file = path),
      script = "world.js"
    )

    e$dependencies <- append(e$dependencies, list(dep))
  }

  e
}
