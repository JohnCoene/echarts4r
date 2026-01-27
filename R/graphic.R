#' Graphic
#'
#' Low level API to define graphic elements.
#'
#' @inheritParams  e_bar
#'
#' @section Functions:
#' \itemize{
#'   \item{\code{e_graphic_g} to initialise graphics, entirely optional.}
#'   \item{\code{e_group_g} to create group, the children of which will share attributes.}
#'   \item{\code{e_image_g} to a png or jpg image.}
#'   \item{\code{e_text_g} to add text.}
#'   \item{\code{e_rect_g} to add a rectangle.}
#'   \item{\code{e_circle_g} to add a circle.}
#'   \item{\code{e_ring_g} to add a ring.}
#'   \item{\code{e_sector_g}}
#'   \item{\code{e_arc_g} to create an arc.}
#'   \item{\code{e_polygon_g} to create a polygon.}
#'   \item{\code{e_polyline_g} to create a polyline.}
#'   \item{\code{e_line_g} to draw a line.}
#'   \item{\code{e_bezier_curve_g} to draw a quadratic bezier curve or cubic bezier curve.}
#' }
#'
#' @note Some elements, i.e.: \code{e_image_g} may not display in the RStudio
#'   browwser but will work fine in your browser, R markdown documents and Shiny
#'   applications.
#'
#' @examples
#' # may not work in RStudio viewer
#' # Open in browser
#' cars |>
#'   e_charts(speed) |>
#'   e_scatter(dist) |>
#'   e_image_g(
#'     right = 20,
#'     top = 20,
#'     z = -999,
#'     style = list(
#'       image = "https://www.r-project.org/logo/Rlogo.png",
#'       width = 150,
#'       height = 150,
#'       opacity = .6
#'     )
#'   )
#'
#' anno_text <-
#' 'xAxis represents miles per gallon, yAxis represents weight in tonnes.'
#' # Position does change depending on window size.
#' mtcars |>
#'   e_charts(mpg) |>
#'   e_line(wt) |>
#'   e_group_g(
#'     left = '18%',
#'     top = '55%',
#'     children = list(
#'       # Background rectangle
#'       list(
#'         type = 'rect',
#'         z = 100,
#'         left = 'center',
#'         top = 'middle',
#'         shape = list(
#'           width = 240,
#'           height = 70
#'         ),
#'         style = list(
#'           fill = '#fff',
#'           stroke = '#555',
#'           lineWidth = 1,
#'           shadowBlur = 8,
#'           shadowOffsetX = 3,
#'           shadowOffsetY = 3,
#'           shadowColor = 'rgba(0,0,0,0.2)'
#'         )
#'       ),
#'       # Text element
#'       list(
#'         type = 'text',
#'         z = 100,
#'         left = 'center',
#'         top = 'middle',
#'         style = list(
#'           fill = '#333',
#'           width = 220,
#'           overflow = 'break',
#'           text = anno_text,
#'           font = '14px Microsoft YaHei'
#'         )
#'       )
#'     )
#'   )
#' #' @seealso \href{https://echarts.apache.org/en/option.html#graphic}{official documentation}
#'
#' @rdname graphic
#' @export
e_graphic_g <- function(e, ...) {
  if (missing(e)) {
    stop("must pass e", call. = FALSE)
  }

  if (!e$x$tl) {
    e$x$opts$graphic <- list(...)
  } else {
    e$x$opts$baseOption$graphic <- list(...)
  }
  e
}

#' @rdname graphic
#' @export
e_group_g <- function(e, ...) {
  if (missing(e)) {
    stop("must pass e", call. = FALSE)
  }

  .e_graphic_elem(e, "group", ...)
}

#' @rdname graphic
#' @export
e_image_g <- function(e, ...) {
  if (missing(e)) {
    stop("must pass e", call. = FALSE)
  }

  .e_graphic_elem(e, "image", ...)
}

#' @rdname graphic
#' @export
e_text_g <- function(e, ...) {
  if (missing(e)) {
    stop("must pass e", call. = FALSE)
  }

  .e_graphic_elem(e, "text", ...)
}

#' @rdname graphic
#' @export
e_rect_g <- function(e, ...) {
  if (missing(e)) {
    stop("must pass e", call. = FALSE)
  }

  .e_graphic_elem(e, "rect", ...)
}

#' @rdname graphic
#' @export
e_circle_g <- function(e, ...) {
  if (missing(e)) {
    stop("must pass e", call. = FALSE)
  }

  .e_graphic_elem(e, "circle", ...)
}

#' @rdname graphic
#' @export
e_ring_g <- function(e, ...) {
  if (missing(e)) {
    stop("must pass e", call. = FALSE)
  }

  .e_graphic_elem(e, "ring", ...)
}

#' @rdname graphic
#' @export
e_sector_g <- function(e, ...) {
  if (missing(e)) {
    stop("must pass e", call. = FALSE)
  }

  .e_graphic_elem(e, "sector", ...)
}

#' @rdname graphic
#' @export
e_arc_g <- function(e, ...) {
  if (missing(e)) {
    stop("must pass e", call. = FALSE)
  }

  .e_graphic_elem(e, "arc", ...)
}

#' @rdname graphic
#' @export
e_polygon_g <- function(e, ...) {
  if (missing(e)) {
    stop("must pass e", call. = FALSE)
  }

  .e_graphic_elem(e, "polygon", ...)
}

#' @rdname graphic
#' @export
e_polyline_g <- function(e, ...) {
  if (missing(e)) {
    stop("must pass e", call. = FALSE)
  }

  .e_graphic_elem(e, "polyline", ...)
}

#' @rdname graphic
#' @export
e_line_g <- function(e, ...) {
  if (missing(e)) {
    stop("must pass e", call. = FALSE)
  }

  .e_graphic_elem(e, "line", ...)
}

#' @rdname graphic
#' @export
e_bezier_curve_g <- function(e, ...) {
  if (missing(e)) {
    stop("must pass e", call. = FALSE)
  }

  .e_graphic_elem(e, "bezierCurve", ...)
}

#' Draft
#'
#' Add a draft watermark to your graph.
#'
#' @inheritParams e_bar
#' @param text Text to display.
#' @param size Font size of text.
#' @param opacity,color Opacity and color of text.
#'
#' @examples
#' cars |>
#'   e_charts(speed) |>
#'   e_scatter(dist) |>
#'   e_draft()
#' @export
e_draft <- function(e, text = "DRAFT", size = "120px", opacity = 0.4, color = "#d3d3d3") {
  if (missing(e)) {
    stop("must pass e", call. = FALSE)
  }
  e |>
    e_text_g(
      left = "center",
      top = "center",
      z = -1000,
      style = list(
        fill = color,
        text = text,
        font = paste(size, '"STHeiti", sans-serif'),
        opacity = opacity
      )
    )
}
