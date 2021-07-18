#' Facet
#'
#' Create facets for multiple plots.
#'
#' @inheritParams e_bar
#' @param rows,cols Number of rows and columns.
#'
#' @details Each serie, i.e.: \code{\link{e_bar}} will be plotted against a facet.
#'
#' @noRd
#' @keywords internal
e_facet <- function(e, rows = 1, cols = 1) {

  # number of series
  nseries <- length(e$x$opts$series)

  # cols and rows as vectors
  c <- 1:cols
  r <- 1:rows

  # define maximums
  if (rows > 1) {
    w_max <- 90
  } else {
    w_max <- 100
  }

  if (cols > 1) {
    h_max <- 90
  } else {
    h_max <- 100
  }

  # grids
  g <- length(r) * length(c)
  height <- h_max / cols
  width <- w_max / rows

  ttop <- 100 / cols
  lleft <- 100 / rows

  for (i in 1:g) {
    if (cols > 1) {
      top <- paste0(ttop * (i - 1), "%")
    } else {
      top <- "5%"
    }

    if (rows > 1) {
      left <- paste0(lleft * (i - 1), "%")
    } else {
      left <- "5%"
    }

    e <- e |>
      e_grid(
        height = paste0(height, "%"),
        width = paste0(width, "%"),
        top = top,
        left = left
      ) |>
      e_y_axis(gridIndex = i - 1, index = i) |>
      e_x_axis(gridIndex = i - 1, index = i)
  }

  e$x$facets <- list(
    grids = 4,
    current = 0
  )
  e
}
