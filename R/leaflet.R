#' Leaflet
#'
#' Leaflet extension.
#'
#' @inheritParams e_bar
#' @param roam Whether to allow the user to roam.
#' @param template \code{urlTemplate}, should not be changed.
#' @param options List of options, including \code{attribution} and \code{label}.
#'
#' @examples
#' \dontrun{
#' url <- paste0(
#'   "https://echarts.apache.org/examples/",
#'   "data-gl/asset/data/population.json"
#' )
#' data <- jsonlite::fromJSON(url)
#' data <- as.data.frame(data)
#' names(data) <- c("lon", "lat", "value")
#' data$value <- log(data$value)
#'
#' data %>%
#'   e_charts(lon) %>%
#'   e_leaflet() %>%
#'   e_leaflet_tile() %>%
#'   e_scatter(lat, size = value, coord_system = "leaflet")
#' }
#'
#' @note Will not render in the RStudio, open in browser.
#'
#' @rdname leaflet
#' @export
e_leaflet <- function(e, roam = TRUE, ...) {
  leaf <- list(...)
  leaf$roam <- roam
  leaf$tiles <- list()

  # remove axis
  e <- .rm_axis(e, TRUE, "x")
  e <- .rm_axis(e, TRUE, "y")

  e$x$opts$leaflet <- leaf

  # add dependency
  path <- system.file("htmlwidgets/lib/echarts-4.8.0/plugins", package = "echarts4r")
  dep <- htmltools::htmlDependency(
    name = "echarts-leaflet",
    version = "1.0.0",
    src = c(file = path),
    script = "echarts-leaflet.js"
  )

  e$dependencies <- append(e$dependencies, htmlwidgets::getDependency("leaflet"))
  e$dependencies <- append(e$dependencies, list(dep))

  e
}

#' @rdname leaflet
#' @export
e_leaflet_tile <- function(
  e,
  template = "https://{s}.tile.openstreetmap.fr/hot/{z}/{x}/{y}.png",
  options = NULL,
  ...
) {
  pkgs <- utils::installed.packages() %>% rownames()
  if (!"leaflet" %in% pkgs) {
    stop("Requires the `leaflet` package installed", call. = FALSE)
  }

  if (!length(e$x$opts$leaflet$roam)) {
    e <- e_leaflet(e)
  }

  tile <- list(...)
  tile$urlTemplate <- template

  if (!is.null(options)) {
    tile$options <- options
  }

  e$x$opts$leaflet$tiles <- list(tile)

  e
}
