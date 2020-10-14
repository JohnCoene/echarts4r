.onAttach <- function(libname, pkgname) {
  shiny::registerInputHandler("echarts4rParse", function(data, ...) {
    jsonlite::fromJSON(jsonlite::toJSON(data, auto_unbox = TRUE))
  }, force = TRUE)

  options(
    "ECHARTS4R_THEME" = NULL,
    "ECHARTS4R_FONT_FAMILY" = NULL
  )
}

.onLoad <- function(libname, pkgname) {
  shiny::registerInputHandler("echarts4rParse", function(data, ...) {
    jsonlite::fromJSON(jsonlite::toJSON(data, auto_unbox = TRUE))
  }, force = TRUE)

  options(
    "ECHARTS4R_THEME" = NULL,
    "ECHARTS4R_FONT_FAMILY" = NULL
  )
}
