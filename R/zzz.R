.onLoad <- function(libname, pkgname) {
  shiny::registerInputHandler("echarts4rParse", function(data, ...) {
    jsonlite::fromJSON(jsonlite::toJSON(data, auto_unbox = TRUE))
  }, force = TRUE)
}