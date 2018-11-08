.onAttach <- function(libname, pkgname) {
  packageStartupMessage(
    "Welcome to echarts4r\n\n",
    "Documentation: echarts4r.john-coene.com"
  )
  
  shiny::registerInputHandler("echarts4rParse", function(data, ...) {
    jsonlite::fromJSON(jsonlite::toJSON(data, auto_unbox = TRUE))
  }, force = TRUE)
}

.onLoad <- function(libname, pkgname) {
  shiny::registerInputHandler("echarts4rParse", function(data, ...) {
    jsonlite::fromJSON(jsonlite::toJSON(data, auto_unbox = TRUE))
  }, force = TRUE)
}