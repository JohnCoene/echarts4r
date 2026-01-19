#' 2011 flight path data
#'
#' @source github repo
#' \url{https://raw.githubusercontent.com/plotly/datasets/master/2011_february_aa_flight_paths.csv}
#' @format A data frame with 178 rows and 8 variables:
#' \describe{
#'   \item{start_lat}{Numeric vector}
#'   \item{start_lon}{Numeric vector}
#'   \item{end_lat}{Numeric vector}
#'   \item{end_lon}{Numeric vector}
#'   \item{airline}{Character vector}
#'   \item{airport1}{Character vector}
#'   \item{airport2}{Character vector}
#'   \item{cnt}{Integer vector}
#' }
"flights"

#' 2011 airport data
#'
#' @source github repo
#' \url{https://raw.githubusercontent.com/plotly/datasets/master/2011_february_us_airport_traffic.csv}
#' @format A data frame with 221 rows and 8 variables:
#' \describe{
#'   \item{iata}{Character vector}
#'   \item{airport}{Character vector}
#'   \item{city}{Character vector}
#'   \item{state}{Character vector}
#'   \item{country}{Character vector}
#'   \item{lat}{Numeric vector}
#'   \item{long}{Numeric vector}
#'   \item{cnt}{Integer vector}
#' }
"airports"

#' Buildings sample set
#'
#' @source echarts
#' \url{https://echarts.apache.org/examples/data-gl/asset/data/buildings.json}
#' @format A tibble
#' \describe{
#'   \item{name}{Character vector}
#'   \item{value}{Character vector}
#'   \item{height}{Character vector}
#' }
"buildings_sample"

#' Buildings sample set in json format
#'
#' @source echarts
#' \url{https://echarts.apache.org/examples/data-gl/asset/data/buildings.json}
#' @format json
"buildings_sample_json"

#' World population data
#'
#' @source echarts
#' \url{https://echarts.apache.org/examples/data-gl/asset/data/population.json}
#' @format data frame with 3 columns and 38,654 rows
#' \describe{
#'   \item{lon}{Numeric vector}
#'   \item{lat}{Numeric vector}
#'   \item{value}{Integer vector}
#' }
"population"
