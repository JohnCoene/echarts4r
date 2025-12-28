flights <- read.csv(paste0( "https://raw.githubusercontent.com/plotly/datasets/", "master/2011_february_aa_flight_paths.csv"))

airports <- read.csv(
  paste0(
    "https://raw.githubusercontent.com/plotly/datasets/",
    "master/2011_february_us_airport_traffic.csv"
  )
)

usethis::use_data(flights, airports, overwrite = TRUE)
