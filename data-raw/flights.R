flights <- read.csv(paste0( "https://raw.githubusercontent.com/plotly/datasets/", "master/2011_february_aa_flight_paths.csv"))

usethis::use_data(flights)
