url <- "https://echarts.apache.org/examples/data-gl/asset/data/population.json"

data <- jsonlite::fromJSON(url)
data <- as.data.frame(data)
names(data) <- c("lon", "lat", "value")
data$value <- log(data$value)
population <- data

usethis::use_data(population, overwrite = TRUE)
