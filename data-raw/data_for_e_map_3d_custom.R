buildings <- jsonlite::read_json(
  paste0(
    "https://echarts.apache.org/examples/",
    "data-gl/asset/data/buildings.json"
  )
)

buildings$features <- buildings$features[1:500]

heights <- purrr::map(buildings$features, "properties") |>
  purrr::map("height") |>
  unlist()

names <- purrr::map(buildings$features, "properties") |>
  purrr::map("name") |>
  unlist()

buildings_sample <- dplyr::tibble(
  name = names,
  value = round(runif(length(names), 0, 1), 6),
  height = heights / 10
)

buildings_sample_json <- buildings
usethis::use_data(buildings_sample, overwrite = TRUE)
usethis::use_data(buildings_sample_json, overwrite = TRUE)
