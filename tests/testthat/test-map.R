### Tests of the functions in the tab "Maps"
### https://echarts4r.john-coene.com/articles/map.html


test_that("e_map plot has the good data structure and type", {
  set.seed(1)
  cns <- countrycode::codelist$country.name.en[1:5]
  cns <- data.frame(
    country = cns,
    value = round(runif(length(cns), 1, 5), 6)
  )

  plot <- cns %>%
    e_charts(country) %>%
    e_map(value) %>%
    e_visual_map(value)

  expect_s3_class(plot, "echarts4r")
  expect_s3_class(plot, "htmlwidget")

  expect_equal(
    plot$x$opts$series[[1]]$data,
    list(
      list(value = c(2.062035), name = "Afghanistan"),
      list(value = c(2.488496), name = "Åland Islands"),
      list(value = c(3.291413), name = "Albania"),
      list(value = c(4.632831), name = "Algeria"),
      list(value = c(1.806728), name = "American Samoa")
    )
  )
  expect_equal(
    plot$x$opts$series[[1]]$type,
    "map"
  )
})


test_that("e_lines plot has the good data structure and type", {
  flights <- read.csv(
    paste0(
      "https://raw.githubusercontent.com/plotly/datasets/",
      "master/2011_february_aa_flight_paths.csv"
    )
  )

  plot <- flights[1:5, ] %>%
    e_charts() %>%
    e_geo() %>%
    e_lines(
      start_lon,
      start_lat,
      end_lon,
      end_lat,
      name = "flights",
      lineStyle = list(normal = list(curveness = 0.3))
    )

  expect_s3_class(plot, "echarts4r")
  expect_s3_class(plot, "htmlwidget")

  # expect_equal(
  #   plot$x$opts$series[[1]]$data,
  #   list(list(coords = list(c(-97.03720, 32.89595), c(-106.60919, 35.04022))),
  #        list(coords = list(c(-87.90446, 41.97960), c(-97.66987, 30.19453))),
  #        list(coords = list(c(-97.03720, 32.89595), c(-72.68323, 41.93887))),
  #        list(coords = list(c(-66.00183, 18.43942), c(-72.68323, 41.93887))),
  #        list(coords = list(c(-97.03720, 32.89595), c(-86.75355, 33.56294))))
  # )

  # test that difference is near 0 because decimals are problematic
  difference <-
    unlist(plot$x$opts$series[[1]]$data) - unlist(list(
      list(coords = list(
        c(-97.03720, 32.89595),
        c(-106.60919, 35.04022)
      )),
      list(coords = list(
        c(-87.90446, 41.97960),
        c(-97.66987, 30.19453)
      )),
      list(coords = list(
        c(-97.03720, 32.89595),
        c(-72.68323, 41.93887)
      )),
      list(coords = list(
        c(-66.00183, 18.43942),
        c(-72.68323, 41.93887)
      )),
      list(coords = list(
        c(-97.03720, 32.89595),
        c(-86.75355, 33.56294)
      ))
    ))
  difference_test <- difference < 10^-5

  expect_true(unique(difference_test))
  expect_equal(
    plot$x$opts$series[[1]]$type,
    "lines"
  )
})


### Either need to put echarts4r.maps in imports, or to find another way without importing it

# test_that("em_map plot has the good data structure and type", {
#   library(echarts4r.maps)
#
#   df <- data.frame(
#     region = c("Rajasthan", "Odisha", "Gujarat"),
#     value = c(1,2, 3)
#   )
#
#   df %>%
#     e_charts(region) %>%
#     em_map("India") %>%
#     e_map(value, map = "India") %>%
#     e_visual_map(value) %>%
#     e_theme("infographic")
#
#   expect_s3_class(plot, "echarts4r")
#   expect_s3_class(plot, "htmlwidget")
#
#   expect_equal(
#     plot$x$opts$series[[1]]$data,
#     list(list(value = c(2.062035), name = "Afghanistan"),
#          list(value = c(2.488496), name = "Åland Islands"),
#          list(value = c(3.291413), name = "Albania"),
#          list(value = c(4.632831), name = "Algeria"),
#          list(value = c(1.806728), name = "American Samoa"))
#   )
#   expect_equal(
#     plot$x$opts$series[[1]]$type,
#     "map"
#   )
# })


test_that("geojson support is functional", {
  json <- jsonlite::read_json("https://raw.githubusercontent.com/shawnbot/topogram/master/data/us-states.geojson")


  plot <- USArrests %>%
    dplyr::mutate(states = row.names(.)) %>%
    e_charts(states) %>%
    e_map_register("USA", json) %>%
    e_map(Murder, map = "USA") %>%
    e_visual_map(Murder)

  expect_s3_class(plot, "echarts4r")
  expect_s3_class(plot, "htmlwidget")

  # different test: it's enough to check if data is equal for two random states to confirm that geojson is supported
  expect_equal(
    plot$x$opts$series[[1]]$data[[2]],
    list(value = 10, name = "Alaska")
  )
  expect_equal(
    plot$x$opts$series[[1]]$data[[22]],
    list(value = 12.1, name = "Michigan")
  )

  expect_equal(
    plot$x$opts$series[[1]]$type,
    "map"
  )
})
