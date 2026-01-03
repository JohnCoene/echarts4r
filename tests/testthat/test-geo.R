test_that("e_geo_3d plot has the good data structure and type", {
  choropleth <- data.frame(
    countries = c(
      "France",
      "Brazil",
      "China",
      "Russia",
      "Canada",
      "India",
      "United States",
      "Argentina",
      "Australia"
    ),
    height = runif(9, 1, 5),
    color = c(
      "#F7FBFF",
      "#DEEBF7",
      "#C6DBEF",
      "#9ECAE1",
      "#6BAED6",
      "#4292C6",
      "#2171B5",
      "#08519C",
      "#08306B"
    )
  )

  plot <- choropleth |>
    e_charts(countries) |>
    e_geo_3d(height, color)

  expect_s3_class(plot, "echarts4r")
  expect_s3_class(plot, "htmlwidget")

  missing_items <- choropleth |>
    e_charts(countries) |>
    e_geo_3d()
  expect_s3_class(missing_items, "echarts4r")
  expect_s3_class(missing_items, "htmlwidget")

  missing_items_ <- choropleth |>
    e_charts(countries) |>
    e_geo_3d_()

  expect_s3_class(missing_items_, "echarts4r")
  expect_s3_class(missing_items_, "htmlwidget")
})

test_that("e_geo_3d.echarts4r and e_geo_3d_ expects error when missing e or serie", {
  expect_error(e_geo_3d(), "must pass e")
  expect_error(e_geo_3d_(), "must pass e")
})
