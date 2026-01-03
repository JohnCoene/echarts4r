test_that("e_line plot has the good data structure and type", {

  tbl <- tibble::tibble(
    lon   = c(-83.0, -85.5, -78.0),
    lat   = c(76.5, 73.5, 73.0),
    value = c(1.1, 2.9, 1.5)
  )
  plot <- tbl |>
    e_charts(lon) |>
    e_globe(
      displacementScale = 0.04
    ) |>
    e_bar_3d(lat, value, "globe") |>
    e_visual_map(show = FALSE) |> suppressMessages()

  expect_s3_class(plot, "echarts4r")
  expect_s3_class(plot, "htmlwidget")

  expect_equal(
    plot$x$opts$series[[1]]$type,
    "bar3D"
  )
})

test_that("e_globe expects error when missing e or base texture", {
  expect_error(e_globe(), "must pass e")

  expect_message(iris |>
                   e_charts() |>
                   e_globe(
                   ), "`base_texture` is `NULL`, see echarts4r-assets.john-coene.com")
})
