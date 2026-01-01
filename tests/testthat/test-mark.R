
# e_mark ------------------------------------------------------------------
test_that("e_mark_point plot has the good data structure and type", {

  df <- data.frame(
    x = seq(3),
    y = c(1, 3, 9),
    z = c(2, 5, 4),
    w = c(3, 4, 3)
  )

  plot <- df |>
    e_charts(x) |>
    e_line(z) |>
    e_mark_point("z", data = list(
      name = "Max",
      type = "max"
    ))

  expect_s3_class(plot, "echarts4r")
  expect_s3_class(plot, "htmlwidget")

  expect_equal(
    plot$x$opts$series[[1]]$markPoint$data[[1]],
    list(
      name = "Max",
      type = "max"
    ))
})

test_that("e_mark_line plot has the good data structure and type", {

  df <- data.frame(
    x = seq(3),
    y = c(1, 3, 9),
    z = c(2, 5, 4),
    w = c(3, 4, 3)
  )

  plot <- df |>
    e_charts(x) |>
    e_line(z) |>
    e_mark_line("z", data = list(
      name = "Max",
      type = "max"
    ))

  expect_s3_class(plot, "echarts4r")
  expect_s3_class(plot, "htmlwidget")

  expect_equal(
    plot$x$opts$series[[1]]$markLine$data[[1]],
    list(
      name = "Max",
      type = "max"
    ))
})

test_that("e_mark_area plot has the good data structure and type", {

  df <- data.frame(
    x = seq(3),
    y = c(1, 3, 9),
    z = c(2, 5, 4),
    w = c(3, 4, 3)
  )

  plot <- df |>
    e_charts(x) |>
    e_line(z) |>
    e_mark_area("z", data = list(
      list(xAxis = "min", yAxis = "min"),
      list(xAxis = "max", yAxis = "max")
    ))

  expect_s3_class(plot, "echarts4r")
  expect_s3_class(plot, "htmlwidget")

  expect_equal(
    plot$x$opts$series[[1]]$markArea$data[[1]],
    list(
      list(xAxis = "min", yAxis = "min"),
      list(xAxis = "max", yAxis = "max")
    ))
})

test_that("e_mark expects error when missing e", {
  expect_error(e_mark_point(), "must pass e")
  expect_error(e_mark_line(), "must pass e")
  expect_error(e_mark_area(), "must pass e")
  expect_error(e_mark_p.echarts4r(), "must pass e")
  expect_error(e_mark_p_(), "must pass e")
})
