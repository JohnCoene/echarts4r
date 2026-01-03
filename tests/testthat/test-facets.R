test_that("e_facet errors informatively with only rows or only cols", {
  df <- data.frame(
    day = rep(1:20, times = 2),
    temperature = runif(40, 10, 40),
    location = rep(LETTERS[1:2], each = 20)
  )

  e <- df |>
    dplyr::group_by(location) |>
    e_charts(day) |>
    e_line(temperature)

  expect_error(e_facet(e, rows = 4), "Must provide both the number of rows and columns.")
  expect_error(e_facet(e, cols = 4), "Must provide both the number of rows and columns.")
})

test_that("e_facet warns when series exceed panels", {
  df <- data.frame(
    day = rep(1:20, times = 5),
    temperature = runif(100, 10, 40),
    location = rep(LETTERS[1:5], each = 20)
  )

  e <- df |>
    dplyr::group_by(location) |>
    e_charts(day) |>
    e_line(temperature)

  expect_warning(
    e_facet(e, rows = 2, cols = 2),
    "5 series exceed the number of panels created by 2 rows and 2 columns"
  )
})

test_that("e_facet automatically determines grid dimensions when rows and cols are NULL", {
  df <- data.frame(
    day = rep(1:20, times = 4),
    temperature = runif(80, 10, 40),
    location = rep(LETTERS[1:4], each = 20)
  )

  testthat::local_mocked_bindings(n2mfrow = function(x) c(2, 2), .package = "grDevices")

  e <- df |>
    dplyr::group_by(location) |>
    e_charts(day) |>
    e_line(temperature)

  result <- e_facet(e)

  expect_s3_class(result, "echarts4r")
})

test_that("e_facet fills in missing margin_trbl values with defaults", {
  df <- data.frame(
    day = rep(1:20, times = 2),
    temperature = runif(40, 10, 40),
    location = rep(LETTERS[1:2], each = 20)
  )

  e <- df |>
    dplyr::group_by(location) |>
    e_charts(day) |>
    e_line(temperature)

  result <- e_facet(e, rows = 1, cols = 2, margin_trbl = c(t = 10, b = 10))

  expect_s3_class(result, "echarts4r")
})

test_that("e_facet calculates spacing for low dimensional grids", {
  df <- data.frame(
    day = rep(1:20, times = 2),
    temperature = runif(40, 10, 40),
    location = rep(LETTERS[1:2], each = 20)
  )

  e <- df |>
    dplyr::group_by(location) |>
    e_charts(day) |>
    e_line(temperature)

  result <- e_facet(e, rows = 2, cols = 2)

  expect_s3_class(result, "echarts4r")
})

test_that("e_facet uses custom panel spacing when provided", {
  df <- data.frame(
    day = rep(1:20, times = 2),
    temperature = runif(40, 10, 40),
    location = rep(LETTERS[1:2], each = 20)
  )

  e <- df |>
    dplyr::group_by(location) |>
    e_charts(day) |>
    e_line(temperature)

  result <- e_facet(e, rows = 2, cols = 2, h_panel_space = 50, v_panel_space = 5)

  expect_s3_class(result, "echarts4r")
})

test_that("e_facet adjusts dimensions based on legend position", {
  df <- data.frame(
    day = rep(1:20, times = 2),
    temperature = runif(40, 10, 40),
    location = rep(LETTERS[1:2], each = 20)
  )

  e <- df |>
    dplyr::group_by(location) |>
    e_charts(day) |>
    e_line(temperature)

  result_top <- e_facet(e, rows = 2, cols = 2, legend_pos = "top")
  result_bottom <- e_facet(e, rows = 2, cols = 2, legend_pos = "bottom")
  result_left <- e_facet(e, rows = 2, cols = 2, legend_pos = "left")
  result_right <- e_facet(e, rows = 2, cols = 2, legend_pos = "right")
  result_none <- e_facet(e, rows = 2, cols = 2, legend_pos = "NONE")

  expect_s3_class(result_top, "echarts4r")
  expect_s3_class(result_bottom, "echarts4r")
  expect_s3_class(result_left, "echarts4r")
  expect_s3_class(result_right, "echarts4r")
  expect_s3_class(result_none, "echarts4r")
})

test_that("e_facet errors informatively without chart type", {
  df <- data.frame(
    day = rep(1:20, times = 2),
    temperature = runif(40, 10, 40),
    location = rep(LETTERS[1:2], each = 20)
  )

  e <- df |>
    dplyr::group_by(location) |>
    e_charts(day)

  expect_error(e_facet(e, rows = 1, cols = 2), "No chart type found for first series")
})

test_that("e_facet handles pie charts", {
  df <- data.frame(
    person = rep(1:2, each = 3),
    activities = rep(LETTERS[1:3], 2),
    duration = sample(1:6, 6, replace = TRUE)
  )

  e <- df |>
    dplyr::group_by(person) |>
    e_charts(activities) |>
    e_pie(duration)

  result <- e_facet(e, rows = 1, cols = 2)

  expect_s3_class(result, "echarts4r")
  expect_equal(result$x$opts$series[[1]]$type, "pie")
  expect_false(result$x$opts$series[[1]]$avoidLabelOverlap)
})

test_that("e_facet handles non-pie charts with grid and axes", {
  df <- data.frame(
    day = rep(1:20, times = 2),
    temperature = runif(40, 10, 40),
    location = rep(LETTERS[1:2], each = 20)
  )

  e <- df |>
    dplyr::group_by(location) |>
    e_charts(day) |>
    e_line(temperature)

  result <- e_facet(e, rows = 1, cols = 2)

  expect_s3_class(result, "echarts4r")
  expect_length(result$x$opts$grid, 2)
  expect_length(result$x$opts$xAxis, 2)
  expect_length(result$x$opts$yAxis, 2)
  expect_equal(result$x$opts$series[[1]]$xAxisIndex, 0)
  expect_equal(result$x$opts$series[[1]]$yAxisIndex, 0)
  expect_equal(result$x$opts$series[[2]]$xAxisIndex, 1)
  expect_equal(result$x$opts$series[[2]]$yAxisIndex, 1)
})
