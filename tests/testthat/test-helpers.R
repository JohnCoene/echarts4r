
test_that("e_zigzag errors informatively without e", {
  expect_error(e_zigzag(), "must pass e")
})

test_that("e_zigzag errors informatively without axis", {
  e <- mtcars |> e_charts(mpg) |> e_scatter(wt)
  expect_error(e_zigzag(e), "must indicate which axis to zigzag")
})

test_that("e_zigzag errors informatively without start or end", {
  e <- mtcars |> e_charts(mpg) |> e_scatter(wt)
  expect_snapshot(e_zigzag(e, axis = "y", start = 400), error = TRUE)
  expect_snapshot(e_zigzag(e, axis = "y", end = 500), error = TRUE)
})

test_that("e_zigzag adds breaks to y axis", {
  df <- data.frame(
    x = c("a", "b", "c", "d", "c"),
    y = c(100, 200, 200, 700, 300)
  )

  result <- df |>
    e_charts(x) |>
    e_bar(y) |>
    e_zigzag(axis = "y", start = 400, end = 500)

  expect_s3_class(result, "echarts4r")
  expect_length(result$x$opts$yAxis[[1]]$breaks, 1)
  expect_equal(result$x$opts$yAxis[[1]]$breaks[[1]]$start, '400')
  expect_equal(result$x$opts$yAxis[[1]]$breaks[[1]]$end, '500')
  expect_equal(result$x$opts$yAxis[[1]]$breaks[[1]]$gap, "3%")
})

test_that("e_zigzag adds breaks to x axis", {
  df <- data.frame(
    x = c(100, 200, 200, 700, 300),
    y = c("a", "b", "c", "d", "e")
  )

  result <- df |>
    e_charts(x) |>
    e_bar(x) |>
    e_zigzag(axis = "x", start = 400, end = 500)

  expect_s3_class(result, "echarts4r")
  expect_length(result$x$opts$xAxis[[1]]$breaks, 1)
  expect_equal(result$x$opts$xAxis[[1]]$breaks[[1]]$start, '400')
  expect_equal(result$x$opts$xAxis[[1]]$breaks[[1]]$end, '500')
})

test_that("e_zigzag handles multiple breaks", {
  df <- data.frame(
    x = c("a", "b", "c", "d", "e"),
    y = c(100, 200, 200, 700, 300)
  )

  result <- df |>
    e_charts(x) |>
    e_bar(y) |>
    e_zigzag(axis = "y", start = c(125, 400), end = c(150, 500))

  expect_s3_class(result, "echarts4r")
  expect_length(result$x$opts$yAxis[[1]]$breaks, 2)
  expect_equal(result$x$opts$yAxis[[1]]$breaks[[1]]$start, '125')
  expect_equal(result$x$opts$yAxis[[1]]$breaks[[1]]$end, '150')
  expect_equal(result$x$opts$yAxis[[1]]$breaks[[2]]$start, '400')
  expect_equal(result$x$opts$yAxis[[1]]$breaks[[2]]$end, '500')
})

test_that("e_zigzag sets zigzagAmplitude parameter", {
  df <- data.frame(
    x = c("a", "b", "c"),
    y = c(100, 200, 700)
  )

  result <- df |>
    e_charts(x) |>
    e_bar(y) |>
    e_zigzag(axis = "y", start = 400, end = 500, zigzagAmplitude = 20)

  expect_equal(result$x$opts$yAxis[[1]]$breakArea$zigzagAmplitude, 20)
})

test_that("e_zigzag sets gap parameter", {
  df <- data.frame(
    x = c("a", "b", "c"),
    y = c(100, 200, 700)
  )

  result <- df |>
    e_charts(x) |>
    e_bar(y) |>
    e_zigzag(axis = "y", start = 400, end = 500, gap = "5%")

  expect_equal(result$x$opts$yAxis[[1]]$breaks[[1]]$gap, "5%")
})

test_that("e_zigzag appends additional arguments", {
  df <- data.frame(
    x = c("a", "b", "c"),
    y = c(100, 200, 700)
  )

  result <- df |>
    e_charts(x) |>
    e_bar(y) |>
    e_zigzag(axis = "y", start = 400, end = 500, customArg = "test")

  expect_equal(result$x$opts$yAxis[[1]]$breakArea$customArg, "test")
})
