test_that("e_add triggers deprecation warning", {
  funnel <- data.frame(
    stage = c("View", "Click"),
    value = c(80, 30),
    color = c("blue", "red")
  )

  expect_warning(
    funnel |>
      e_charts() |>
      e_funnel(value, stage) |>
      e_add("itemStyle", color),
    "deprecated"
  )
})

# e_add_nested ------------------------------------------------------------
test_that("e_add_nested errors informatively without e", {
  expect_error(e_add_nested(), "missing e or param")
  e <- mtcars |> e_charts(mpg) |> e_scatter(wt)
  expect_error(e_add_nested(e), "missing e or param")
})

test_that("e_add_nested adds nested data to series", {
  funnel <- data.frame(
    stage = c("View", "Click", "Purchase"),
    value = c(80, 30, 20),
    color = c("blue", "red", "green")
  )

  result <- funnel |>
    e_charts() |>
    e_funnel(value, stage) |>
    e_add_nested("itemStyle", color)

  expect_equal(result$x$opts$series[[1]]$data[[1]]$itemStyle$color, "blue")
  expect_equal(result$x$opts$series[[1]]$data[[2]]$itemStyle$color, "red")
  expect_equal(result$x$opts$series[[1]]$data[[3]]$itemStyle$color, "green")
})

test_that("e_add_nested works with polar", {
 df <- data.frame(
   x = seq(3),
   y = rnorm(3, 10, 3),
   z = rnorm(3, 11, 2),
   w = rnorm(3, 9, 2),
   color = c("blue", "red", "green")
 )
  result <-  df |> e_charts(x) |>
    e_polar() |>
    e_angle_axis(x) |> # angle = x
    e_radius_axis() |>
    e_bar(y, coord_system = "polar") |>
    e_add_nested("itemStyle", color)

  expect_equal(result$x$opts$series[[1]]$data[[1]]$itemStyle$color, "blue")
  expect_equal(result$x$opts$series[[1]]$data[[2]]$itemStyle$color, "red")
  expect_equal(result$x$opts$series[[1]]$data[[3]]$itemStyle$color, "green")
})

test_that("e_add_nested uses original data when .data is NULL", {
  funnel <- data.frame(
    stage = c("View", "Click"),
    value = c(80, 30),
    color = c("blue", "red")
  )

  result <- funnel |>
    e_charts() |>
    e_funnel(value, stage) |>
    e_add_nested("itemStyle", color, .data = NULL)

  expect_equal(result$x$opts$series[[1]]$data[[1]]$itemStyle$color, "blue")
})

test_that("e_add_nested works with timeline", {
  df <- data.frame(
    x = c("A", "B", "A", "B"),
    y = c(1, 2, 3, 4),
    grp = c(1, 1, 2, 2),
    color = c("red", "blue", "green", "yellow")
  )

  e <- df |>
    dplyr::group_by(grp) |>
    e_charts(x, timeline = TRUE) |>
    e_bar(y)

  result <- e_add_nested(e, "itemStyle", color)

  expect_equal(result$x$opts$options[[1]]$series[[1]]$data[[1]]$itemStyle$color, "red")
  expect_equal(result$x$opts$options[[2]]$series[[1]]$data[[1]]$itemStyle$color, "green")
})

test_that("e_add_nested works with .serie parameter", {
  df <- data.frame(
    x = c("A", "B", "C"),
    y = c(1, 2, 3),
    z = c(4, 5, 6),
    color = c("red", "blue", "green")
  )

  result <- df |>
    e_charts(x) |>
    e_bar(y) |>
    e_bar(z) |>
    e_add_nested("itemStyle", color, .serie = 1)

  expect_equal(result$x$opts$series[[1]]$data[[1]]$itemStyle$color, "red")
  expect_null(result$x$opts$series[[2]]$data[[1]]$itemStyle)
})

test_that("e_add_nested handles multiple columns", {
  df <- data.frame(
    x = c("A", "B"),
    y = c(1, 2),
    color = c("red", "blue"),
    size = c(10, 20)
  )

  result <- df |>
    e_charts(x) |>
    e_bar(y) |>
    e_add_nested("itemStyle", color, size)

  expect_equal(result$x$opts$series[[1]]$data[[1]]$itemStyle$color, "red")
  expect_equal(result$x$opts$series[[1]]$data[[1]]$itemStyle$size, '10')
})

# e_add_unnested ----------------------------------------------------------
test_that("e_add_nested errors informatively without e", {
  expect_error(e_add_nested(), "missing e or param")
  e <- mtcars |> e_charts(mpg) |> e_scatter(wt)
  test_that("e_add_unnested errors informatively without e", {
    expect_error(e_add_unnested(), "missing e, param, or value")
    e <- mtcars |> e_charts(mpg) |> e_scatter(wt)
    expect_error(e_add_unnested(e), "missing e, param, or value")
    expect_error(e_add_unnested(e, "param"), "missing e, param, or value")
  })

test_that("e_add_unnested adds unnested data to series", {
  funnel <- data.frame(
    stage = c("View", "Click", "Purchase"),
    value = c(80, 30, 20),
    size = c(10, 20, 30)
  )

  nested <- funnel |>
    e_charts() |>
    e_funnel(value, stage) |>
    e_add_nested("symbolSize", size)

  result <- nested |>
    e_add_unnested("symbolSize", size)

  expect_equal(result$x$opts$series[[1]]$data[[1]]$symbolSize, 10)
  expect_equal(result$x$opts$series[[1]]$data[[2]]$symbolSize, 20)
  expect_equal(result$x$opts$series[[1]]$data[[3]]$symbolSize, 30)
})

test_that("e_add_unnested works with polar", {
  df <- data.frame(
    x = seq(3),
    y = rnorm(3, 10, 3),
    z = rnorm(3, 11, 2),
    w = rnorm(3, 9, 2),
    size = c(5, 10, 15)
  )

  nested <- df |>
    e_charts(x) |>
    e_polar() |>
    e_angle_axis(x) |>
    e_radius_axis() |>
    e_bar(y, coord_system = "polar") |>
    e_add_unnested("symbolSize", size)
  expect_equal(nested$x$opts$series[[1]]$data[[1]]$symbolSize, 5)
  expect_equal(nested$x$opts$series[[1]]$data[[2]]$symbolSize, 10)
  expect_equal(nested$x$opts$series[[1]]$data[[3]]$symbolSize, 15)
})

test_that("e_add_unnested uses original data when .data is NULL", {
  funnel <- data.frame(
    stage = c("View", "Click"),
    value = c(80, 30),
    size = c(10, 20)
  )

  nested <- funnel |>
    e_charts() |>
    e_funnel(value, stage) |>
    e_add_nested("itemStyle", stage)

  result <- funnel |>
    e_charts() |>
    e_funnel(value, stage) |>
    e_add_unnested("symbolSize", size, .data = NULL)

  expect_equal(result$x$opts$series[[1]]$data[[1]]$symbolSize, 10)
})

test_that("e_add_unnested works with timeline", {
  df <- data.frame(
    x = c("A", "B", "A", "B"),
    y = c(1, 2, 3, 4),
    grp = c(1, 1, 2, 2),
    size = c(5, 10, 15, 20)
  )

  e <- df |>
    dplyr::group_by(grp) |>
    e_charts(x, timeline = TRUE) |>
    e_bar(y)

  result <- e_add_unnested(e, "symbolSize", size)

  expect_equal(result$x$opts$options[[1]]$series[[1]]$data[[1]]$symbolSize, 5)
  expect_equal(result$x$opts$options[[2]]$series[[1]]$data[[1]]$symbolSize, 15)
})

test_that("e_add_unnested works with .serie parameter", {
  df <- data.frame(
    x = c("A", "B", "C"),
    y = c(1, 2, 3),
    z = c(4, 5, 6),
    size = c(10, 20, 30)
  )

  result <- df |>
    e_charts(x) |>
    e_bar(y) |>
    e_bar(z) |>
    e_add_unnested("symbolSize", size, .serie = 1)

  expect_equal(result$x$opts$series[[1]]$data[[1]]$symbolSize, 10)
  expect_null(result$x$opts$series[[2]]$data[[1]]$symbolSize)
})
})
