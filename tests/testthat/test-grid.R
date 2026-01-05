test_that("errors informatively without e", {
  expect_error(e_axis_(), "must pass e")
  e <- mtcars |> e_charts(mpg)
  expect_error(e_axis_(e), "no attribute")
  expect_error(e_x_axis_(), "must pass e")
  expect_error(e_y_axis_(), "must pass e")
  expect_error(e_z_axis_(), "must pass e")
  expect_error(e_x_axis(), "must pass e")
  expect_error(e_y_axis(), "must pass e")
  expect_error(e_z_axis(), "must pass e")
  expect_error(e_grid(), "must pass e")
  expect_error(e_radius_axis(), "must pass e")
  expect_error(e_radius_axis_(), "must pass e")
  expect_error(e_angle_axis(), "must pass e")
  expect_error(e_angle_axis_(), "must pass e")
  expect_error(e_radar_opts(), "must pass e")
  expect_error(e_single_axis(), "must pass e")
  expect_error(e_axis_labels(), "must pass e")
  expect_error(e_hide_grid_lines(), "must pass e")
  expect_error(e_axis_stagger(), "must pass e")
})

# e_axis_ -----------------------------------------------------------------
test_that("e_axis_ sets axis attributes based on serie", {
  e <- mtcars |> e_charts(mpg) |> e_scatter(wt)
  result <- e_axis(e, serie = wt, axis = "y", min = 1, max = 6)
  expect_equal(result$x$opts$yAxis[[1]]$min, 1)
  expect_equal(result$x$opts$yAxis[[1]]$max, 6)
})

test_that("e_axis_ calculates min and max from serie with margin", {
  df <- data.frame(x = 1:5, y = c(2, 4, 6, 8, 10))
  e <- df |> e_charts(x) |> e_scatter(y)
  result <- e_axis_(e, serie = "y", axis = "y", margin = 1)
  expect_equal(result$x$opts$yAxis[[1]]$min, 1)
  expect_equal(result$x$opts$yAxis[[1]]$max, 11)
})

test_that("e_axis_ sets formatter", {
  e <- mtcars |> e_charts(mpg) |> e_scatter(wt)
  fmt <- htmlwidgets::JS("function(value) { return value + 5; }")
  result <- e_axis_(e, axis = "x", formatter = fmt)
  expect_equal(result$x$opts$xAxis[[1]]$axisLabel$formatter, fmt)
})

test_that("e_axis_ works with timeline", {
  e <- mtcars |> dplyr::group_by(cyl) |> e_charts(mpg, timeline = TRUE) |> e_scatter(wt)
  result <- e_axis_(e, axis = "x", min = 10, max = 35)
  expect_equal(result$x$opts$baseOption$xAxis[[1]]$min, 10)
  expect_equal(result$x$opts$baseOption$xAxis[[1]]$max, 35)
})

test_that("e_axis_ handles deep list structure", {
  e <- mtcars |> e_charts(mpg) |> e_scatter(wt) |> e_scatter(qsec)
  result <- e_axis_(e, axis = "y", index = 0, min = 0)
  expect_equal(result$x$opts$yAxis[[1]]$min, 0)
})

test_that("e_axis_ resets to index 1 when r.index exceeds max", {
  e <- mtcars |> e_charts(mpg) |> e_scatter(wt)
  result <- e_axis_(e, axis = "x", index = 10, min = 0)
  expect_equal(result$x$opts$xAxis[[1]]$min, 0)
})

test_that("e_axis_ passes additional arguments", {
  e <- mtcars |> e_charts(mpg) |> e_scatter(wt)
  result <- e_axis_(e, axis = "x", name = "Miles per Gallon", nameLocation = "middle")
  expect_equal(result$x$opts$xAxis[[1]]$name, "Miles per Gallon")
  expect_equal(result$x$opts$xAxis[[1]]$nameLocation, "middle")
})

# e_x_axis_ ---------------------------------------------------------------
test_that("axis funs work", {
  e <- mtcars |> e_charts(mpg) |> e_scatter(wt)

  x_ <- e_x_axis_(e, min = 5)
  y_ <- e_y_axis_(e, min = 5)
  z_ <- e_z_axis_(e, min = 5)

  x <- e_x_axis(e, min = 5)
  y <- e_y_axis(e, min = 5)
  z <- e_z_axis(e, min = 5)

  expect_equal(x_$x$opts$xAxis[[1]]$min, 5)
  expect_equal(y_$x$opts$yAxis[[1]]$min, 5)
  expect_equal(z_$x$opts$zAxis$min, 5)

  expect_equal(x$x$opts$xAxis[[1]]$min, 5)
  expect_equal(y$x$opts$yAxis[[1]]$min, 5)
  expect_equal(z$x$opts$zAxis$min, 5)
})


# e_rm_axis ---------------------------------------------------------------


# e_axis_formatter --------------------------------------------------------

