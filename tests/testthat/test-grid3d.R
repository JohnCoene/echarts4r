
test_that("e_axis_3d errors informatively without e", {
  expect_error(e_axis_3d(), "missing e")
})

test_that("e_axis_3d errors informatively with invalid axis", {
  v <- LETTERS[1:10]
  matrix <- data.frame(
    x = sample(v, 300, replace = TRUE),
    y = sample(v, 300, replace = TRUE),
    z1 = rnorm(300, 10, 1),
    stringsAsFactors = FALSE
  ) |>
    dplyr::summarise(z1 = sum(z1), .by = c("x", "y"))

  e <- matrix |>
    e_charts(x) |>
    e_bar_3d(y, z1)

  expect_error(e_axis_3d(e, axis = "x", index = 10), "invalid axis")
})

test_that("e_axis_3d errors informatively without attributes", {
  v <- LETTERS[1:10]
  matrix <- data.frame(
    x = sample(v, 300, replace = TRUE),
    y = sample(v, 300, replace = TRUE),
    z1 = rnorm(300, 10, 1),
    stringsAsFactors = FALSE
  ) |>
    dplyr::summarise(z1 = sum(z1), .by = c("x", "y"))

  e <- matrix |>
    e_charts(x) |>
    e_bar_3d(y, z1)

  expect_error(e_axis_3d(e, axis = "x"), "no attribute")
})

test_that("e_axis_3d sets axis attributes", {
  v <- LETTERS[1:10]
  matrix <- data.frame(
    x = sample(v, 300, replace = TRUE),
    y = sample(v, 300, replace = TRUE),
    z1 = rnorm(300, 10, 1),
    stringsAsFactors = FALSE
  ) |>
    dplyr::summarise(z1 = sum(z1), .by = c("x", "y"))

  result <- matrix |>
    e_charts(x) |>
    e_bar_3d(y, z1) |>
    e_axis_3d(axis = "x", index = 0, axisLine = list(lineStyle = list(color = "blue")))

  expect_equal(result$x$opts$xAxis3D[[1]]$axisLine$lineStyle$color, "blue")
})

test_that("e_x_axis_3d errors informatively without e", {
  expect_error(e_x_axis_3d(), "missing e")
})

test_that("e_y_axis_3d errors informatively without e", {
  expect_error(e_y_axis_3d(), "missing e")
})

test_that("e_z_axis_3d errors informatively without e", {
  expect_error(e_z_axis_3d(), "missing e")
})

test_that("e_x_axis_3d sets x axis attributes", {
  v <- LETTERS[1:10]
  matrix <- data.frame(
    x = sample(v, 300, replace = TRUE),
    y = sample(v, 300, replace = TRUE),
    z1 = rnorm(300, 10, 1),
    stringsAsFactors = FALSE
  ) |>
    dplyr::summarise(z1 = sum(z1), .by = c("x", "y"))

  result <- matrix |>
    e_charts(x) |>
    e_bar_3d(y, z1) |>
    e_x_axis_3d(axisLine = list(lineStyle = list(color = "red")))

  expect_equal(result$x$opts$xAxis3D[[1]]$axisLine$lineStyle$color, "red")
})

test_that("e_axis_3d works with timeline", {
  df <- data.frame(
    x = c("A", "B", "A", "B"),
    y = c("C", "D", "C", "D"),
    z = c(1, 2, 3, 4),
    grp = c(1, 1, 2, 2)
  )

  result <- df |>
    dplyr::group_by(grp) |>
    e_charts(x, timeline = TRUE) |>
    e_bar_3d(y, z) |>
    e_axis_3d(axis = "x", index = 0, axisLine = list(lineStyle = list(color = "green")))

  expect_equal(result$x$opts$baseOption$xAxis3D[[1]]$axisLine$lineStyle$color, "green")
})


# e_grid_3d ---------------------------------------------------------------
test_that("e_grid_3d errors informatively without e", {
  expect_error(e_grid_3d(), "missing e")
})

test_that("e_grid_3d errors informatively without attributes", {
  v <- LETTERS[1:10]
  matrix <- data.frame(
    x = sample(v, 300, replace = TRUE),
    y = sample(v, 300, replace = TRUE),
    z1 = rnorm(300, 10, 1),
    stringsAsFactors = FALSE
  ) |>
    dplyr::summarise(z1 = sum(z1), .by = c("x", "y"))

  e <- matrix |>
    e_charts(x) |>
    e_bar_3d(y, z1)

  expect_error(e_grid_3d(e), "no attribute")
})

test_that("e_grid_3d sets grid attributes", {
  v <- LETTERS[1:10]
  matrix <- data.frame(
    x = sample(v, 300, replace = TRUE),
    y = sample(v, 300, replace = TRUE),
    z1 = rnorm(300, 10, 1),
    stringsAsFactors = FALSE
  ) |>
    dplyr::summarise(z1 = sum(z1), .by = c("x", "y"))

  result <- matrix |>
    e_charts(x) |>
    e_bar_3d(y, z1) |>
    e_grid_3d(splitLine = list(lineStyle = list(color = "blue")))

  expect_equal(result$x$opts$grid3D[[1]]$splitLine$lineStyle$color, "blue")
})

test_that("e_grid_3d initializes grid3D if not existing", {
  v <- LETTERS[1:10]
  matrix <- data.frame(
    x = sample(v, 300, replace = TRUE),
    y = sample(v, 300, replace = TRUE),
    z1 = rnorm(300, 10, 1),
    stringsAsFactors = FALSE
  ) |>
    dplyr::summarise(z1 = sum(z1), .by = c("x", "y"))

  e <- matrix |>
    e_charts(x) |>
    e_bar_3d(y, z1)

  e$x$opts$grid3D <- NULL

  result <- e_grid_3d(e, splitLine = list(lineStyle = list(color = "red")))

  expect_equal(result$x$opts$grid3D[[1]]$splitLine$lineStyle$color, "red")
})

test_that("e_grid_3d works with timeline", {
  df <- data.frame(
    x = c("A", "B", "A", "B"),
    y = c("C", "D", "C", "D"),
    z = c(1, 2, 3, 4),
    grp = c(1, 1, 2, 2)
  )

  result <- df |>
    dplyr::group_by(grp) |>
    e_charts(x, timeline = TRUE) |>
    e_bar_3d(y, z) |>
    e_grid_3d(splitLine = list(lineStyle = list(color = "green")))

  expect_equal(result$x$opts$baseOption$grid3D[[1]]$splitLine$lineStyle$color, "green")
})

test_that("e_grid_3d works with custom index", {
  v <- LETTERS[1:10]
  matrix <- data.frame(
    x = sample(v, 300, replace = TRUE),
    y = sample(v, 300, replace = TRUE),
    z1 = rnorm(300, 10, 1),
    stringsAsFactors = FALSE
  ) |>
    dplyr::summarise(z1 = sum(z1), .by = c("x", "y"))

  result <- matrix |>
    e_charts(x) |>
    e_bar_3d(y, z1) |>
    e_grid_3d(index = 0, splitLine = list(lineStyle = list(color = "yellow")))

  expect_equal(result$x$opts$grid3D[[1]]$splitLine$lineStyle$color, "yellow")
})

