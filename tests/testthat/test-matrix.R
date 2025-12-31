
# get_base_nodes ----------------------------------------------------------

test_that("returns empty list for atomic values", {
  expect_equal(get_base_nodes(1), list())
  expect_equal(get_base_nodes("a"), list())
  expect_equal(get_base_nodes(TRUE), list())
})

test_that("returns empty list for data frames", {
  df <- data.frame(x = 1:3, y = 4:6)
  expect_equal(get_base_nodes(df), list())
})

test_that("returns unnamed atomic elements from list", {
  x <- list(1, 2, 3)
  expect_equal(get_base_nodes(x), list(1, 2, 3))
})

test_that("returns unnamed data frames from list", {
  df <- data.frame(x = 1:2)
  x <- list(df, df)
  expect_equal(get_base_nodes(x), list(df, df))
})

test_that("skips named atomic elements", {
  x <- list(a = 1, b = 2)
  expect_equal(get_base_nodes(x), list())
})

test_that("recursively finds unnamed atomic elements in nested lists", {
  x <- list(list(1, 2), list(3, 4))
  expect_equal(get_base_nodes(x), list(1, 2, 3, 4))
})

test_that("handles mixed named and unnamed elements", {
  x <- list(a = 1, 2, list(3, b = 4))
  expect_equal(get_base_nodes(x), list(2, 3))
})

test_that("returns unnamed elements at all nesting levels", {
  x <- list(list(list(1)), list(2))
  expect_equal(get_base_nodes(x), list(1, 2))
})

test_that("handles empty nested lists", {
  x <- list(list(), list(1))
  expect_equal(get_base_nodes(x), list(1))
})

test_that("handles NULL values in list", {
  x <- list(NULL, 1, list(NULL, 2))
  expect_equal(get_base_nodes(x), list(NULL, 1, NULL, 2))
})


# e_matrix ----------------------------------------------------------------

test_that("e_matrix fails informatively without e", {
  expect_error(e_matrix(), "must pass e")
})

test_that("e_matrix fails informatively without xAxis or yAxis", {
  df <- data.frame(Class = c("A", "B"), Grade = c("1", "2"))
  e <- df |> e_charts()

  expect_snapshot(e_matrix(e, xAxis = "Class"), error = TRUE)
  expect_snapshot(e_matrix(e, yAxis = "Grade"), error = TRUE)
})

test_that("e_matrix creates matrix structure with unique values", {
  df <- data.frame(
    Class = rep(c("Class1", "Class2"), 4),
    Grade = rep(c("Grade1", "Grade2"), each = 2)
  )

  plot <- df |> e_charts() |> e_matrix(xAxis = "Class", yAxis = "Grade")

  expect_s3_class(plot, "echarts4r")
  expect_equal(plot$x$opts$matrix$x$name, "Class")
  expect_equal(plot$x$opts$matrix$y$name, "Grade")
  expect_equal(plot$x$opts$matrix$x$data, list("Class1", "Class2"))
  expect_equal(plot$x$opts$matrix$y$data, list("Grade1", "Grade2"))
})

test_that("e_matrix appends additional arguments", {
  df <- data.frame(Class = c("A"), Grade = c("1"))

  plot <- df |> e_charts() |>
    e_matrix(xAxis = "Class", yAxis = "Grade", bottom = 20)

  expect_equal(plot$x$opts$matrix$bottom, 20)
})

# e_matrix_raw ----------------------------------------------------------------

test_that("e_matrix_raw fails informatively without rows or cols", {
  expect_snapshot(e_matrix_raw(rows = 3), error = TRUE)
  expect_snapshot(e_matrix_raw(cols = 3), error = TRUE)
  expect_snapshot(e_matrix_raw(), error = TRUE)
})

test_that("e_matrix_raw creates matrix structure with specified dimensions", {
  plot <- e_matrix_raw(rows = 3, cols = 4)

  expect_s3_class(plot, "echarts4r")
  expect_null(plot$x$opts$yAxis)
  expect_length(plot$x$opts$matrix$x$data, 4)
  expect_length(plot$x$opts$matrix$y$data, 3)
  expect_false(plot$x$opts$matrix$x$show)
  expect_false(plot$x$opts$matrix$y$show)
})

test_that("e_matrix_raw appends additional arguments", {
  plot <- e_matrix_raw(rows = 2, cols = 2, customArg = "test")

  expect_equal(plot$x$opts$matrix$customArg, "test")
})

test_that("e_matrix_raw creates proper NA structure", {
  plot <- e_matrix_raw(rows = 2, cols = 3)

  expect_true(all(sapply(plot$x$opts$matrix$x$data, is.na)))
  expect_true(all(sapply(plot$x$opts$matrix$y$data, is.na)))
})

# e_matrix_parent ---------------------------------------------------------
test_that("e_matrix_parent fails informatively without e", {
  expect_error(e_matrix_parent(),
               "must pass e")
})

test_that("e_matrix_parent creates parent node on x axis with direct children", {
  df <- data.frame(
    Class = rep(c("Class1", "Class2", "Class3"), each = 3),
    Grade = c("Grade1", "Grade2", "Grade3")
  )

  plot <- df |>
    e_charts() |>
    e_matrix(xAxis = "Class", yAxis = "Grade") |>
    e_matrix_parent(axis = "x", value = "Primary", children = c("Class1", "Class2"))

  expect_s3_class(plot, "echarts4r")
  expect_length(plot$x$opts$matrix$x$data, 2)
  expect_equal(plot$x$opts$matrix$x$data[[2]]$value, "Primary")
  expect_length(plot$x$opts$matrix$x$data[[2]]$children, 2)
})

test_that("e_matrix_parent creates parent node on y axis with direct children", {
  df <- data.frame(
    Class = rep(c("Class1", "Class2", "Class3"), each = 3),
    Grade = c("Grade1", "Grade2", "Grade3")
  )

  plot <- df |>
    e_charts() |>
    e_matrix(xAxis = "Class", yAxis = "Grade") |>
    e_matrix_parent(axis = "y", value = "High", children = c("Grade1", "Grade2"))

  expect_s3_class(plot, "echarts4r")
  expect_length(plot$x$opts$matrix$y$data, 2)
  expect_equal(plot$x$opts$matrix$y$data[[2]]$value, "High")
  expect_length(plot$x$opts$matrix$y$data[[2]]$children, 2)
})

test_that("e_matrix_parent fails informatively when children not found", {
  df <- data.frame(
    Class = rep(c("Class1", "Class2"), each = 2),
    Grade = c("Grade1", "Grade2")
  )

  plot <- df |>
    e_charts() |>
    e_matrix(xAxis = "Class", yAxis = "Grade")

  expect_error(e_matrix_parent(plot, axis = "x", value = "Test", children = "NonExistent"), "No children found in the data")

  expect_error(e_matrix_parent(plot, axis = "y", value = "Test", children = "NonExistent"), "No children found in the data")
})

test_that("e_matrix_parent appends additional arguments", {
  df <- data.frame(
    Class = rep(c("Class1", "Class2"), each = 2),
    Grade = c("Grade1", "Grade2")
  )

  plot <- df |>
    e_charts() |>
    e_matrix(xAxis = "Class", yAxis = "Grade") |>
    e_matrix_parent(axis = "x", value = "Primary", children = "Class1", customArg = "test")

  expect_equal(plot$x$opts$matrix$x$data[[2]]$customArg, "test")
})

# TODO check this - maybe need value
# test_that("e_matrix_parent finds nested children", {
#   df <- data.frame(
#     Class = rep(c("Class1", "Class2"), each = 2),
#     Grade = c("Grade1", "Grade2")
#   )
#
#   plot <- df |>
#     e_charts() |>
#     e_matrix(xAxis = "Class", yAxis = "Grade") |>
#     e_matrix_parent(axis = "x", value = "Primary", children = "Class1")
#   plot |>
#     e_matrix_parent(axis = "x", value = "AllClasses", children = "Class1")
#
#   expect_s3_class(plot, "echarts4r")
#   expect_equal(plot$x$opts$matrix$x$data[[2]]$value, "AllClasses")
# })
