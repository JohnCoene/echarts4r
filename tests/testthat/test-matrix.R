
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
    e_matrix(xAxis = "Class", yAxis = "Grade", customArg = "test")

  expect_equal(plot$x$opts$matrix$customArg, "test")
})

# e_matrix_raw ----------------------------------------------------------------


