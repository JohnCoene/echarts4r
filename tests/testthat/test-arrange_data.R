### .arrange_data_x

test_that(".arrange_data_x only takes a dataframe or a tibble as dataset", {
  x <- list(a = c(1, 2, 3, 2), b = c(3, 1, 6, 4))
  expect_error(.arrange_data_x(x, "a"))
})

test_that("argument x must be a character", {
  data <- head(mtcars)
  expect_error(.arrange_data_x(data, mpg))
  expect_silent(.arrange_data_x(data, "mpg"))
})

test_that(".arrange_data_x returns a dataframe", {
  data <- head(mtcars)
  x <- .arrange_data_x(data, "mpg")
  expect_s3_class(x, "data.frame")
})

test_that("argument reorder works", {

  ### output is different with or without reorder
  data <- head(mtcars)
  x <- .arrange_data_x(data, "mpg")
  y <- .arrange_data_x(data, "mpg", reorder = FALSE)
  expect_false(identical(x, y))

  ### if reorder is false, input = output
  expect_identical(data, y)
})

test_that(".arrange_data_x returns the same output as dplyr::arrange", {
  data <- head(mtcars)
  x <- .arrange_data_x(data, "mpg")
  y <- dplyr::arrange(data, mpg)
  expect_identical(x, y)
})
