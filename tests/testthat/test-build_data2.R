test_that("build_data2 needs a dataframe or tibble as data input", {
  x <- mtcars
  y <- dplyr::as_tibble(mtcars)
  z <- list(mpg = c(1, 2, 3), cyl = 4, 5, 6)

  expect_silent(.build_data2(x, "mpg"))
  expect_silent(.build_data2(y, "mpg"))
  expect_error(.build_data2(z, "mpg"))
  expect_error(.build_data2())
})

test_that("build_data2 needs a (vector of) character or a numeric as other input", {
  x <- mtcars
  a <- "mpg"
  b <- "cyl"
  expect_silent(.build_data2(x, c("mpg", "cyl")))
  expect_error(.build_data2(x, c(mpg, cyl)))
  expect_silent(.build_data2(x, a, b))

  expect_silent(.build_data2(x, c(1, 2)))

  expect_equal(
    length(unlist(.build_data2(x))),
    0
  )
})

test_that("build_data2 creates a list", {
  x <- mtcars
  y <- .build_data2(mtcars, "mpg")

  expect_type(y, "list")
})
