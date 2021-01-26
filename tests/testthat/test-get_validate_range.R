test_that("get_validate_range needs an argument", {
  expect_error(.get_validate_range())
})

test_that("get_validate_range works with numeric and character", {
  expect_silent(.get_validate_range(c(1, 2)))
  expect_silent(.get_validate_range(c("a", "b")))
})

test_that("get_validate_range works with numeric", {
  expect_silent(.get_validate_range(c(1, 2)))
})

test_that("get_validate_range returns a list", {
  expect_type(
    .get_validate_range(c(1, 2)),
    "list"
  )
})

test_that("get_validate_range works", {
  expect_equal(
    .get_validate_range(c(1, 2)),
    list(max = 2, min = 1)
  )

  expect_equal(
    .get_validate_range(c("a", "c", "b")),
    list(max = "c", min = "a")
  )
})
