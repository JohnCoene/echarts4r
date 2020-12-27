test_that(".jitter output is equal to input if first argument is not numeric", {
  x <- mtcars
  expect_identical(.jitter(x), x)
})

test_that(".jitter output is different from input if arguments are good", {
  x <- mtcars
  expect_false(identical(.jitter(x$mpg), x))
})
