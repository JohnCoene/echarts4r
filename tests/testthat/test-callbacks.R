
test_that("e_on errors informatively without query or handler", {
  df <- data.frame(x = 1:3, y = 4:6)
  e <- df |> e_charts(x)

  expect_error(e_on(e, query = list(seriesName = "test")), "must pass query and handler")
  expect_error(e_on(e, handler = "function(){}"), "must pass query and handler")
})

test_that("e_on sets default event to click", {
  df <- data.frame(x = 1:3, y = 4:6)
  e <- df |> e_charts(x)

  result <- e_on(e, query = list(seriesName = "test"), handler = "function(){}")

  expect_equal(result$x$on[[1]]$event, "click")
})

test_that("e_on accepts custom event", {
  df <- data.frame(x = 1:3, y = 4:6)
  e <- df |> e_charts(x)

  result <- e_on(e, query = list(seriesName = "test"), handler = "function(){}", event = "mouseover")

  expect_equal(result$x$on[[1]]$event, "mouseover")
})

test_that("e_on wraps handler in htmlwidgets::JS", {
  df <- data.frame(x = 1:3, y = 4:6)
  e <- df |> e_charts(x)

  result <- e_on(e, query = list(seriesName = "test"), handler = "function(){alert('test')}")

  expect_s3_class(result$x$on[[1]]$handler, "JS_EVAL")

  expect_equal(result$x$on[[1]]$query, list(seriesName = "test"))
})



test_that("e_off errors informatively without query or handler", {
  df <- data.frame(x = 1:3, y = 4:6)
  e <- df |> e_charts(x)

  expect_error(e_off(e, query = list(seriesName = "test")), "must pass query and handler")
  expect_error(e_off(e, handler = "function(){}"), "must pass query and handler")
})

test_that("e_off sets default event to click", {
  df <- data.frame(x = 1:3, y = 4:6)
  e <- df |> e_charts(x)

  result <- e_off(e, query = list(seriesName = "test"), handler = "function(){}")

  expect_equal(result$x$off[[1]]$event, "click")
})

test_that("e_off accepts custom event", {
  df <- data.frame(x = 1:3, y = 4:6)
  e <- df |> e_charts(x)

  result <- e_off(e, query = list(seriesName = "test"), handler = "function(){}", event = "mouseover")

  expect_equal(result$x$off[[1]]$event, "mouseover")
})

test_that("e_off wraps handler in htmlwidgets::JS", {
  df <- data.frame(x = 1:3, y = 4:6)
  e <- df |> e_charts(x)

  result <- e_off(e, query = list(seriesName = "test"), handler = "function(){alert('test')}")

  expect_s3_class(result$x$off[[1]]$handler, "JS_EVAL")

  expect_equal(result$x$off[[1]]$query, list(seriesName = "test"))
})

test_that("e_off wraps handler in htmlwidgets::JS", {
  e <- e_charts()
  expect_null(e$x$zr)
  result <- e_get_zr(e)
  expect_true(result$x$zr)
})

