
test_that("e_on errors informatively without query or handler", {
  df <- data.frame(x = 1:3, y = 4:6)
  e <- df |> e_charts(x)

  expect_error(e_on(e, query = list(seriesName = "test")), "must pass query and handler")
  expect_error(e_on(e, handler = "function(){}"), "must pass query and handler")
})
# TODO review this
test_that("e_on initializes on list when empty", {
  df <- data.frame(x = 1:3, y = 4:6)
  e <- df |> e_charts(x)
  e$x$on <- NULL

  result <- e_on(e, query = list(seriesName = "test"), handler = "function(){}")

  expect_type(result$x$on, "list")
  expect_length(result$x$on, 1)
})

test_that("e_on appends to existing on list", {
  df <- data.frame(x = 1:3, y = 4:6)
  e <- df |> e_charts(x)
  e$x$on <- list(list(event = "click", query = list(), handler = htmlwidgets::JS("function(){}")))

  result <- e_on(e, query = list(seriesName = "test"), handler = "function(){alert('test')}")

  expect_length(result$x$on, 2)
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
})

test_that("e_on stores query correctly", {
  df <- data.frame(x = 1:3, y = 4:6)
  e <- df |> e_charts(x)

  result <- e_on(e, query = list(seriesName = "dist"), handler = "function(){}")

  expect_equal(result$x$on[[1]]$query, list(seriesName = "dist"))
})
