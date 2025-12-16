# e_tooltip ---------------------------------------------------------------

test_that("e_tooltip errors informatively without e", {
  expect_error(e_tooltip(), "must pass e")
})

test_that("e_tooltip sets tooltip with default trigger", {
  e <- mtcars |> e_charts(mpg)
  result <- e_tooltip(e)
  expect_equal(result$x$opts$tooltip$trigger, "item")
})

test_that("e_tooltip sets tooltip with axis trigger", {
  e <- mtcars |> e_charts_("mpg")
  result <- e_tooltip(e, trigger = "axis")
  expect_equal(result$x$opts$tooltip$trigger, "axis")
})

test_that("e_tooltip passes additional arguments", {
  e <- mtcars |> e_charts(mpg)
  result <- e_tooltip(e, backgroundColor = "red")
  expect_equal(result$x$opts$tooltip$backgroundColor, "red")
})

test_that("e_tooltip works with timeline", {
  e <- mtcars |> dplyr::group_by(cyl) |> e_charts(mpg, timeline = TRUE)
  result <- e_tooltip(e)
  expect_equal(result$x$opts$baseOption$tooltip$trigger, "item")
  expect_true(e$x$tl)
})

test_that("e_tooltip works without formatter", {
  e <- mtcars |> e_charts(mpg)
  result <- e_tooltip(e)
  expect_null(result$x$opts$tooltip$formatter)
})


# Tooltip formatter -------------------------------------------------------

test_that("e_tooltip_item_formatter sets formatter for item_formatter", {
  fmt = e_tooltip_item_formatter(style = "decimal", digits = 2)
  expect_snapshot(fmt)
})

test_that("e_tooltip_choro_formatter sets formatter for item_formatter", {
  fmt = e_tooltip_choro_formatter(style = "decimal", digits = 2)
  expect_snapshot(fmt)
})

test_that("e_tooltip_pie_formatter sets formatter for item_formatter", {
  fmt = e_tooltip_pie_formatter(style = "decimal", digits = 2)
  expect_snapshot(fmt)
})

test_that("e_tooltip_pointer_formatter sets formatter for item_formatter", {
  fmt = e_tooltip_pointer_formatter(style = "decimal", digits = 2)
  expect_snapshot(fmt)
})

# e_legend ----------------------------------------------------------------
test_that("e_legend errors informatively without e", {
  expect_error(e_tooltip(), "must pass e")
})

test_that("e_legend sets legend with default show", {
  e <- mtcars |> e_charts(mpg) |> e_scatter(wt)
  result <- e_legend(e)
  expect_true(result$x$opts$legend$show)
})

test_that("e_legend hides legend when show is FALSE", {
  e <- mtcars |> e_charts(mpg) |> e_scatter(wt)
  result <- e_legend(e, show = FALSE)
  expect_false(result$x$opts$legend$show)
})

test_that("e_legend sets legend type", {
  e <- mtcars |> e_charts(mpg) |> e_scatter(wt) |> e_scatter(mpg)
  result <- e_legend(e, type = "scroll")
  expect_equal(result$x$opts$legend$type, "scroll")
})

test_that("e_legend passes additional arguments", {
  e <- mtcars |> e_charts(mpg) |> e_scatter(wt) |> e_scatter(mpg)
  result <- e_legend(e, orient = "vertical")
  expect_equal(result$x$opts$legend$orient, "vertical")
})

test_that("e_legend works with timeline", {
  e <- mtcars |> dplyr::group_by(cyl) |> e_charts(mpg, timeline = TRUE) |> e_scatter(wt)
  result <- e_legend(e)
  expect_equal(result$x$opts$baseOption$legend$show, TRUE)
})

test_that("e_legend sets single icon", {
  e <- mtcars |> e_charts(mpg) |> e_scatter(wt)
  result <- e_legend(e, icons = list("square"))
  expect_equal(result$x$opts$legend$icon, list("square"))
})

test_that("e_legend errors informatively with too few icons", {
  e <- mtcars |> e_charts_("mpg") |> e_scatter(wt) |> e_scatter(qsec)
  expect_snapshot(e_legend(e, icons = list("circle")), error = TRUE)
})

test_that("e_legend sets multiple icons", {
  e <- mtcars |> e_charts(mpg) |> e_scatter(wt) |> e_scatter(qsec)
  result <- e_legend(e, icons = list("circle", "rect"))
  expect_equal(result$x$opts$legend$data[[1]]$name, "wt")
  expect_equal(result$x$opts$legend$data[[1]]$icon, "circle")
  expect_equal(result$x$opts$legend$data[[2]]$name, "qsec")
  expect_equal(result$x$opts$legend$data[[2]]$icon, "rect")
})

# e_toolbox ---------------------------------------------------------------


