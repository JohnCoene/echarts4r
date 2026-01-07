

# e_timeline_opts ---------------------------------------------------------


test_that("e_timeline_opts works", {
  e <-iris |>
    group_by(Species) |>
    e_charts(Sepal.Length, timeline = TRUE) |>
    e_line(Sepal.Width)
  result <-   e_timeline_opts(e,
    autoPlay = TRUE,
    rewind = TRUE
  )
  expect_equal(result$x$opts$baseOption$timeline$axisType, "category")

  expect_true(result$x$opts$baseOption$timeline$autoPlay)
  expect_true(result$x$opts$baseOption$timeline$rewind)
})


# e_timeline_serie  -------------------------------------------------------
test_that("e_timeline_opts works", {
  e <-iris |>
    group_by(Species) |>
    e_charts(Sepal.Length, timeline = TRUE) |>
    e_line(Sepal.Width)
  result <- e |>
    e_timeline_serie(
      title = list(
        list(text = "setosa"),
        list(text = "versicolor"),
        list(text = "virginica")
      )
    )

  expect_equal(result$x$opts$options[[1]]$title[[1]]$text, "setosa")
  expect_equal(result$x$opts$options[[2]]$title[[1]]$text, "versicolor")
  expect_equal(result$x$opts$options[[3]]$title[[1]]$text, "virginica")
})

# e_timeline_on_serie  ----------------------------------------------------
test_that("expects error when missing e ", {
  expect_error(e_timeline_opts(), "must pass e")
  expect_error(e_timeline_serie(), "must pass e")
  expect_error(e_timeline_on_serie(), "must pass e or serie_index")

  expect_warning(e_charts() |> e_timeline_opts(), "timeline not enabled in e_chart")

  expect_error(e_charts() |> e_timeline_serie(), "no arguments passed")
})

test_that("e_timeline_on_serie adds options to series at specified index", {
  e <- iris |>
    group_by(Species) |>
    e_charts(Sepal.Length, timeline = TRUE) |>
    e_line(Sepal.Width)

  result <- e_timeline_on_serie(e, lineStyle = list(
    list(color = "red"),
    list(color = "blue"),
    list(color = "green")
  ), serie_index = 1)

  expect_equal(result$x$opts$options[[1]]$series[[1]]$lineStyle, list(color = "red"))
  expect_equal(result$x$opts$options[[2]]$series[[1]]$lineStyle, list(color = "blue"))
  expect_equal(result$x$opts$options[[3]]$series[[1]]$lineStyle, list(color = "green"))
})

test_that("e_timeline_on_serie works with multiple arguments", {
  e <- iris |>
    group_by(Species) |>
    e_charts(Sepal.Length, timeline = TRUE) |>
    e_line(Sepal.Width)

  result <- e_timeline_on_serie(
    e,
    lineStyle = list(
      list(width = 2),
      list(width = 3),
      list(width = 4)
    ),
    smooth = list(TRUE, FALSE, TRUE),
    serie_index = 1
  )

  expect_equal(result$x$opts$options[[1]]$series[[1]]$lineStyle, list(width = 2))
  expect_equal(result$x$opts$options[[1]]$series[[1]]$smooth, TRUE)
  expect_equal(result$x$opts$options[[2]]$series[[1]]$lineStyle, list(width = 3))
  expect_equal(result$x$opts$options[[2]]$series[[1]]$smooth, FALSE)
  expect_equal(result$x$opts$options[[3]]$series[[1]]$lineStyle, list(width = 4))
  expect_equal(result$x$opts$options[[3]]$series[[1]]$smooth, TRUE)
})
