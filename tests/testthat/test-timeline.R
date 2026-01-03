

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
