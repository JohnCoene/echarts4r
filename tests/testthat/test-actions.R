# e_highlight -------------------------------------------------------------
test_that("e_highlight errors informatively without series_index or series_name", {
  e <- iris |> e_charts(Sepal.Length) |> e_line(Sepal.Width)
  expect_error(e_highlight(e), "must set index or name")
})

test_that("e_highlight adds highlight action to events with series_index", {
  e <- iris |> e_charts(Sepal.Length) |> e_line(Sepal.Width, symbolSize = 1)
  result <- e_highlight(e, series_index = 0)

  expect_s3_class(result, "echarts4r")
  expect_length(result$x$events, 1)
  expect_equal(result$x$events[[1]]$data$type, "highlight")
  expect_equal(result$x$events[[1]]$data$seriesIndex, list(0))
})

test_that("e_highlight adds highlight action to events with series_name", {
  e <- iris |> dplyr::group_by(Species) |> e_charts(Sepal.Length) |>
    e_line(Sepal.Width, symbolSize = 1)
  result <- e_highlight(e, series_name = "veriscolor")

  expect_s3_class(result, "echarts4r")
  expect_length(result$x$events, 1)
  expect_equal(result$x$events[[1]]$data$type, "highlight")
  expect_equal(result$x$events[[1]]$data$seriesName, list("veriscolor"))
})

test_that("e_highlight adds highlight action to button when btn provided", {

  e <- iris |> e_charts(Sepal.Length) |> e_line(Sepal.Width, symbolSize = 1) |>
    e_button("btn1")
  result <- e_highlight(e, series_index = 0, btn = "btn1")

  expect_s3_class(result, "echarts4r")
  expect_length(result$x$events, 0)
  expect_length(result$x$buttons$btn1, 1)
  expect_equal(result$x$buttons$btn1[[1]]$data$type, "highlight")
  expect_equal(result$x$buttons$btn1[[1]]$data$seriesIndex, list(0))
})

test_that("e_highlight appends to existing button actions", {
  e <- iris |> e_charts(Sepal.Length) |> e_line(Sepal.Width, symbolSize = 1) |>
    e_button("btn1")
  result <- e |>
    e_highlight(series_index = 0, btn = "btn1") |>
    e_highlight(series_index = 1, btn = "btn1")

  expect_length(result$x$buttons$btn1, 2)
  expect_equal(result$x$buttons$btn1[[1]]$data$seriesIndex, list(0))
  expect_equal(result$x$buttons$btn1[[2]]$data$seriesIndex, list(1))
})

test_that("e_highlight works with multiple series_index", {
  e <- iris |> e_charts(Sepal.Length) |> e_line(Sepal.Width, symbolSize = 1)
  result <- e_highlight(e, series_index = c(0, 1))

  expect_equal(result$x$events[[1]]$data$seriesIndex, list(0, 1))
})

test_that("e_highlight works with multiple series_name", {
  e <- iris |> dplyr::group_by(Species) |> e_charts(Sepal.Length) |> e_line(Sepal.Width, symbolSize = 1)
  result <- e_highlight(e, series_name = c("setosa", "versicolor"))

  expect_equal(result$x$events[[1]]$data$seriesName, list("setosa", "versicolor"))
})

# e_downplay --------------------------------------------------------------
test_that("e_downplay errors informatively without series_index or series_name", {
  e <- iris |> e_charts(Sepal.Length) |> e_line(Sepal.Width)
  expect_error(e_downplay(e), "must set index or name")
})

test_that("e_downplay adds highlight action to events with series_index", {
  e <- iris |> e_charts(Sepal.Length) |> e_line(Sepal.Width)
  result <- e_downplay(e, series_index = 0)

  expect_s3_class(result, "echarts4r")
  expect_length(result$x$events, 1)
  expect_equal(result$x$events[[1]]$data$type, "downplay")
  expect_equal(result$x$events[[1]]$data$seriesIndex, list(0))
})

test_that("e_downplay adds highlight action to events with series_name", {
  e <- iris |> dplyr::group_by(Species) |> e_charts(Sepal.Length) |>
    e_line(Sepal.Width, symbolSize = 1)
  result <- e_downplay(e, series_name = "veriscolor")

  expect_s3_class(result, "echarts4r")
  expect_length(result$x$events, 1)
  expect_equal(result$x$events[[1]]$data$type, "downplay")
  expect_equal(result$x$events[[1]]$data$seriesName, list("veriscolor"))
})

test_that("e_downplay adds highlight action to button when btn provided", {

  e <- iris |> e_charts(Sepal.Length) |> e_line(Sepal.Width) |>
    e_button("btn1")
  result <- e_downplay(e, series_index = 0, btn = "btn1")

  expect_s3_class(result, "echarts4r")
  expect_length(result$x$events, 0)
  expect_length(result$x$buttons$btn1, 1)
  expect_equal(result$x$buttons$btn1[[1]]$data$type, "downplay")
  expect_equal(result$x$buttons$btn1[[1]]$data$seriesIndex, list(0))
})

test_that("e_downplay appends to existing button actions", {
  e <- iris |> e_charts(Sepal.Length) |> e_line(Sepal.Width) |>
    e_button("btn1")
  result <- e |>
    e_downplay(series_index = 0, btn = "btn1") |>
    e_downplay(series_index = 1, btn = "btn1")

  expect_length(result$x$buttons$btn1, 2)
  expect_equal(result$x$buttons$btn1[[1]]$data$seriesIndex, list(0))
  expect_equal(result$x$buttons$btn1[[2]]$data$seriesIndex, list(1))
})

test_that("e_downplay works with multiple series_index", {
  e <- iris |> e_charts(Sepal.Length) |> e_line(Sepal.Width)
  result <- e_downplay(e, series_index = c(0, 1))

  expect_equal(result$x$events[[1]]$data$seriesIndex, list(0, 1))
})

test_that("e_downplay works with multiple series_name", {
  e <- iris |> dplyr::group_by(Species) |> e_charts(Sepal.Length) |> e_line(Sepal.Width)
  result <- e_downplay(e, series_name = c("setosa", "versicolor"))

  expect_equal(result$x$events[[1]]$data$seriesName, list("setosa", "versicolor"))
})

# e_showtip ---------------------------------------------------------------
test_that("e_showtip adds showtip action to events", {
  e <- iris |> e_charts(Sepal.Length) |> e_scatter(Sepal.Width) |> e_tooltip()
  result <- e_showtip(e, seriesIndex = 0) |> e_button("btn", "Show tooltip")

  expect_s3_class(result, "echarts4r")
  expect_length(result$x$events, 1)
  expect_equal(result$x$events[[1]]$data$type, "showTip")
})

test_that("e_showtip passes additional arguments", {
  e <- iris |> e_charts(Sepal.Length) |> e_scatter(Sepal.Width)
  result <- e_showtip(e, seriesIndex = 0, dataIndex = 1)

  expect_equal(result$x$events[[1]]$data$seriesIndex, 0)
  expect_equal(result$x$events[[1]]$data$dataIndex, 1)
})

test_that("e_showtip adds showtip action to button when btn provided", {
  e <- iris |> e_charts(Sepal.Length) |> e_scatter(Sepal.Width) |>
    e_button("btn1")
  result <- e_showtip(e, seriesIndex = 0, btn = "btn1")

  expect_s3_class(result, "echarts4r")
  expect_length(result$x$events, 0)
  expect_length(result$x$buttons$btn1, 1)
  expect_equal(result$x$buttons$btn1[[1]]$data$type, "showTip")
})

test_that("e_showtip appends to existing button actions", {
  e <- iris |> e_charts(Sepal.Length) |> e_scatter(Sepal.Width) |>
    e_button("btn1")
  result <- e |>
    e_showtip(seriesIndex = 0, btn = "btn1") |>
    e_showtip(seriesIndex = 1, btn = "btn1")

  expect_length(result$x$buttons$btn1, 2)
  expect_equal(result$x$buttons$btn1[[1]]$data$type, "showTip")
  expect_equal(result$x$buttons$btn1[[2]]$data$type, "showTip")
})

# e_hidetip ---------------------------------------------------------------
test_that("e_hidetip adds showtip action to events", {
  e <- iris |> e_charts(Sepal.Length) |> e_scatter(Sepal.Width) |> e_tooltip()
  result <- e_hidetip(e, seriesIndex = 0) |> e_button("btn", "Show tooltip")

  expect_s3_class(result, "echarts4r")
  expect_length(result$x$events, 1)
  expect_equal(result$x$events[[1]]$data$type, "hideTip")
})

test_that("e_hidetip passes additional arguments", {
  e <- iris |> e_charts(Sepal.Length) |> e_scatter(Sepal.Width)
  result <- e_hidetip(e, seriesIndex = 0, dataIndex = 1)

  expect_equal(result$x$events[[1]]$data$seriesIndex, 0)
  expect_equal(result$x$events[[1]]$data$dataIndex, 1)
})

test_that("e_hidetip adds showtip action to button when btn provided", {
  e <- iris |> e_charts(Sepal.Length) |> e_scatter(Sepal.Width) |>
    e_button("btn1")
  result <- e_hidetip(e, seriesIndex = 0, btn = "btn1")

  expect_s3_class(result, "echarts4r")
  expect_length(result$x$events, 0)
  expect_length(result$x$buttons$btn1, 1)
  expect_equal(result$x$buttons$btn1[[1]]$data$type, "hideTip")
})

test_that("e_hidetip appends to existing button actions", {
  e <- iris |> e_charts(Sepal.Length) |> e_scatter(Sepal.Width)
  result <- e |>
    e_hidetip(seriesIndex = 0, btn = "btn1") |>
    e_hidetip(seriesIndex = 1, btn = "btn1")

  expect_length(result$x$buttons$btn1, 2)
  expect_equal(result$x$buttons$btn1[[1]]$data$type, "hideTip")
  expect_equal(result$x$buttons$btn1[[2]]$data$type, "hideTip")
})

# e_zoom ------------------------------------------------------------------
test_that("e_zoom appends to existing button actions", {
  e <- cars |> e_charts(dist) |> e_scatter(speed) |> e_datazoom()
  result <- e |> e_zoom(start = 20,
                            end = 40, btn = "BUTTON") |>
    e_button("BUTTON", "Zoom in")

  expect_s3_class(result, "echarts4r")
  expect_equal(result$x$buttons$BUTTON[[1]]$data$type, "dataZoom")
  expect_equal(result$x$buttons$BUTTON[[1]]$data$start, 20)
  expect_equal(result$x$buttons$BUTTON[[1]]$data$end, 40)
})

# e_visual_map_range ------------------------------------------------------
test_that("e_visual_map_range appends to existing button actions", {
  e <- cars |> e_charts(dist) |> e_scatter(speed) |> e_visual_map(dist)
  result <- e |> e_visual_map_range(selected = list(60, 120), btn = "BUTTON")

  expect_s3_class(result, "echarts4r")
  expect_equal(result$x$buttons$BUTTON[[1]]$data$type, "selectDataRange")
  expect_equal(result$x$buttons$BUTTON[[1]]$data$selected, list(60, 120))
})

