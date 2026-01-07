

# e_country_names ---------------------------------------------------------
test_that("errors", {
  expect_error(e_country_names(), "must pass data and input")
  expect_error(e_country_names_(), "must pass data and input")
})

test_that("e_country_names with output", {
  cns <- data.frame(country = c("US", "BE"))
  c <- e_country_names(cns, input = country, output = clean)
  expect_s3_class(c, "data.frame")
  expect_equal(names(c), c("country", "clean"))
})

test_that("e_country_names with no output", {
  cns <- data.frame(country = c("US", "BE"))
  c <- e_country_names(cns, input = country)
  c_ <- e_country_names_(cns, input = "country")
  expect_s3_class(c, "data.frame")
  expect_equal(names(c), c("country"))
  expect_true(identical(c, c_))
})


# e_color_range -----------------------------------------------------------
test_that("errors", {
  expect_error(e_color_range(), "must pass data, input and output",)
  expect_error(e_color_range_(), "must pass data, input and output",)
})

test_that("e_color_range works", {
  df <- data.frame(val = 1:10)

  colors <- c("#bf444c", "#d88273", "#f6efa6") |> toupper()
  c <- e_color_range(df, input = val, output = out, colors = colors)

  expect_s3_class(c, "data.frame")
  expect_equal(names(c), c("val", "out"))
  expect_equal(c$val, 1:10)

  # Starts and ends with correct colors
  expect_equal(c$out[1], colors[1])
  expect_equal(c$out[10], colors[3])
})

test_that("e_color_range_ works when input is a character", {
  df <- data.frame(letters = LETTERS[1:10])

  colors <- c("#bf444c", "#d88273", "#f6efa6") |> toupper()
  c <- e_color_range(df, input = letters, output = out, colors = colors)

  expect_s3_class(c, "data.frame")
  expect_equal(names(c), c("letters", "out"))
  expect_equal(c$letters, LETTERS[1:10])

  # Starts and ends with correct colors
  expect_equal(c$out[1], colors[1])
  expect_equal(c$out[10], colors[3])
})


# e_get_data --------------------------------------------------------------
test_that("e_get_data works", {
  e <- iris |> e_charts()
  df <- e_get_data(e)
  expect_identical(df[[1]], iris)
})


# e_format_axis -----------------------------------------------------------
test_that("errors", {
  expect_error(e_format_axis(), "must pass e")
  expect_error(e_charts() |> e_format_axis(), "missing formatting")
})

test_that("e_format_x_axis works", {
  e <- mtcars |> e_charts(mpg) |> e_line(cyl) |> e_format_x_axis(suffix = "S", prefix = "P")
  expect_equal(e$x$opts$xAxis[[1]]$axisLabel$formatter, "P {value} S")
})

test_that("e_format_y_axis works", {
  e <- mtcars |> e_charts(mpg) |> e_line(cyl) |> e_format_y_axis(suffix = "S", prefix = "P")
  expect_equal(e$x$opts$yAxis[[1]]$axisLabel$formatter, "P {value} S")
})


# e_labels ----------------------------------------------------------------
test_that("errors", {
  expect_error(e_labels(), "must pass e")
})

test_that("e_labels works", {
  e <- mtcars |>
    e_chart(wt) |>
    e_scatter(qsec, mpg) |>
    e_labels(fontSize = 15, color = "red", position = "left")

  expect_equal(e$x$opts$series[[1]]$label$fontSize, 15)
  expect_equal(e$x$opts$series[[1]]$label$color, "red")
  expect_equal(e$x$opts$series[[1]]$label$position, "left")
})

test_that("e_labels works with timeline", {
  e <- mtcars |>
    group_by(cyl) |>
    e_chart(wt, timeline = TRUE) |>
    e_scatter(qsec, mpg) |>
    e_labels(fontSize = 15, color = "red")

  expect_equal(e$x$opts$baseOption$series[[1]]$label$fontSize, 15)
  expect_equal(e$x$opts$baseOption$series[[1]]$label$color, "red")
  expect_equal(e$x$opts$baseOption$series[[1]]$label$position, "top")
})


# e_list ------------------------------------------------------------------
test_that("e_list", {
  expect_error(e_list(), "must pass e")
  expect_error(e_charts() |> e_list(), "missing list")
})

test_that("e_list works", {
  N <- 20 # data points

  opts <- list(
    xAxis = list(
      type = "category",
      data = LETTERS[1:N]
    ),
    yAxis = list(
      type = "value"
    ),
    series = list(
      list(
        type = "line",
        data = round(runif(N, 5, 20))
      )
    )
  )

  e <- e_charts() |>
    e_list(opts)

  expect_equal(e$x$opts$series[[1]]$type, 'line')
  expect_equal(e$x$opts$xAxis$type, "category")
  expect_equal(e$x$opts$yAxis$type, "value")
})


# e_aria ------------------------------------------------------------------
test_that("e_aria turns on", {
  e <- e_charts()
  expect_null(e$x$opts$aria$enabled)

  e_aria <- e |>
    e_aria(TRUE, label = list(enabled = TRUE))

  expect_true(e_aria$x$opts$aria$enabled)
  expect_true(e_aria$x$opts$aria$label$enabled)
})

# e_inspect ---------------------------------------------------------------

test_that("e_list works", {
  e <- mtcars |>
    e_chart(wt) |>
    e_scatter(qsec, mpg)

  json <- e |>
    e_inspect(
      json = TRUE,
      pretty = TRUE
    )

  expect_s3_class(json, "json")
})


# echarts_from_json -------------------------------------------------------

test_that("echarts_from_json works", {
  e <- mtcars |>
    e_chart(wt) |>
    e_scatter(qsec, mpg, symbol_size = 20)

  # This only outputs opts
  json <- e |>
    e_inspect(
      json = TRUE,
      pretty = TRUE
    )

  built <- echarts_from_json(json, jswrapper = FALSE)

  # opts should match bc that's the only list outputted by e_inspect
  expect_equal(built$x$opts$yAxis, e$x$opts$yAxis)
  expect_equal(built$x$opts$xAxis, e$x$opts$xAxis)
  expect_equal(built$x$opts$legend, e$x$opts$legend)
  # data doesnt quite match but the rest does!
  expect_equal(built$x$opts$series[[1]][2:7], e$x$opts$series[[1]][2:7])
})


# e_zigzag ----------------------------------------------------------------
test_that("e_zigzag errors informatively without e", {
  expect_error(e_zigzag(), "must pass e")
})

test_that("e_zigzag errors informatively without axis", {
  e <- mtcars |> e_charts(mpg) |> e_scatter(wt)
  expect_error(e_zigzag(e), "must indicate which axis to zigzag")
})

test_that("e_zigzag errors informatively without start or end", {
  e <- mtcars |> e_charts(mpg) |> e_scatter(wt)
  expect_snapshot(e_zigzag(e, axis = "y", start = 400), error = TRUE)
  expect_snapshot(e_zigzag(e, axis = "y", end = 500), error = TRUE)
})

test_that("e_zigzag adds breaks to y axis", {
  df <- data.frame(
    x = c("a", "b", "c", "d", "c"),
    y = c(100, 200, 200, 700, 300)
  )

  result <- df |>
    e_charts(x) |>
    e_bar(y) |>
    e_zigzag(axis = "y", start = 400, end = 500)

  expect_s3_class(result, "echarts4r")
  expect_length(result$x$opts$yAxis[[1]]$breaks, 1)
  expect_equal(result$x$opts$yAxis[[1]]$breaks[[1]]$start, '400')
  expect_equal(result$x$opts$yAxis[[1]]$breaks[[1]]$end, '500')
  expect_equal(result$x$opts$yAxis[[1]]$breaks[[1]]$gap, "3%")
})

test_that("e_zigzag adds breaks to x axis", {
  df <- data.frame(
    x = c(100, 200, 200, 700, 300),
    y = c("a", "b", "c", "d", "e")
  )

  result <- df |>
    e_charts(x) |>
    e_bar(x) |>
    e_zigzag(axis = "x", start = 400, end = 500)

  expect_s3_class(result, "echarts4r")
  expect_length(result$x$opts$xAxis[[1]]$breaks, 1)
  expect_equal(result$x$opts$xAxis[[1]]$breaks[[1]]$start, '400')
  expect_equal(result$x$opts$xAxis[[1]]$breaks[[1]]$end, '500')
})

test_that("e_zigzag handles multiple breaks", {
  df <- data.frame(
    x = c("a", "b", "c", "d", "e"),
    y = c(100, 200, 200, 700, 300)
  )

  result <- df |>
    e_charts(x) |>
    e_bar(y) |>
    e_zigzag(axis = "y", start = c(125, 400), end = c(150, 500))

  expect_s3_class(result, "echarts4r")
  expect_length(result$x$opts$yAxis[[1]]$breaks, 2)
  expect_equal(result$x$opts$yAxis[[1]]$breaks[[1]]$start, '125')
  expect_equal(result$x$opts$yAxis[[1]]$breaks[[1]]$end, '150')
  expect_equal(result$x$opts$yAxis[[1]]$breaks[[2]]$start, '400')
  expect_equal(result$x$opts$yAxis[[1]]$breaks[[2]]$end, '500')
})

test_that("e_zigzag sets zigzagAmplitude parameter", {
  df <- data.frame(
    x = c("a", "b", "c"),
    y = c(100, 200, 700)
  )

  result <- df |>
    e_charts(x) |>
    e_bar(y) |>
    e_zigzag(axis = "y", start = 400, end = 500, zigzagAmplitude = 20)

  expect_equal(result$x$opts$yAxis[[1]]$breakArea$zigzagAmplitude, 20)
})

test_that("e_zigzag sets gap parameter", {
  df <- data.frame(
    x = c("a", "b", "c"),
    y = c(100, 200, 700)
  )

  result <- df |>
    e_charts(x) |>
    e_bar(y) |>
    e_zigzag(axis = "y", start = 400, end = 500, gap = "5%")

  expect_equal(result$x$opts$yAxis[[1]]$breaks[[1]]$gap, "5%")
})

test_that("e_zigzag appends additional arguments", {
  df <- data.frame(
    x = c("a", "b", "c"),
    y = c(100, 200, 700)
  )

  result <- df |>
    e_charts(x) |>
    e_bar(y) |>
    e_zigzag(axis = "y", start = 400, end = 500, customArg = "test")

  expect_equal(result$x$opts$yAxis[[1]]$breakArea$customArg, "test")
})

# e_jitter ----------------------------------------------------------------
test_that("e_jitter errors informatively without e", {
  expect_error(e_jitter(), "must pass e")

  e <- mtcars |> e_charts(mpg) |> e_scatter(wt)

  expect_error(e_jitter(e, axis = NULL), "must indicate which axis to apply jitter. e.g. 'x' or 'y'")

  tl <- mtcars |> dplyr::group_by(cyl) |> e_charts(mpg, timeline = TRUE) |> e_scatter(wt)
  expect_error(tl |> e_jitter(), "jitter is only supported with scatter plots")
})

test_that("e_zigzag appends additional arguments", {
  df <- data.frame(
    value = c(rnorm(50, mean = 5, sd = 1),
              rnorm(50, mean = 10, sd = 1),
              rnorm(50, mean = 15, sd = 1)),
    group = rep(c("Group A", "Group B", "Group C"), each = 50)
  )

  result <- df |> e_charts(group) |> e_scatter(value) |> e_jitter(jitter = 20, jitterOverlap = TRUE)

  expect_equal(result$x$opts$xAxis[[1]]$jitter, 20)
  expect_equal(result$x$opts$xAxis[[1]]$jitterOverlap, TRUE)
  expect_equal(result$x$opts$xAxis[[1]]$jitterMargin, 5)

  result <- df |> e_charts(group) |> e_scatter(value) |> e_jitter(jitter = 20, jitterOverlap = TRUE, axis = 'y')

  expect_equal(result$x$opts$yAxis[[1]]$jitter, 20)
  expect_equal(result$x$opts$yAxis[[1]]$jitterOverlap, TRUE)
  expect_equal(result$x$opts$yAxis[[1]]$jitterMargin, 5)
})
