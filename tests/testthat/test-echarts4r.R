test_that("echarts_build removes data and mapping", {
  e <- mtcars |> e_charts(mpg) |> e_scatter(wt)
  result <- echarts_build(e)
  expect_null(result$x$data)
  expect_null(result$x$mapping)
})

test_that("echarts_build applies font family option when set", {
  withr::local_options(ECHARTS4R_FONT_FAMILY = "Arial")
  e <- mtcars |> e_charts(mpg) |> e_scatter(wt)
  result <- echarts_build(e)
  expect_equal(result$x$opts$textStyle$fontFamily, "Arial")
})

test_that("echarts_build does not apply font family when option is NULL", {
  withr::local_options(ECHARTS4R_FONT_FAMILY = NULL)
  e <- mtcars |> e_charts(mpg) |> e_scatter(wt)
  result <- echarts_build(e)
  expect_null(result$x$opts$textStyle$fontFamily)
})

# e_charts ----------------------------------------------------------------

test_that("e_charts and e_charts_ expects errors", {
   expect_error(e_charts(timeline = TRUE), "timeline expects data")
   expect_error(iris |> e_charts(timeline = TRUE), "must pass grouped data when timeline = TRUE")

   expect_error(e_charts_(timeline = TRUE), "timeline expects data")
   expect_error(iris |> e_charts_(timeline = TRUE), "must pass grouped data when timeline = TRUE")
})

test_that("e_charts and e_charts_ expects theme is applied", {
  withr::local_options(ECHARTS4R_THEME = "azul")
  result <- iris |> e_charts()
  expect_equal(result$x$theme, "azul")

  result2 <- iris |> e_charts_()
  expect_equal(result2$x$theme, "azul")
})


test_that("e_charts works without x argument", {
  result <- e_charts(mtcars)
  expect_null(result$chart$x$mapping$x)
})

test_that("e_charts sets mapping when x provided", {
  result <- e_charts(data = mtcars, x = mpg)
  expect_equal(result$x$mapping$x, "mpg")
  expect_equal(result$x$mapping$x_class, "numeric")
})

test_that("e_charts errors informatively with timeline but no data", {
  expect_error(e_charts(timeline = TRUE), "timeline expects data")
})

test_that("e_charts errors informatively with timeline and ungrouped data", {
  expect_error(e_charts(mtcars, mpg, timeline = TRUE), "must pass grouped data when timeline = TRUE")
})

test_that("e_charts works with grouped data and timeline", {
  grouped_data <- mtcars |> dplyr::group_by(cyl)
  result <- e_charts(grouped_data, mpg, timeline = TRUE)
  expect_true(result$x$tl)
  expect_equal(result$x$opts$baseOption$timeline$axisType, "category")
})

test_that("e_charts sets character x axis for timeline", {
  df <- data.frame(x = c("a", "b", "c"), y = 1:3)
  grouped_data <- df |> dplyr::group_by(x)
  result <- e_charts(grouped_data, x, timeline = TRUE)
  expect_equal(result$x$opts$baseOption$xAxis[[1]]$type, "category")
})

test_that("e_charts sets time x axis for timeline with POSIXct", {
  df <- data.frame(x = as.POSIXct(c("2020-01-01", "2020-01-02")), y = 1:2)
  grouped_data <- df |> dplyr::group_by(x)
  result <- e_charts(grouped_data, x, timeline = TRUE)
  expect_equal(result$x$opts$baseOption$xAxis[[1]]$type, "time")
})

test_that("e_charts sets value x axis for timeline with numeric", {
  grouped_data <- mtcars |> dplyr::group_by(cyl)
  result <- e_charts(grouped_data, mpg, timeline = TRUE)
  expect_equal(result$x$opts$baseOption$xAxis[[1]]$type, "value")
  expect_true(result$x$mapping$include_x)
})

# e_charts_
test_that("e_charts_ works without x argument", {
  result <- e_charts_(mtcars)
  expect_null(result$chart$x$mapping$x)
})

test_that("e_charts_ sets mapping when x provided", {
  result <- e_charts_(data = mtcars, x = "mpg")
  expect_equal(result$x$mapping$x, "mpg")
  expect_equal(result$x$mapping$x_class, "numeric")
})

test_that("e_charts_ errors informatively with timeline but no data", {
  expect_error(e_charts_(timeline = TRUE), "timeline expects data")
})

test_that("e_charts_ errors informatively with timeline and ungrouped data", {
  expect_error(e_charts_(mtcars, "mpg", timeline = TRUE), "must pass grouped data when timeline = TRUE")
})

test_that("e_charts_ works with grouped data and timeline", {
  grouped_data <- mtcars |> dplyr::group_by(cyl)
  result <- e_charts_(grouped_data, "mpg", timeline = TRUE)
  expect_true(result$x$tl)
  expect_equal(result$x$opts$baseOption$timeline$axisType, "category")
})

test_that("e_charts_ sets character x axis for timeline", {
  df <- data.frame(x = c("a", "b", "c"), y = 1:3)
  grouped_data <- df |> dplyr::group_by(x)
  result <- e_charts_(grouped_data, "x", timeline = TRUE)
  expect_equal(result$x$opts$baseOption$xAxis[[1]]$type, "category")
})

test_that("e_charts_ sets time x axis for timeline with POSIXct", {
  df <- data.frame(x = as.POSIXct(c("2020-01-01", "2020-01-02")), y = 1:2)
  grouped_data <- df |> dplyr::group_by(x)
  result <- e_charts_(grouped_data, "x", timeline = TRUE)
  expect_equal(result$x$opts$baseOption$xAxis[[1]]$type, "time")
})

test_that("e_charts_ sets value x axis for timeline with numeric", {
  grouped_data <- mtcars |> dplyr::group_by(cyl)
  result <- e_charts_(grouped_data, "mpg", timeline = TRUE)
  expect_equal(result$x$opts$baseOption$xAxis[[1]]$type, "value")
  expect_true(result$x$mapping$include_x)
})

# e_data ------------------------------------------------------------------

test_that("e_data grabs data from e_chart when missing", {
  e <- iris |> e_charts_() |> e_data()
  data <- e$x$data[[1]]
  expect_identical(data, iris)
})

# render in shiny -------------------------------------------------------------------

test_that("echarts4rOutput creates shiny widget output", {
  result <- echarts4rOutput("myChart")
  expect_s3_class(result, "shiny.tag.list")
})

test_that("echarts4rOutput sets default width and height", {
  result <- echarts4rOutput("myChart")
  expect_match(as.character(result), "100%")
  expect_match(as.character(result), "400px")
})

test_that("echarts4rOutput accepts custom width and height", {
  result <- echarts4rOutput("myChart", width = "500px", height = "300px")
  expect_match(as.character(result), "500px")
  expect_match(as.character(result), "300px")
})

test_that("renderEcharts4r returns a shiny render function", {
  result <- renderEcharts4r(mtcars |> e_charts(mpg) |> e_scatter(wt))
  expect_s3_class(result, "shiny.render.function")
})

test_that("renderEcharts4r forces quoted expression when quoted is FALSE", {
  expr <- quote(mtcars |> e_charts(mpg) |> e_scatter(wt))
  result <- renderEcharts4r(expr, quoted = FALSE)
  expect_s3_class(result, "shiny.render.function")
})

test_that("renderEcharts4r accepts quoted expression when quoted is TRUE", {
  expr <- quote(mtcars |> e_charts(mpg) |> e_scatter(wt))
  result <- renderEcharts4r(expr, quoted = TRUE)
  expect_s3_class(result, "shiny.render.function")
})

test_that("renderEcharts4r uses parent frame as default environment", {
  result <- renderEcharts4r(mtcars |> e_charts(mpg) |> e_scatter(wt))
  expect_s3_class(result, "shiny.render.function")
})

test_that("echarts4rProxy returns proxy without data", {
  result <- echarts4rProxy("myChart")
  expect_s3_class(result, "echarts4rProxy")
  expect_equal(result$id, "myChart")
  expect_null(result$session)
})

test_that("echarts4rProxy returns proxy with data and x", {
  result <- echarts4rProxy("myChart", data = mtcars, x = mpg)
  expect_s3_class(result, "echarts4rProxy")
  expect_equal(result$id, "myChart")
  expect_s3_class(result$chart, "echarts4r")
})

test_that("echarts4rProxy works without x argument", {
  result <- echarts4rProxy("myChart", mtcars)
  expect_s3_class(result, "echarts4rProxy")
  expect_null(result$chart$x$mapping$x)
})

test_that("echarts4rProxy sets mapping when x provided", {
  result <- echarts4rProxy("myChart", data = mtcars, x = mpg)
  expect_equal(result$chart$x$mapping$x, "mpg")
  expect_equal(result$chart$x$mapping$x_class, "numeric")
})

test_that("echarts4rProxy errors informatively with timeline but no data", {
  expect_error(echarts4rProxy("myChart", timeline = TRUE), "timeline expects data")
})

test_that("echarts4rProxy errors informatively with timeline and ungrouped data", {
  expect_error(echarts4rProxy("myChart", mtcars, mpg, timeline = TRUE) , "must pass grouped data when timeline = TRUE")
})

test_that("echarts4rProxy works with grouped data and timeline", {
  grouped_data <- mtcars |> dplyr::group_by(cyl)
  result <- echarts4rProxy("myChart", grouped_data, mpg, timeline = TRUE)
  expect_true(result$chart$x$tl)
  expect_equal(result$chart$x$opts$baseOption$timeline$axisType, "category")
})

test_that("echarts4rProxy sets character x axis for timeline", {
  df <- data.frame(x = c("a", "b", "c"), y = 1:3)
  grouped_data <- df |> dplyr::group_by(x)
  result <- echarts4rProxy("myChart", grouped_data, x, timeline = TRUE)
  expect_equal(result$chart$x$opts$baseOption$xAxis[[1]]$type, "category")
})

test_that("echarts4rProxy sets time x axis for timeline with POSIXct", {
  df <- data.frame(x = as.POSIXct(c("2020-01-01", "2020-01-02")), y = 1:2)
  grouped_data <- df |> dplyr::group_by(x)
  result <- echarts4rProxy("myChart", grouped_data, x, timeline = TRUE)
  expect_equal(result$chart$x$opts$baseOption$xAxis[[1]]$type, "time")
})

test_that("echarts4rProxy sets value x axis for timeline with numeric", {
  grouped_data <- mtcars |> dplyr::group_by(cyl)
  result <- echarts4rProxy("myChart", grouped_data, mpg, timeline = TRUE)
  expect_equal(result$chart$x$opts$baseOption$xAxis[[1]]$type, "value")
  expect_true(result$chart$x$mapping$include_x)
})
