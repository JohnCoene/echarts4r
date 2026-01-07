test_that("errors informatively without e", {
  expect_error(e_axis_(), "must pass e")
  e <- mtcars |> e_charts(mpg)
  expect_error(e_axis_(e), "no attribute")
  expect_error(e_x_axis_(), "must pass e")
  expect_error(e_y_axis_(), "must pass e")
  expect_error(e_z_axis_(), "must pass e")
  expect_error(e_x_axis(), "must pass e")
  expect_error(e_y_axis(), "must pass e")
  expect_error(e_z_axis(), "must pass e")
  expect_error(e_grid(), "must pass e")
  expect_error(e_radius_axis(), "must pass e")
  expect_error(e_radius_axis_(), "must pass e")
  expect_error(e_angle_axis(), "must pass e")
  expect_error(e_angle_axis_(), "must pass e")
  expect_error(e_radar_opts(), "must pass e")
  expect_error(e_single_axis(), "must pass e")
  expect_error(e_axis_labels(), "must pass e")
  expect_error(e_hide_grid_lines(), "must pass e")
  expect_error(e_axis_stagger(), "must pass e")
})

# e_axis_ -----------------------------------------------------------------
test_that("e_axis_ sets axis attributes based on serie", {
  e <- mtcars |> e_charts(mpg) |> e_scatter(wt)
  result <- e_axis(e, serie = wt, axis = "y", min = 1, max = 6)
  expect_equal(result$x$opts$yAxis[[1]]$min, 1)
  expect_equal(result$x$opts$yAxis[[1]]$max, 6)
})

test_that("e_axis_ calculates min and max from serie with margin", {
  df <- data.frame(x = 1:5, y = c(2, 4, 6, 8, 10))
  e <- df |> e_charts(x) |> e_scatter(y)
  result <- e_axis_(e, serie = "y", axis = "y", margin = 1)
  expect_equal(result$x$opts$yAxis[[1]]$min, 1)
  expect_equal(result$x$opts$yAxis[[1]]$max, 11)
})

test_that("e_axis_ sets formatter", {
  e <- mtcars |> e_charts(mpg) |> e_scatter(wt)
  fmt <- htmlwidgets::JS("function(value) { return value + 5; }")
  result <- e_axis_(e, axis = "x", formatter = fmt)
  expect_equal(result$x$opts$xAxis[[1]]$axisLabel$formatter, fmt)
})

test_that("e_axis_ works with timeline", {
  e <- mtcars |> dplyr::group_by(cyl) |> e_charts(mpg, timeline = TRUE) |> e_scatter(wt)
  result <- e_axis_(e, axis = "x", min = 10, max = 35)
  expect_equal(result$x$opts$baseOption$xAxis[[1]]$min, 10)
  expect_equal(result$x$opts$baseOption$xAxis[[1]]$max, 35)
})

test_that("e_axis_ handles deep list structure", {
  e <- mtcars |> e_charts(mpg) |> e_scatter(wt) |> e_scatter(qsec)
  result <- e_axis_(e, axis = "y", index = 0, min = 0)
  expect_equal(result$x$opts$yAxis[[1]]$min, 0)
})

test_that("e_axis_ resets to index 1 when r.index exceeds max", {
  e <- mtcars |> e_charts(mpg) |> e_scatter(wt)
  result <- e_axis_(e, axis = "x", index = 10, min = 0)
  expect_equal(result$x$opts$xAxis[[1]]$min, 0)
})

test_that("e_axis_ passes additional arguments", {
  e <- mtcars |> e_charts(mpg) |> e_scatter(wt)
  result <- e_axis_(e, axis = "x", name = "Miles per Gallon", nameLocation = "middle")
  expect_equal(result$x$opts$xAxis[[1]]$name, "Miles per Gallon")
  expect_equal(result$x$opts$xAxis[[1]]$nameLocation, "middle")
})

# e_x_axis_ ---------------------------------------------------------------
test_that("axis funs work", {
  e <- mtcars |> e_charts(mpg) |> e_scatter(wt)

  x_ <- e_x_axis_(e, min = 5)
  y_ <- e_y_axis_(e, min = 5)
  z_ <- e_z_axis_(e, min = 5)

  x <- e_x_axis(e, min = 5)
  y <- e_y_axis(e, min = 5)
  z <- e_z_axis(e, min = 5)

  expect_equal(x_$x$opts$xAxis[[1]]$min, 5)
  expect_equal(y_$x$opts$yAxis[[1]]$min, 5)
  expect_equal(z_$x$opts$zAxis$min, 5)

  expect_equal(x$x$opts$xAxis[[1]]$min, 5)
  expect_equal(y$x$opts$yAxis[[1]]$min, 5)
  expect_equal(z$x$opts$zAxis$min, 5)
})

# e_rm_axis ---------------------------------------------------------------
test_that("remove axis work", {
  e <- mtcars |> e_charts(mpg) |> e_scatter(wt)

  remove <- e_rm_axis(e, axis = "x")

  expect_null(remove$x$opts$xAxis)
  expect_length(remove$x$opts$yAxis, 1)
})

# e_axis_formatter --------------------------------------------------------
test_that("e_axis_formatter returns JS function with correct locale", {
  withr::local_envvar(LANG = "en_US.UTF-8")
  result <- e_axis_formatter("decimal", digits = 2)
  expect_s3_class(result, "JS_EVAL")
  expect_match(as.character(result), "en")
})

test_that("e_axis_formatter handles different styles", {
  decimal_fmt <- e_axis_formatter("decimal", digits = 1)
  percent_fmt <- e_axis_formatter("percent", digits = 0)
  currency_fmt <- e_axis_formatter("currency", digits = 2, currency = "EUR")

  expect_s3_class(decimal_fmt, "JS_EVAL")
  expect_s3_class(percent_fmt, "JS_EVAL")
  expect_s3_class(currency_fmt, "JS_EVAL")

  expect_match(as.character(currency_fmt), "EUR")
})

test_that("e_axis_formatter uses provided locale", {
  result <- e_axis_formatter("decimal", digits = 0, locale = "fr")
  expect_match(as.character(result), "fr")
})

test_that("e_axis_formatter sets correct digit options", {
  result <- e_axis_formatter("decimal", digits = 3)
  expect_match(as.character(result), "minimumFractionDigits")
  expect_match(as.character(result), "maximumFractionDigits")
})

test_that("e_axis_formatter warns in RStudio", {
  testthat::local_mocked_bindings(isAvailable = function() TRUE, .package = "rstudioapi")
  expect_warning(e_axis_formatter("decimal"), "breaks the plot in RStudio")
})

test_that("e_axis_formatter does not warn outside RStudio", {
  testthat::local_mocked_bindings(isAvailable = function() FALSE, .package = "rstudioapi")
  expect_no_warning(e_axis_formatter("decimal"))
})

# e_radius_axis -----------------------------------------------------------
test_that("
e_radius_axis_ sets axis attributes based on serie", {
  plot <- iris[1:4,] |> e_charts_("Species") |>
    e_polar() |>
    e_angle_axis_() |>
    e_radius_axis_("Species") |>
    e_line_("Sepal.Length", coord_system = "polar")

  expect_s3_class(plot, "echarts4r")
  expect_s3_class(plot, "htmlwidget")

  expect_equal(
    unlist(plot$x$opts$series[[1]]$data),
    iris[1:4,][["Sepal.Length"]]
  )
  expect_equal(
    plot$x$opts$series[[1]]$type,
    "line"
  )
})

# e_radar_opts ------------------------------------------------------------




test_that("e_radar_opts errors informatively without attributes", {
  e <- iris |> e_charts(Species) |> e_radar(Sepal.Length)
  expect_error(e_radar_opts(e), "no attribute")
})

test_that("e_radar_opts sets radar options", {
  df <- data.frame(
    x = LETTERS[1:5],
    y = runif(5, 1, 5),
    z = runif(5, 3, 7)
  )
  e <- df |> e_charts(x) |> e_radar(y, max = 7) |> e_radar(z)
  result <- e_radar_opts(e, center = c("50%", "50%"))

  expect_equal(result$x$opts$radar[[1]]$center, c("50%", "50%"))
})

test_that("e_radar_opts works with index", {
  df <- data.frame(
    x = LETTERS[1:5],
    y = runif(5, 1, 5),
    z = runif(5, 3, 7)
  )
  e <- df |> e_charts(x) |> e_radar(y, max = 7) |> e_radar(z)
  result <- e_radar_opts(e, index = 0, radius = "70%")

  expect_equal(result$x$opts$radar[[1]]$radius, "70%")
})

test_that("e_radar_opts initializes radar when r.index exceeds max", {
  df <- data.frame(
    x = LETTERS[1:5],
    y = runif(5, 1, 5)
  )
  e <- df |> e_charts(x) |> e_radar(y)
  result <- e_radar_opts(e, index = 1, center = c("25%", "25%"))

  expect_equal(result$x$opts$radar[[1]]$center, c("25%", "25%"))
})
#  e_radar does not support timeline
# test_that("e_radar_opts works with timeline", {
#   df <- data.frame(
#     x = LETTERS[1:5],
#     y = runif(5, 1, 5),
#     grp = rep(c("A", "B"), length.out = 5)
#   )
#   grouped_data <- df |> dplyr::group_by(grp)
#   e <- grouped_data |> e_charts(x, timeline = TRUE) |> e_radar(y)
#   result <- e_radar_opts(e, center = c("30%", "30%"))
#
#   expect_equal(result$x$opts$baseOption$radar[[1]]$center, c("30%", "30%"))
# })

test_that("e_radar_opts passes multiple arguments", {
  df <- data.frame(
    x = LETTERS[1:5],
    y = runif(5, 1, 5)
  )
  e <- df |> e_charts(x) |> e_radar(y)
  result <- e_radar_opts(e, center = c("50%", "50%"), radius = "60%", shape = "circle")

  expect_equal(result$x$opts$radar[[1]]$center, c("50%", "50%"))
  expect_equal(result$x$opts$radar[[1]]$radius, "60%")
  expect_equal(result$x$opts$radar[[1]]$shape, "circle")
})

# e_single_axis -----------------------------------------------------------

test_that("e_single_axis removes x and y axes", {
  e <- mtcars |> e_charts(mpg) |> e_scatter(wt, coord_system = "singleAxis")
  result <- e_single_axis(e)
  expect_null(result$x$opts$xAxis)
  expect_null(result$x$opts$yAxis)
})

test_that("e_single_axis sets singleAxis options", {
  df <- data.frame(
    axis = LETTERS[1:10],
    value = runif(10, 3, 20),
    size = runif(10, 3, 20)
  )
  e <- df |> e_charts(axis)
  result <- e_single_axis(e, type = "category")
  expect_equal(result$x$opts$singleAxis$type, "category")
})

test_that("e_single_axis appends options when singleAxis already exists", {
  df <- data.frame(
    axis = LETTERS[1:10],
    value = runif(10, 3, 20)
  )
  e <- df |> e_charts(axis) |> e_single_axis() |> e_scatter(value, coord_system = "singleAxis")
  result <- e_single_axis(e, left = 50)
  expect_length(result$x$opts$singleAxis, 3)
})

test_that("e_single_axis sets type based on x mapping for category", {
  df <- data.frame(x = LETTERS[1:5], y = 1:5)
  e <- df |> e_charts(x)
  result <- e_single_axis(e) |> e_scatter(x, coord_system = "singleAxis")
  expect_equal(result$x$opts$singleAxis$type, "category")
  expect_equal(result$x$opts$singleAxis$data, LETTERS[1:5])
})

test_that("e_single_axis sets type based on x mapping for time", {
  df <- data.frame(x = as.POSIXct(c("2020-01-01", "2020-01-02")), y = 1:2)
  e <- df |> e_charts(x)  |> e_scatter(x, coord_system = "singleAxis")
  result <- e_single_axis(e)
  expect_equal(result$x$opts$singleAxis$type, "time")
  expect_length(result$x$opts$singleAxis$data, 2)
})

test_that("e_single_axis warns when x not passed to e_charts", {
  e <- e_charts()
  expect_warning(e_single_axis(e), "x not pass to e_charts")
})

test_that("e_single_axis works with timeline", {
  df <- data.frame(x = LETTERS[1:5], y = 1:5, grp = rep(c("A", "B"), length.out = 5))
  grouped_data <- df |> dplyr::group_by(grp)
  e <- grouped_data |> e_charts(x, timeline = TRUE)
  result <- e_single_axis(e) |> e_scatter(x, coord_system = "singleAxis")

  expect_true(result$x$tl)

  expect_equal(
    result$x$opts$baseOption$timeline$data,
    list("A", "B")
  )

  expect_equal(result$x$opts$baseOption$singleAxis$type, "category")
})

test_that("e_single_axis passes additional arguments", {
  df <- data.frame(axis = LETTERS[1:10], value = runif(10, 3, 20))
  e <- df |> e_charts(axis)
  result <- e_single_axis(e, left = 100, right = 100)
  expect_equal(result$x$opts$singleAxis$left, 100)
  expect_equal(result$x$opts$singleAxis$right, 100)
})

# e_axis_labels -----------------------------------------------------------
test_that("e_axis_labels works", {
  df <- data.frame(axis = LETTERS[1:10], value = runif(10, 3, 20))
  e <- df |> e_charts(axis)
  result <- e_axis_labels(e, x = "x_name", y = "y_name")
  expect_equal(result$x$opts$xAxis[[1]]$name, "x_name")
  expect_equal(result$x$opts$yAxis[[1]]$name, "y_name")
})

# e_hide_grid_lines -------------------------------------------------------
test_that("e_hide_grid_lines hides x axis grid lines when which includes x", {
  e <- cars |> e_charts(speed) |> e_scatter(dist)
  result <- e_hide_grid_lines(e, which = "x")
  expect_false(result$x$opts$xAxis[[1]]$splitLine$show)
})

test_that("e_hide_grid_lines hides y axis grid lines when which includes y", {
  e <- cars |> e_charts(speed) |> e_scatter(dist)
  result <- e_hide_grid_lines(e, which = "y")
  expect_false(result$x$opts$yAxis[[1]]$splitLine$show)
})

test_that("e_hide_grid_lines hides both axes when which includes both", {
  e <- cars |> e_charts(speed) |> e_scatter(dist)
  result <- e_hide_grid_lines(e, which = c("x", "y"))
  expect_false(result$x$opts$xAxis[[1]]$splitLine$show)
  expect_false(result$x$opts$yAxis[[1]]$splitLine$show)
})

test_that("e_hide_grid_lines works with timeline", {
  grouped_data <- mtcars |> dplyr::group_by(cyl)
  e <- grouped_data |> e_charts(mpg, timeline = TRUE) |> e_scatter(wt)
  result <- e_hide_grid_lines(e, which = c("x", "y"))
  expect_false(result$x$opts$baseOption$xAxis[[1]]$splitLine$show)
  expect_false(result$x$opts$baseOption$yAxis[[1]]$splitLine$show)
})

test_that("e_hide_grid_lines returns e", {
  e <- cars |> e_charts(speed) |> e_scatter(dist)
  result <- e_hide_grid_lines(e)
  expect_s3_class(result, "echarts4r")
})
# e_axis_stagger ----------------------------------------------------------
test_that("e_axis_stagger sets axis label formatter", {
  e <- cars |> e_charts(speed) |> e_scatter(dist)
  result <- e_axis_stagger(e)
  expect_s3_class(result$x$opts$xAxis[[1]]$axisLabel$formatter, "JS_EVAL")
})

test_that("e_axis_stagger works with timeline", {
  grouped_data <- mtcars |> dplyr::group_by(cyl)
  e <- grouped_data |> e_charts(mpg, timeline = TRUE) |> e_scatter(wt)
  result <- e_axis_stagger(e)
  expect_s3_class(result$x$opts$baseOption$xAxis[[1]]$axisLabel$formatter, "JS_EVAL")
})

test_that("e_axis_stagger returns e", {
  e <- cars |> e_charts(speed) |> e_scatter(dist)
  result <- e_axis_stagger(e)
  expect_s3_class(result, "echarts4r")
})
