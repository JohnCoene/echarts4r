
# get_base_nodes ----------------------------------------------------------

test_that("returns empty list for atomic values", {
  expect_equal(get_base_nodes(1), list())
  expect_equal(get_base_nodes("a"), list())
  expect_equal(get_base_nodes(TRUE), list())
})

test_that("returns empty list for data frames", {
  df <- data.frame(x = 1:3, y = 4:6)
  expect_equal(get_base_nodes(df), list())
})

test_that("returns unnamed atomic elements from list", {
  x <- list(1, 2, 3)
  expect_equal(get_base_nodes(x), list(1, 2, 3))
})

test_that("returns unnamed data frames from list", {
  df <- data.frame(x = 1:2)
  x <- list(df, df)
  expect_equal(get_base_nodes(x), list(df, df))
})

test_that("skips named atomic elements", {
  x <- list(a = 1, b = 2)
  expect_equal(get_base_nodes(x), list())
})

test_that("recursively finds unnamed atomic elements in nested lists", {
  x <- list(list(1, 2), list(3, 4))
  expect_equal(get_base_nodes(x), list(1, 2, 3, 4))
})

test_that("handles mixed named and unnamed elements", {
  x <- list(a = 1, 2, list(3, b = 4))
  expect_equal(get_base_nodes(x), list(2, 3))
})

test_that("returns unnamed elements at all nesting levels", {
  x <- list(list(list(1)), list(2))
  expect_equal(get_base_nodes(x), list(1, 2))
})

test_that("handles empty nested lists", {
  x <- list(list(), list(1))
  expect_equal(get_base_nodes(x), list(1))
})

test_that("handles NULL values in list", {
  x <- list(NULL, 1, list(NULL, 2))
  expect_equal(get_base_nodes(x), list(NULL, 1, NULL, 2))
})


# e_matrix ----------------------------------------------------------------

test_that("e_matrix fails informatively without e", {
  expect_error(e_matrix(), "must pass e")
})

test_that("e_matrix fails informatively without xAxis or yAxis", {
  df <- data.frame(Class = c("A", "B"), Grade = c("1", "2"))
  e <- df |> e_charts()

  expect_snapshot(e_matrix(e, xAxis = "Class"), error = TRUE)
  expect_snapshot(e_matrix(e, yAxis = "Grade"), error = TRUE)
})

test_that("e_matrix creates matrix structure with unique values", {
  df <- data.frame(
    Class = rep(c("Class1", "Class2"), 4),
    Grade = rep(c("Grade1", "Grade2"), each = 2)
  )

  plot <- df |> e_charts() |> e_matrix(xAxis = "Class", yAxis = "Grade")

  expect_s3_class(plot, "echarts4r")
  expect_equal(plot$x$opts$matrix$x$name, "Class")
  expect_equal(plot$x$opts$matrix$y$name, "Grade")
  expect_equal(plot$x$opts$matrix$x$data, list("Class1", "Class2"))
  expect_equal(plot$x$opts$matrix$y$data, list("Grade1", "Grade2"))
})

test_that("e_matrix appends additional arguments", {
  df <- data.frame(Class = c("A"), Grade = c("1"))

  plot <- df |> e_charts() |>
    e_matrix(xAxis = "Class", yAxis = "Grade", bottom = 20)

  expect_equal(plot$x$opts$matrix$bottom, 20)
})

# e_matrix_raw ----------------------------------------------------------------

test_that("e_matrix_raw fails informatively without rows or cols", {
  expect_snapshot(e_matrix_raw(rows = 3), error = TRUE)
  expect_snapshot(e_matrix_raw(cols = 3), error = TRUE)
  expect_snapshot(e_matrix_raw(), error = TRUE)
})

test_that("e_matrix_raw creates matrix structure with specified dimensions", {
  plot <- e_matrix_raw(rows = 3, cols = 4)

  expect_s3_class(plot, "echarts4r")
  expect_null(plot$x$opts$yAxis)
  expect_length(plot$x$opts$matrix$x$data, 4)
  expect_length(plot$x$opts$matrix$y$data, 3)
  expect_false(plot$x$opts$matrix$x$show)
  expect_false(plot$x$opts$matrix$y$show)
})

test_that("e_matrix_raw appends additional arguments", {
  plot <- e_matrix_raw(rows = 2, cols = 2, customArg = "test")

  expect_equal(plot$x$opts$matrix$customArg, "test")
})

test_that("e_matrix_raw creates proper NA structure", {
  plot <- e_matrix_raw(rows = 2, cols = 3)

  expect_true(all(sapply(plot$x$opts$matrix$x$data, is.na)))
  expect_true(all(sapply(plot$x$opts$matrix$y$data, is.na)))
})

# e_matrix_parent ---------------------------------------------------------
test_that("e_matrix_parent fails informatively without e", {
  expect_error(e_matrix_parent(),
               "must pass e")
})

test_that("e_matrix_parent creates parent node on x axis with direct children", {
  df <- data.frame(
    Class = rep(c("Class1", "Class2", "Class3"), each = 3),
    Grade = c("Grade1", "Grade2", "Grade3")
  )

  plot <- df |>
    e_charts() |>
    e_matrix(xAxis = "Class", yAxis = "Grade") |>
    e_matrix_parent(axis = "x", value = "Primary", children = c("Class1", "Class2"))

  expect_s3_class(plot, "echarts4r")
  expect_length(plot$x$opts$matrix$x$data, 2)
  expect_equal(plot$x$opts$matrix$x$data[[2]]$value, "Primary")
  expect_length(plot$x$opts$matrix$x$data[[2]]$children, 2)
})

test_that("e_matrix_parent creates parent node on y axis with direct children", {
  df <- data.frame(
    Class = rep(c("Class1", "Class2", "Class3"), each = 3),
    Grade = c("Grade1", "Grade2", "Grade3")
  )

  plot <- df |>
    e_charts() |>
    e_matrix(xAxis = "Class", yAxis = "Grade") |>
    e_matrix_parent(axis = "y", value = "High", children = c("Grade1", "Grade2"))

  expect_s3_class(plot, "echarts4r")
  expect_length(plot$x$opts$matrix$y$data, 2)
  expect_equal(plot$x$opts$matrix$y$data[[2]]$value, "High")
  expect_length(plot$x$opts$matrix$y$data[[2]]$children, 2)
})

test_that("e_matrix_parent fails informatively when children not found", {
  df <- data.frame(
    Class = rep(c("Class1", "Class2"), each = 2),
    Grade = c("Grade1", "Grade2")
  )

  plot <- df |>
    e_charts() |>
    e_matrix(xAxis = "Class", yAxis = "Grade")

  expect_error(e_matrix_parent(plot, axis = "x", value = "Test", children = "NonExistent"), "No children found in the data")

  expect_error(e_matrix_parent(plot, axis = "y", value = "Test", children = "NonExistent"), "No children found in the data")
})

test_that("e_matrix_parent appends additional arguments", {
  df <- data.frame(
    Class = rep(c("Class1", "Class2"), each = 2),
    Grade = c("Grade1", "Grade2")
  )

  plot <- df |>
    e_charts() |>
    e_matrix(xAxis = "Class", yAxis = "Grade") |>
    e_matrix_parent(axis = "x", value = "Primary", children = "Class1", customArg = "test")

  expect_equal(plot$x$opts$matrix$x$data[[2]]$customArg, "test")
})

# TODO check this - maybe need value
# test_that("e_matrix_parent finds nested children", {
#   df <- data.frame(
#     Class = rep(c("Class1", "Class2"), each = 2),
#     Grade = c("Grade1", "Grade2")
#   )
#
#   plot <- df |>
#     e_charts() |>
#     e_matrix(xAxis = "Class", yAxis = "Grade") |>
#     e_matrix_parent(axis = "x", value = "Primary", children = "Class1")
#   plot |>
#     e_matrix_parent(axis = "x", value = "AllClasses", children = "Class1")
#
#   expect_s3_class(plot, "echarts4r")
#   expect_equal(plot$x$opts$matrix$x$data[[2]]$value, "AllClasses")
# })

# e_matrix_corner ---------------------------------------------------------
test_that("e_matrix_corner fails informatively without e", {
  expect_error(e_matrix_corner(), "must pass e")
})

test_that("e_matrix_corner adds corner data with default coordinates", {
  df <- data.frame(
    Class = rep(c("Class1", "Class2"), each = 2),
    Grade = c("Grade1", "Grade2")
  )

  plot <- df |>
    e_charts() |>
    e_matrix(xAxis = "Class", yAxis = "Grade") |>
    e_matrix_corner(value = "All School")

  expect_s3_class(plot, "echarts4r")
  expect_equal(plot$x$opts$matrix$corner$data[[1]]$coord, c(-1, -1))
  expect_equal(plot$x$opts$matrix$corner$data[[1]]$value, "All School")
  expect_true(plot$x$opts$matrix$corner$data[[1]]$mergeCells)
  expect_false(plot$x$opts$matrix$corner$data[[1]]$coordClamp)
})

test_that("e_matrix_corner adds corner data with custom coordinates", {
  df <- data.frame(
    Class = rep(c("Class1", "Class2"), each = 2),
    Grade = c("Grade1", "Grade2")
  )

  plot <- df |>
    e_charts() |>
    e_matrix(xAxis = "Class", yAxis = "Grade") |>
    e_matrix_corner(coord = c(0, 0), value = "Corner")

  expect_equal(plot$x$opts$matrix$corner$data[[1]]$coord, c(0, 0))
  expect_equal(plot$x$opts$matrix$corner$data[[1]]$value, "Corner")
})

test_that("e_matrix_corner sets mergeCells and coordClamp parameters", {
  df <- data.frame(
    Class = rep(c("Class1", "Class2"), each = 2),
    Grade = c("Grade1", "Grade2")
  )

  plot <- df |>
    e_charts() |>
    e_matrix(xAxis = "Class", yAxis = "Grade") |>
    e_matrix_corner(value = "Test", mergeCells = FALSE, coordClamp = TRUE)

  expect_false(plot$x$opts$matrix$corner$data[[1]]$mergeCells)
  expect_true(plot$x$opts$matrix$corner$data[[1]]$coordClamp)
})

test_that("e_matrix_corner appends additional arguments", {
  df <- data.frame(
    Class = rep(c("Class1", "Class2"), each = 2),
    Grade = c("Grade1", "Grade2")
  )

  plot <- df |>
    e_charts() |>
    e_matrix(xAxis = "Class", yAxis = "Grade") |>
    e_matrix_corner(value = "Test", label = list(fontSize = 24, color = "#555"))

  expect_equal(plot$x$opts$matrix$corner$label$fontSize, 24)
  expect_equal(plot$x$opts$matrix$corner$label$color, "#555")
})

test_that("e_matrix_corner can add multiple corner cells", {
  df <- data.frame(
    Class = rep(c("Class1", "Class2"), each = 2),
    Grade = c("Grade1", "Grade2")
  )

  plot <- df |>
    e_charts() |>
    e_matrix(xAxis = "Class", yAxis = "Grade") |>
    e_matrix_corner(coord = c(-1, -1), value = "Corner1") |>
    e_matrix_corner(coord = c(0, 0), value = "Corner2")

  expect_length(plot$x$opts$matrix$corner, 2)
  expect_equal(plot$x$opts$matrix$corner[[1]][[1]]$value, "Corner1")
  expect_equal(plot$x$opts$matrix$corner[[2]][[1]]$value, "Corner2")
})


# e_format_matrix_axis ----------------------------------------------------
test_that("e_format_matrix_axis fails informatively without e", {
  expect_error(e_format_matrix_axis(), "must pass e")
})

test_that("e_format_matrix_axis fails informatively without axis", {
  df <- data.frame(Class = c("A"), Grade = c("1"))
  plot <- df |> e_charts() |> e_matrix(xAxis = "Class", yAxis = "Grade")

  expect_error(e_format_matrix_axis(plot), "must specify which axis")
})

test_that("e_format_matrix_axis can format both axes independently", {
  df <- data.frame(Class = c("A"), Grade = c("1"))

  plot <- df |> e_charts() |>
    e_matrix(xAxis = "Class", yAxis = "Grade") |>
    e_format_matrix_axis(axis = "x", label = list(color = "red")) |>
    e_format_matrix_axis(axis = "y", label = list(color = "blue", fontSize = 24))
  expect_s3_class(plot, "echarts4r")
  expect_equal(plot$x$opts$matrix$x$label$color, "red")
  expect_equal(plot$x$opts$matrix$y$label$color, "blue")
  expect_equal(plot$x$opts$matrix$y$label$fontSize, 24)
})

test_that("e_format_matrix_axis appends multiple formatting options", {
  df <- data.frame(Class = c("A"), Grade = c("1"))

  plot <- df |> e_charts() |>
    e_matrix(xAxis = "Class", yAxis = "Grade") |>
    e_format_matrix_axis(axis = "x", label = list(color = "red"), splitLine = list(show = FALSE))

  expect_equal(plot$x$opts$matrix$x$label$color, "red")
  expect_false(plot$x$opts$matrix$x$splitLine$show)
})
# e_matrix_addChart -------------------------------------------------------
test_that("e_matrix_addChart errors informatively without e", {
  expect_error(e_matrix_addChart(), "must pass e")
})

test_that("e_matrix_addChart errors informatively without chart", {
  e <- e_matrix_raw(rows = 3, cols = 3)
  expect_error(e_matrix_addChart(e), "must provide an echart to add into matrix grid")
})

test_that("e_matrix_addChart errors informatively with non-echart", {
  e <- e_matrix_raw(rows = 3, cols = 3)
  expect_error(e_matrix_addChart(e, chart = list()), "the provided chart must be an echart")
})

test_that("e_matrix_addChart errors informatively without coord", {
  e <- e_matrix_raw(rows = 3, cols = 3)
  chart <- iris |> e_charts(Sepal.Length) |> e_line(Sepal.Width)
  expect_error(e_matrix_addChart(e, chart = chart), "must provide coordinates for chart locations in matrix grid")
})

test_that("e_matrix_addChart errors informatively with timeline charts", {
  e <- e_matrix_raw(rows = 3, cols = 3)
  chart <- iris |> dplyr::group_by(Species) |> e_charts(Sepal.Length, timeline = TRUE) |> e_line(Sepal.Width)
  expect_error(e_matrix_addChart(e, chart = chart, coord = list(0, 0)), "e_matrix_addChart does not currently support timeline charts")
})

test_that("e_matrix_addChart errors informatively with duplicate id", {
  e <- e_matrix_raw(rows = 3, cols = 3)
  chart <- iris |> e_charts(Sepal.Length) |> e_line(Sepal.Width)
  e <- e_matrix_addChart(e, chart = chart, coord = list(0, 0), id = "test")
  expect_error(e_matrix_addChart(e, chart = chart, coord = list(1, 0), id = "test"), "Chart id has already been used. Please supply a different id value")
})

test_that("e_matrix_addChart adds chart with default parameters", {
  e <- e_matrix_raw(rows = 3, cols = 3)
  chart <- iris |> e_charts(Sepal.Length) |> e_line(Sepal.Width)
  result <- e_matrix_addChart(e, chart = chart, coord = list(0, 0))

  expect_s3_class(result, "echarts4r")
  expect_length(result$x$opts$xAxis, 1)
  expect_length(result$x$opts$yAxis, 1)
  expect_length(result$x$opts$series, 1)
  expect_length(result$x$opts$grid, 1)
  expect_equal(result$x$opts$grid[[1]]$id, "chart1")
  expect_equal(result$x$opts$grid[[1]]$coord, list(0, 0))
})

test_that("e_matrix_addChart adds chart with custom id", {
  e <- e_matrix_raw(rows = 3, cols = 3)
  chart <- iris |> e_charts(Sepal.Length) |> e_line(Sepal.Width)
  result <- e_matrix_addChart(e, chart = chart, coord = list(1, 1), id = "custom_id")

  expect_equal(result$x$opts$grid[[1]]$id, "custom_id")
  expect_equal(result$x$opts$xAxis[[1]]$gridId, "custom_id")
  expect_equal(result$x$opts$yAxis[[1]]$gridId, "custom_id")
  expect_equal(result$x$opts$xAxis[[1]]$id, "custom_id")
  expect_equal(result$x$opts$yAxis[[1]]$id, "custom_id")
})

test_that("e_matrix_addChart adds chart with custom margins", {
  e <- e_matrix_raw(rows = 3, cols = 3)
  chart <- iris |> e_charts(Sepal.Length) |> e_line(Sepal.Width)
  result <- e_matrix_addChart(e, chart = chart, coord = list(0, 0), margin_trbl = c(t = "10%", r = "10%", b = "20%", l = "15%"))

  expect_equal(result$x$opts$grid[[1]]$top, "10%")
  expect_equal(result$x$opts$grid[[1]]$right, "10%")
  expect_equal(result$x$opts$grid[[1]]$bottom, "20%")
  expect_equal(result$x$opts$grid[[1]]$left, "15%")
})

test_that("e_matrix_addChart fills in missing margins with defaults", {
  e <- e_matrix_raw(rows = 3, cols = 3)
  chart <- iris |> e_charts(Sepal.Length) |> e_line(Sepal.Width)
  result <- e_matrix_addChart(e, chart = chart, coord = list(0, 0), margin_trbl = c(t = "10%", b = "20%"))

  expect_equal(result$x$opts$grid[[1]]$top, "10%")
  expect_equal(result$x$opts$grid[[1]]$bottom, "20%")
  expect_equal(result$x$opts$grid[[1]]$right, "5")
  expect_equal(result$x$opts$grid[[1]]$left, "5")
})

test_that("e_matrix_addChart adds legend when legend is TRUE", {
  e <- e_matrix_raw(rows = 3, cols = 3)
  chart <- iris |> e_charts(Sepal.Length) |> e_line(Sepal.Width)
  result <- e_matrix_addChart(e, chart = chart, coord = list(0, 0), legend = TRUE)

  expect_length(result$x$opts$legend, 1)
  expect_equal(result$x$opts$legend[[1]]$coordinateSystem, "matrix")
  expect_equal(result$x$opts$legend[[1]]$coord, list(0, 0))
})

test_that("e_matrix_addChart does not add legend when legend is FALSE", {
  e <- e_matrix_raw(rows = 3, cols = 3)
  chart <- iris |> e_charts(Sepal.Length) |> e_line(Sepal.Width)
  result <- e_matrix_addChart(e, chart = chart, coord = list(0, 0), legend = FALSE)

  expect_null(result$x$opts$legend)
})

test_that("e_matrix_addChart positions legend correctly", {
  e <- e_matrix_raw(rows = 3, cols = 3)
  chart <- iris |> e_charts(Sepal.Length) |> e_line(Sepal.Width)

  result_top <- e_matrix_addChart(e, chart = chart, coord = list(0, 0), legend = TRUE, legend_pos = "top", legend_space = "0%")
  expect_equal(result_top$x$opts$legend[[1]]$top, "0%")

  result_right <- e_matrix_addChart(e, chart = chart, coord = list(0, 0), legend = TRUE, legend_pos = "right", legend_space = "0%")
  expect_equal(result_right$x$opts$legend[[1]]$right, "0%")

  result_left <- e_matrix_addChart(e, chart = chart, coord = list(0, 0), legend = TRUE, legend_pos = "left", legend_space = "0%")
  expect_equal(result_left$x$opts$legend[[1]]$left, "0%")

  result_bottom <- e_matrix_addChart(e, chart = chart, coord = list(0, 0), legend = TRUE, legend_pos = "bottom", legend_space = "0%")
  expect_equal(result_bottom$x$opts$legend[[1]]$bottom, "0%")
})
#
test_that("e_matrix_addChart adds multiple charts", {
  e <- e_matrix_raw(rows = 3, cols = 3)
  chart1 <- iris |> e_charts(Sepal.Length) |> e_line(Sepal.Width)
  chart2 <- iris |> e_charts(Petal.Length) |> e_scatter(Petal.Width)

  result <- e |>
    e_matrix_addChart(chart = chart1, coord = list(0, 0), id = "chart1") |>
    e_matrix_addChart(chart = chart2, coord = list(1, 1), id = "chart2")

  expect_length(result$x$opts$xAxis, 2)
  expect_length(result$x$opts$yAxis, 2)
  expect_length(result$x$opts$series, 2)
  expect_length(result$x$opts$grid, 2)
})

# e_title_matrix ----------------------------------------------------------

test_that("e_title_matrix errors informatively without e", {
  expect_error(e_title_matrix(), "must pass e")
})

test_that("e_title_matrix creates titles for each series", {
  e <- e_matrix_raw(rows = 3, cols = 3)
  chart1 <- iris |> e_charts(Sepal.Length) |> e_line(Sepal.Width)
  chart2 <- iris |> e_charts(Petal.Length) |> e_scatter(Petal.Width)

  result <- e |>
    e_matrix_addChart(chart = chart1, coord = list(0, 0), id = "chart1") |>
    e_matrix_addChart(chart = chart2, coord = list(1, 1), id = "chart2") |>
    e_title_matrix()

  expect_s3_class(result, "echarts4r")
  expect_length(result$x$opts$title, 2)
  expect_equal(result$x$opts$title[[1]]$text, "Sepal.Width")
  expect_equal(result$x$opts$title[[2]]$text, "Petal.Width")
  expect_equal(result$x$opts$title[[1]]$coordinateSystem, "matrix")
  expect_equal(result$x$opts$title[[2]]$coordinateSystem, "matrix")
  expect_equal(result$x$opts$title[[1]]$coord, list(0, 0))
  expect_equal(result$x$opts$title[[2]]$coord, list(1, 1))
})

test_that("e_title_matrix passes additional arguments", {
  e <- e_matrix_raw(rows = 3, cols = 3)
  chart <- iris |> e_charts(Sepal.Length) |> e_line(Sepal.Width)

  result <- e |>
    e_matrix_addChart(chart = chart, coord = list(0, 0), id = "chart1") |>
    e_title_matrix(textStyle = list(fontSize = 10), left = "center", top = "top")

  expect_equal(result$x$opts$title[[1]]$textStyle$fontSize, 10)
  expect_equal(result$x$opts$title[[1]]$left, "center")
  expect_equal(result$x$opts$title[[1]]$top, "top")
})

test_that("e_title_matrix works with empty series", {
  e <- e_matrix_raw(rows = 3, cols = 3)
  result <- e_title_matrix(e)

  expect_s3_class(result, "echarts4r")
  expect_null(result$x$opts$title)
})

# e_geoFacet --------------------------------------------------------------

test_that("e_geoFacet errors informatively without e", {
  expect_error(e_geoFacet(), "must pass e")
})

test_that("e_geoFacet errors informatively with invalid grid type", {
  e <- iris |> dplyr::group_by(Species) |> e_charts(Sepal.Length) |> e_line(Sepal.Width)
  expect_snapshot(e_geoFacet(e, grid = 123), error = TRUE)
})

test_that("e_geoFacet uses max row and col when rows/cols missing", {
  grid <- data.frame(
    name = c("setosa", "versicolor", "virginica"),
    row = c(1, 2, 3),
    col = c(1, 2, 3)
  )
  e <- iris |> dplyr::group_by(Species) |> e_charts(Sepal.Length) |> e_line(Sepal.Width)

  testthat::local_mocked_bindings(e_matrix_raw = function(rows, cols, ...) {
    list(rows = 3, cols = 3)
  })

  result <- e_geoFacet(e, grid = grid, margin_trbl = c(t = "10%", r = "10%", b = "10%", l = "10%"))
  expect_equal(result$rows, 3)
  expect_equal(result$cols, 3)

  expect_length(result$x$opts$grid, 3)
  expect_equal(result$x$opts$grid[[1]]$left, "10%")
  expect_equal(result$x$opts$grid[[1]]$top, "10%")
  expect_equal(result$x$opts$grid[[1]]$right, "10%")
  expect_equal(result$x$opts$grid[[1]]$bottom, "10%")
})

test_that("e_geoFacet warns when series data not found in grid", {
  grid <- data.frame(
    name = c("A", "B"),
    row = c(1, 2),
    col = c(1, 2)
  )
  e <- iris |> dplyr::group_by(Species) |> e_charts(Sepal.Length) |> e_line(Sepal.Width)

  testthat::local_mocked_bindings(
    e_matrix_raw = function(...) {
      e_chart()
    },
    e_matrix_addChart = function(e, ...) {
      e
    }
  )

  expect_warning(expect_warning(e_geoFacet(e, grid = grid), "A: data not found in series"), "B: data not found in series")
})

test_that("e_geoFacet creates matrix with custom rows and cols", {
  grid <- data.frame(
    name = c("setosa", "versicolor"),
    row = c(1, 2),
    col = c(1, 2)
  )
  e <- iris |> dplyr::group_by(Species) |> e_charts(Sepal.Length) |> e_line(Sepal.Width)

  testthat::local_mocked_bindings(
    e_matrix_raw = function(rows, cols, ...) {
      list(rows = rows, cols = cols)
    },
    e_matrix_addChart = function(e, ...) {
      e
    }
  )

  result <- e_geoFacet(e, rows = 5, cols = 5, grid = grid)
  expect_equal(result$rows, 5)
  expect_equal(result$cols, 5)
})

# test_that("e_geoFacet fetches geofacet grid by name", {
#   e <- iris |> dplyr::group_by(Species) |> e_charts(Sepal.Length) |> e_line(Sepal.Width)
#
#   mock_grid <- data.frame(
#     name = c("setosa", "versicolor", "virginica"),
#     row = c(1, 2, 3),
#     col = c(1, 2, 3)
#   )
#
#   testthat::local_mocked_bindings(
#     get_grid = function(x) {
#       mock_grid
#     },
#     .package = "geofacet",
#     e_matrix_raw = function(...) {
#       e_chart()
#     },
#     e_matrix_addChart = function(e, ...) {
#       e
#     }
#   )
#   result <- e_geoFacet(e, grid = "us_state_grid1")
#   expect_s3_class(result, "echarts4r")
# })

# e_pie_matrix ------------------------------------------------------------

test_that("e_pie_matrix errors informatively without e", {
  expect_error(e_pie_matrix(), "must pass e")
})

test_that("e_pie_matrix errors informatively without matrix coordinate system", {
  df <- data.frame(
    Class = rep(c("Class1", "Class2"), each = 2),
    Grade = c("Grade1", "Grade2"),
    A = sample(1:10, 4),
    B = sample(1:10, 4)
  )

  e <- df |> e_charts()
  expect_snapshot(e_pie_matrix(e, x = "A", y = "B"), error = TRUE)
})

test_that("e_pie_matrix creates pie series for each matrix cell", {
  df <- data.frame(
    Class = rep(c("Class1", "Class2"), each = 2),
    Grade = rep(c("Grade1", "Grade2"), 2),
    A = sample(1:10, 4),
    B = sample(1:10, 4)
  )

  plot <- df |>
    e_charts() |>
    e_matrix(xAxis = "Class", yAxis = "Grade") |>
    e_pie_matrix(x = "A", y = "B")

  expect_s3_class(plot, "echarts4r")
  expect_length(plot$x$opts$series, 4)
  expect_equal(plot$x$opts$series[[1]]$type, "pie")
  expect_equal(plot$x$opts$series[[1]]$coordinateSystem, "matrix")
  expect_equal(plot$x$opts$series[[1]]$center, c("Class1", "Grade1"))
})

test_that("e_pie_matrix removes x and y axes", {
  df <- data.frame(
    Class = rep(c("Class1", "Class2"), each = 2),
    Grade = rep(c("Grade1", "Grade2"), 2),
    A = sample(1:10, 4),
    B = sample(1:10, 4)
  )

  plot <- df |>
    e_charts() |>
    e_matrix(xAxis = "Class", yAxis = "Grade") |>
    e_pie_matrix(x = "A", y = "B")

  expect_null(plot$x$opts$xAxis)
  expect_null(plot$x$opts$yAxis)
})

test_that("e_pie_matrix appends additional arguments", {
  df <- data.frame(
    Class = c("Class1"),
    Grade = c("Grade1"),
    A = 5,
    B = 10
  )

  plot <- df |>
    e_charts() |>
    e_matrix(xAxis = "Class", yAxis = "Grade") |>
    e_pie_matrix(x = "A", y = "B", radius = "50%")

  expect_equal(plot$x$opts$series[[1]]$radius, "50%")
})

# test_that("e_pie_matrix handles timeline charts", {
#   df <- data.frame(
#     Class = rep(c("Class1", "Class2"), each = 2),
#     Grade = rep(c("Grade1", "Grade2"), 2),
#     A = sample(1:10, 4),
#     B = sample(1:10, 4),
#     grp = rep(c("A", "B"), each = 2)
#   )
#
#   testthat::local_mocked_bindings(.rm_axis = function(e, rm, axis) e)
#   testthat::local_mocked_bindings(get_base_nodes = function(x) list("Class1", "Class2"))
#
#   plot <- df |>
#     dplyr::group_by(grp) |>
#     e_charts(timeline = TRUE) |>
#     e_matrix(xAxis = "Class", yAxis = "Grade") |>
#     e_pie_matrix(x = "A", y = "B")
#
#   expect_s3_class(plot, "echarts4r")
# })

# e_scatter_matrix --------------------------------------------------------
test_that("e_scatter_matrix errors informatively without e", {
  expect_error(e_scatter_matrix(), "must pass e")
})

test_that("e_scatter_matrix errors informatively without matrix coordinate system", {
  df <- data.frame(
    Class = rep(c("Class1", "Class2"), each = 2),
    Grade = c("Grade1", "Grade2"),
    A = sample(1:10, 4)
  )

  e <- df |> e_charts()
  expect_snapshot(e_scatter_matrix(e, z = "A"), error = TRUE)
})

test_that("e_scatter_matrix creates scatter series for each matrix cell", {
  df <- data.frame(
    Class = rep(c("Class1", "Class2"), each = 2),
    Grade = rep(c("Grade1", "Grade2"), 2),
    A = sample(1:10, 4)
  )

  plot <- df |>
    e_charts() |>
    e_matrix(xAxis = "Class", yAxis = "Grade") |>
    e_scatter_matrix("A")  |>
    e_visual_map(A, inRange = list(symbolSize = c(1,50)))

  expect_s3_class(plot, "echarts4r")
  expect_length(plot$x$opts$series, 1)
  expect_equal(plot$x$opts$series[[1]]$type, "scatter")
  expect_equal(plot$x$opts$series[[1]]$coordinateSystem, "matrix")
  expect_length(plot$x$opts$series[[1]]$data, 4)
})

test_that("e_scatter_matrix removes x and y axes", {
  df <- data.frame(
    Class = rep(c("Class1", "Class2"), each = 2),
    Grade = rep(c("Grade1", "Grade2"), 2),
    A = sample(1:10, 4)
  )

  plot <- df |>
    e_charts() |>
    e_matrix(xAxis = "Class", yAxis = "Grade") |>
    e_scatter_matrix("A")

  expect_null(plot$x$opts$xAxis)
  expect_null(plot$x$opts$yAxis)
})

test_that("e_scatter_matrix appends additional arguments", {
  df <- data.frame(
    Class = c("Class1"),
    Grade = c("Grade1"),
    A = 5
  )

  plot <- df |>
    e_charts() |>
    e_matrix(xAxis = "Class", yAxis = "Grade") |>
    e_scatter_matrix("A", symbolSize = 10)

  expect_equal(plot$x$opts$series[[1]]$symbolSize, 10)
})

test_that("e_scatter_matrix creates correct data structure", {
  df <- data.frame(
    Class = rep(c("Class1", "Class2"), each = 2),
    Grade = rep(c("Grade1", "Grade2"), 2),
    A = c(1, 2, 3, 4)
  )

  plot <- df |>
    e_charts() |>
    e_matrix(xAxis = "Class", yAxis = "Grade") |>
    e_scatter_matrix("A")

  expect_equal(plot$x$opts$series[[1]]$data[[1]], list("Class1", "Grade1", 1))
  expect_equal(plot$x$opts$series[[1]]$data[[2]], list("Class1", "Grade2", 2))
  expect_equal(plot$x$opts$series[[1]]$data[[3]], list("Class2", "Grade1", 3))
  expect_equal(plot$x$opts$series[[1]]$data[[4]], list("Class2", "Grade2", 4))
})

# e_heatmap_matrix --------------------------------------------------------

test_that("e_heatmap_matrix errors informatively without e", {
  expect_error(e_heatmap_matrix(), "must pass e")
})

test_that("e_heatmap_matrix errors informatively without matrix coordinate system", {
  df <- data.frame(
    Class = rep(c("Class1", "Class2"), each = 2),
    Grade = c("Grade1", "Grade2"),
    A = sample(1:10, 4)
  )

  e <- df |> e_charts()
  expect_snapshot(e_heatmap_matrix(e, z = "A"), error = TRUE)
})

test_that("e_heatmap_matrix creates heatmap series for each matrix cell", {
  df <- data.frame(
    Class = rep(c("Class1", "Class2"), each = 2),
    Grade = rep(c("Grade1", "Grade2"), 2),
    A = sample(1:10, 4)
  )

  plot <- df |>
    e_charts() |>
    e_matrix(xAxis = "Class", yAxis = "Grade") |>
    e_heatmap_matrix("A")

  expect_s3_class(plot, "echarts4r")
  expect_length(plot$x$opts$series, 1)
  expect_equal(plot$x$opts$series[[1]]$type, "heatmap")
  expect_equal(plot$x$opts$series[[1]]$coordinateSystem, "matrix")
  expect_length(plot$x$opts$series[[1]]$data, 4)
})

test_that("e_heatmap_matrix removes x and y axes", {
  df <- data.frame(
    Class = rep(c("Class1", "Class2"), each = 2),
    Grade = rep(c("Grade1", "Grade2"), 2),
    A = sample(1:10, 4)
  )

  plot <- df |>
    e_charts() |>
    e_matrix(xAxis = "Class", yAxis = "Grade") |>
    e_heatmap_matrix("A")

  expect_null(plot$x$opts$xAxis)
  expect_null(plot$x$opts$yAxis)
})

test_that("e_heatmap_matrix appends additional arguments", {
  df <- data.frame(
    Class = c("Class1"),
    Grade = c("Grade1"),
    A = 5
  )

  plot <- df |>
    e_charts() |>
    e_matrix(xAxis = "Class", yAxis = "Grade") |>
    e_heatmap_matrix("A", emphasis = list(itemStyle = list(shadowBlur = 10)))

  expect_equal(plot$x$opts$series[[1]]$emphasis$itemStyle$shadowBlur, 10)
})

test_that("e_heatmap_matrix creates correct data structure", {
  df <- data.frame(
    Class = rep(c("Class1", "Class2"), each = 2),
    Grade = rep(c("Grade1", "Grade2"), 2),
    A = c(1, 2, 3, 4)
  )

  plot <- df |>
    e_charts() |>
    e_matrix(xAxis = "Class", yAxis = "Grade") |>
    e_heatmap_matrix("A")

  expect_equal(plot$x$opts$series[[1]]$data[[1]], list("Class1", "Grade1", 1))
  expect_equal(plot$x$opts$series[[1]]$data[[2]], list("Class1", "Grade2", 2))
  expect_equal(plot$x$opts$series[[1]]$data[[3]], list("Class2", "Grade1", 3))
  expect_equal(plot$x$opts$series[[1]]$data[[4]], list("Class2", "Grade2", 4))
})
