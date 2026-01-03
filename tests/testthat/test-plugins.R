
# cloud -------------------------------------------------------------------
test_that("e_cloud plot has the good data structure and type", {
  tf <- data.frame(
    terms = LETTERS[1:26],
    freq = rnorm(26, 55, 10)
  ) |>
    dplyr::arrange(-freq)

  plot <- tf |>
    # e_color_range(freq, color) |>
    e_charts() |>
    e_cloud(terms, freq, shape = "circle", sizeRange = c(3, 15))


  expect_s3_class(plot, "echarts4r")
  expect_s3_class(plot, "htmlwidget")

  tf_expected_point <- tf[1,] |> unlist()
  data_point <- plot$x$opts$series[[1]]$data[[1]] |> unlist()

  tf_expected_point %in% data_point

  expect_true(all(tf_expected_point %in% data_point))
  expect_equal(
    plot$x$opts$series[[1]]$type,
    "wordCloud"
  )
})

test_that("e_cloud and e_cloud_ expects error when missing e ", {
  expect_error(e_cloud(), "must pass e")
  expect_error(e_cloud_(), "must pass e")
})


test_that("e_cloud plot has the good data structure and type", {
  words <- function(n = 5000) {
    set.seed(1)
    a <- do.call(paste0, replicate(5, sample(LETTERS, n, TRUE), FALSE))
    paste0(a, sprintf("%04d", sample(9999, n, TRUE)), sample(LETTERS, n, TRUE))
  }

  tf <- data.frame(terms = words(5))

  set.seed(1)
  tf$freq <- round(rnorm(5, 55, 10), 5)
  tf <- tf |>
    dplyr::arrange(-freq)

  plot <- tf |>
    e_color_range(freq, color) |>
    e_charts() |>
    e_cloud(terms, freq, color, shape = "circle", sizeRange = c(3, 15)) |>
    e_title("Wordcloud", "Random strings")

  expect_s3_class(plot, "echarts4r")
  expect_s3_class(plot, "htmlwidget")

  expect_equal(
    plot$x$opts$series[[1]]$data,
    list(
      list(value = 70.95281, name = "ARJIY6526F", textStyle = list(color = "#F6EFA6")),
      list(value = 58.29508, name = "BSVON5071J", textStyle = list(color = "#D78071")),
      list(value = 56.83643, name = "DKUJE7845T", textStyle = list(color = "#D4796C")),
      list(value = 48.73546, name = "YWANU8677A", textStyle = list(color = "#C45052")),
      list(value = 46.64371, name = "GNUGI5922C", textStyle = list(color = "#BF444C"))
    )
  )
  expect_equal(
    plot$x$opts$series[[1]]$type,
    "wordCloud"
  )
})

# liquid ------------------------------------------------------------------
test_that("e_liquid plot has the good data structure and type", {

  df <- data.frame(val = c(0.6, 0.5, 0.4))

  plot <- df |>
    e_charts() |>
    e_liquid(val)

  expect_s3_class(plot, "echarts4r")
  expect_s3_class(plot, "htmlwidget")

  expect_equal(
    plot$x$opts$series[[1]]$data, (df |> as.vector())$val)

  expect_equal(
    plot$x$opts$series[[1]]$type,
    "liquidFill"
  )
})

test_that("e_liquid plot has color", {

  df <- data.frame(val = c(0.6, 0.5, 0.1), col = c("#FFF", "#FFF", "#FFF"))

  plot <- df |>
    e_charts() |>
    e_liquid(val, color = col)

  expect_equal(
    plot$x$opts$series[[1]]$color, df$col)
})

test_that("e_liquid and e_liquid_ expects error when missing e ", {
  expect_error(e_liquid(), "must pass e")

  expect_error(e_liquid_(), "must pass e")
})

# modularity --------------------------------------------------------------
test_that("e_modularity has good data structure", {
  nodes <- data.frame(
    name = paste0(LETTERS, 1:100),
    value = rnorm(100, 10, 2),
    stringsAsFactors = FALSE
  )

  edges <- data.frame(
    source = sample(nodes$name, 200, replace = TRUE),
    target = sample(nodes$name, 200, replace = TRUE),
    stringsAsFactors = FALSE
  )

  plot <- e_charts() |>
    e_graph() |>
    e_modularity(
      list(
        resolution = 5,
        sort = TRUE
      )
    )
  expect_s3_class(plot, "echarts4r")
  expect_s3_class(plot, "htmlwidget")

  # Inputs are as expected
  expect_equal(
    plot$x$opts$series[[1]]$modularity$modularity$resolution, 5)
  expect_true(
    plot$x$opts$series[[1]]$modularity$modularity$sort)
})

test_that("e_cloud and e_cloud_ expects error when missing e ", {
  expect_error(e_modularity(), "must pass e")
})

# doughnut ----------------------------------------------------------------
test_that("e_doughnut has good data structure", {

  plot <- e_charts() |>
    e_doughnut(numerator = 3, denominator = 6, fontSize = "11px")

  expect_s3_class(plot, "echarts4r")
  expect_s3_class(plot, "htmlwidget")

  # THe numerator
  expect_equal(
    plot$x$opts$series[[1]]$data[[1]], 3)

  # The denominator
  expect_equal(
    plot$x$opts$series[[1]]$itemPayload$segmentCount, 6)

  # Font size
  expect_equal(plot$x$opts$series[[1]]$itemPayload$label$fontSize, "11px")

  expect_snapshot(plot$x$opts)
})

test_that("e_doughnut expects error when missing e", {
  expect_error(e_doughnut(), "must pass e")
})


# violin ------------------------------------------------------------------

test_that("e_violin works", {

  plot <- iris |> e_chart(Species) |>
    e_scatter(Sepal.Length) |>
    e_violin(name = "Iris")

  expect_s3_class(plot, "echarts4r")
  expect_s3_class(plot, "htmlwidget")

  # First data point
  expect_equal(
    plot$x$opts$series[[1]]$data[[1]]$value,
    c("setosa", "5.1")
  )
  expect_equal(
    plot$x$opts$series[[2]]$type,
    "custom"
  )

  # Legend name is correct
  expect_equal(plot$x$opts$legend$data[[2]], "Iris")
})

test_that("e_violin x and y index works", {

  plot <- iris |> e_chart(Species) |>
    e_scatter(Sepal.Length) |>
    e_violin(
      name = "Iris",

               y_index = 10,
               x_index = 10
    )
  expect_gt(length(plot$x$opts$xAxis), 10)
  expect_gt(length(plot$x$opts$yAxis), 10)
})

test_that("e_violin expects error when missing e", {

  expect_error( iris |> dplyr::group_by("Species") |>
                  e_chart(Species, timeline = TRUE) |>
                  e_scatter_("Sepal.Length") |>
                  e_violin(
                    name = "Iris"
                  ) |>
                  e_timeline_serie(
                    title = list(
                    )
                  ), "timeline not supported"
  )

  expect_error( iris |> dplyr::group_by("Species") |>
                  e_chart(Species) |>
                  e_violin(
                    name = "Iris"
                  ), "violin is only supported with scatter plots"
  )
  expect_error(e_violin(), "must pass e")
})

# barRange ----------------------------------------------------------------
test_that("e_barRange works", {
  df <- iris |>
    dplyr::group_by(Species) |>
    dplyr::summarise(min_length = min(Sepal.Length),
                     max_length = max(Sepal.Length))

  plot <- df |> e_chart(Species) |>
    e_barRange(lower=min_length,
               upper=max_length,
               name = "bar range",
               textSymbol = 'NEWSYMBOL'
    )

  expect_s3_class(plot, "echarts4r")
  expect_s3_class(plot, "htmlwidget")

  # text symbol has been applied.
  expect_equal(plot$x$opts$series[[1]]$itemPayload$textSymbol, "NEWSYMBOL")

  expect_equal(
    plot$x$opts$series[[1]]$data[[1]]$value,
    c("setosa", "4.3", "5.8")
  )
  expect_equal(
    plot$x$opts$series[[1]]$type,
    "custom"
  )

  # Legend name is correct
  expect_equal(plot$x$opts$legend$data[[1]], "bar range")
})

test_that("e_barRange x and y index works", {
  df <- iris |>
    dplyr::group_by(Species) |>
    dplyr::summarise(min_length = min(Sepal.Length),
                     max_length = max(Sepal.Length))

  plot <- df |> e_chart(Species) |>
    e_barRange(lower=min_length,
               upper=max_length,
               y_index = 10,
               x_index = 10
    )

  expect_gt(length(plot$x$opts$xAxis), 10)
  expect_gt(length(plot$x$opts$yAxis), 10)
})

test_that("e_barRange timeline works", {
  df <- iris |>
    dplyr::group_by(Species) |>
    dplyr::summarise(min_length = min(Sepal.Length),
                     max_length = max(Sepal.Length))

  plot <- df |> dplyr::group_by(Species) |> e_chart(Species, timeline = TRUE) |>
    e_barRange(lower=min_length,
               upper=max_length
    )
  expect_true(plot$x$tl)

  # Time series
  expect_equal(
    plot$x$opts$baseOption$timeline$data,
    list("setosa", "versicolor", "virginica")
  )

  expect_equal(plot$x$opts$baseOption$series[[1]]$type, "custom")
})

test_that("e_barRange expects error when missing e", {
  expect_error(e_barRange(), "must pass e")
})

# contour -----------------------------------------------------------------
test_that("e_contour works", {

  plot <- mtcars |>
    e_charts(mpg) |>
    e_contour(serie = mpg, name = "TEST")

  expect_s3_class(plot, "echarts4r")
  expect_s3_class(plot, "htmlwidget")

  # Since mpg is for x and y all of those values in the plot should be in the data
  data_points <- plot$x$opts$series[[1]]$data |> unlist() |> unique()
  expected_data_points <- mtcars[["mpg"]]

  expect_true(
   all(expected_data_points %in% data_points)
  )

  expect_equal(
    plot$x$opts$series[[1]]$type,
    "custom"
  )

  # Legend name is correct
  expect_equal(plot$x$opts$legend$data[[1]], "TEST")
})

test_that("e_contour timeline works", {
  plot <- mtcars |>
    dplyr::group_by(vs) |>
    e_charts(mpg, timeline = TRUE) |>
    e_contour(serie = mpg) |>
    e_timeline_serie(
      title = list(
        list(text = "a"),
        list(text = "b")
      )
    )

  expect_true(plot$x$tl)

  # Time series
  expect_equal(
    plot$x$opts$baseOption$timeline$data,
    list("0", "1")
  )

  expect_equal(plot$x$opts$baseOption$series[[1]]$type, "custom")
})

test_that("e_contour expects error when missing e", {
  expect_error(e_contour(), "must pass e")
})


# lineRange ---------------------------------------------------------------

test_that("e_lineRange works", {
  df <- iris |>
    dplyr::group_by(Species) |>
    dplyr::summarise(min_length = min(Sepal.Length),
                     max_length = max(Sepal.Length))

  plot <- df |> e_chart(Species) |>
    e_lineRange(lower=min_length,
               upper=max_length,
               name = "line range"
    )

  expect_s3_class(plot, "echarts4r")
  expect_s3_class(plot, "htmlwidget")


  expect_equal(
    plot$x$opts$series[[1]]$data[[1]]$value,
    c("setosa", "4.3", "5.8")
  )
  expect_equal(
    plot$x$opts$series[[1]]$type,
    "custom"
  )

  # Legend name is correct
  expect_equal(plot$x$opts$legend$data[[1]]$name, "line range")
})

test_that("e_lineRange x and y index works", {
  df <- iris |>
    dplyr::group_by(Species) |>
    dplyr::summarise(min_length = min(Sepal.Length),
                     max_length = max(Sepal.Length))

  plot <- df |> e_chart(Species) |>
    e_line(min_length) |>

    e_lineRange(lower=min_length,
               upper=max_length,
               areaStyle = list(color = "red"),
               y_index = 10,
               x_index = 10
    )
  expect_gt(length(plot$x$opts$xAxis), 10)
  expect_gt(length(plot$x$opts$yAxis), 10)
})

test_that("e_lineRange expects error when missing e", {
  df <- iris |>
    dplyr::group_by(Species) |>
    dplyr::summarise(min_length = min(Sepal.Length),
                     max_length = max(Sepal.Length))

  expect_error(e_lineRange(), "must pass e")
  expect_error(df |> dplyr::group_by(Species) |> e_chart(Species, timeline = TRUE) |>
                 e_lineRange(lower=min_length,
                             upper=max_length
                 ), "timeline not supported"
               )
  expect_error(df |> dplyr::group_by(Species) |> e_chart(Species, timeline = TRUE) |> e_lineRange(), "must pass lower, or upper")
})

# stage -------------------------------------------------------------------

test_that("e_stage works", {

  df <- data.frame(
    start = as.POSIXct(c(
      "2024-09-07 06:12", "2024-09-07 06:15", "2024-09-07 05:45",
      "2024-09-07 04:57", "2024-09-07 06:12", "2024-09-07 06:18"
    )),

    end = as.POSIXct(c(
      "2024-09-07 06:12", "2024-09-07 06:18", "2024-09-07 06:12",
      "2024-09-07 05:45", "2024-09-07 06:15", "2024-09-07 07:37"
    )),

    stage = c(
      "Awake", "Awake",  "REM",
      "Core", "Core", "Deep"
    ),
    stringsAsFactors = FALSE
  )

  plot <- df |>
    e_charts() |>
    e_stage(start = start,
            end = end,
            stage = stage,
            name = "TEST") |>
    e_x_axis(type = 'time') |>
    e_y_axis(type = 'category')

  expect_s3_class(plot, "echarts4r")
  expect_s3_class(plot, "htmlwidget")


  expect_equal(
    plot$x$opts$dataset[[1]][[1]],
    list(c(df$start[1]), df$end[1], df$stage[1]))

  expect_equal(
    plot$x$opts$series[[1]]$type,
    "custom"
  )

  # Legend name is correct
  expect_equal(plot$x$opts$legend$data[[1]], "TEST")
})

test_that("e_stage timeline works", {
  df <- data.frame(
    start = as.POSIXct(c(
      "2024-09-07 06:12", "2024-09-07 06:15", "2024-09-07 05:45",
      "2024-09-07 04:57", "2024-09-07 06:12", "2024-09-07 06:18"
    )),

    end = as.POSIXct(c(
      "2024-09-07 06:12", "2024-09-07 06:18", "2024-09-07 06:12",
      "2024-09-07 05:45", "2024-09-07 06:15", "2024-09-07 07:37"
    )),

    stage = c(
      "Awake", "Awake",  "REM",
      "Core", "Core", "Deep"
    ),
    stringsAsFactors = FALSE
  )
  plot <-  df |> dplyr::group_by(stage) |>
    e_charts(timeline = TRUE) |>
    e_stage(start = start,
            end = end,
            stage = stage,
            name = "TEST") |>
    e_x_axis(type = 'time') |>
    e_y_axis(type = 'category') |>
    e_timeline_serie(
      title = list(
        list(text = "a"),
        list(text = "b"),
        list(text = "c"),
        list(text = "d")
      )
    )
  expect_true(plot$x$tl)

  # Time series
  expect_equal(
    plot$x$opts$baseOption$timeline$data,
    list("Awake", "Core", "Deep", "REM")
  )

  expect_equal(plot$x$opts$baseOption$series[[1]]$type, "custom")
})

test_that("e_stage expects error when missing e", {
  expect_error(e_stage(), "must pass e")
})

