# cloud -------------------------------------------------------------------
test_that("e_cloud_ plot has the good data structure and type", {
  tf <- data.frame(
    terms = LETTERS[1:26],
    freq = rnorm(26, 55, 10)
  ) |>
    dplyr::arrange(-freq)

  plot <- tf |>
    # e_color_range(freq, color) |>
    e_charts() |>
    e_cloud_("terms", "freq", shape = "circle", sizeRange = c(3, 15))

  expect_s3_class(plot, "echarts4r")
  expect_s3_class(plot, "htmlwidget")

  tf_expected_point <- tf[1, ] |> unlist()
  data_point <- plot$x$opts$series[[1]]$data[[1]] |> unlist()

  tf_expected_point %in% data_point

  expect_true(all(tf_expected_point %in% data_point))
  expect_equal(
    plot$x$opts$series[[1]]$type,
    "wordCloud"
  )
})

test_that("e_cloud and e_cloud_ expects error when missing e ", {
  expect_error(e_cloud_(), "must pass e")
})


test_that("e_cloud_ plot has the good data structure and type", {
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
    e_cloud_(
      "terms",
      "freq",
      "color",
      shape = "circle",
      sizeRange = c(3, 15)
    ) |>
    e_title("Wordcloud", "Random strings")

  expect_s3_class(plot, "echarts4r")
  expect_s3_class(plot, "htmlwidget")

  expect_equal(
    plot$x$opts$series[[1]]$data,
    list(
      list(
        value = 70.95281,
        name = "ARJIY6526F",
        textStyle = list(color = "#F6EFA6")
      ),
      list(
        value = 58.29508,
        name = "BSVON5071J",
        textStyle = list(color = "#D78071")
      ),
      list(
        value = 56.83643,
        name = "DKUJE7845T",
        textStyle = list(color = "#D4796C")
      ),
      list(
        value = 48.73546,
        name = "YWANU8677A",
        textStyle = list(color = "#C45052")
      ),
      list(
        value = 46.64371,
        name = "GNUGI5922C",
        textStyle = list(color = "#BF444C")
      )
    )
  )
  expect_equal(
    plot$x$opts$series[[1]]$type,
    "wordCloud"
  )
})

# liquid ------------------------------------------------------------------
test_that("e_liquid_ plot has the good data structure and type", {
  df <- data.frame(val = c(0.6, 0.5, 0.4))

  plot <- df |>
    e_charts() |>
    e_liquid_("val")

  expect_s3_class(plot, "echarts4r")
  expect_s3_class(plot, "htmlwidget")

  expect_equal(
    plot$x$opts$series[[1]]$data,
    (df |> as.vector())$val
  )

  expect_equal(
    plot$x$opts$series[[1]]$type,
    "liquidFill"
  )
})

test_that("e_liquid_ plot has color", {
  df <- data.frame(val = c(0.6, 0.5, 0.1), col = c("#FFF", "#FFF", "#FFF"))

  plot <- df |>
    e_charts() |>
    e_liquid_("val", color = "col")

  expect_equal(
    plot$x$opts$series[[1]]$color,
    df$col
  )
})

test_that("e_liquid_ expects error when missing e ", {
  expect_error(e_liquid_(), "must pass e")
})

# barRange ----------------------------------------------------------------
test_that("e_barRange_ works", {
  df <- iris |>
    dplyr::group_by(Species) |>
    dplyr::summarise(
      min_length = min(Sepal.Length),
      max_length = max(Sepal.Length)
    )

  plot <- df |>
    e_chart(Species) |>
    e_barRange_(
      lower = "min_length",
      upper = "max_length",
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
    dplyr::summarise(
      min_length = min(Sepal.Length),
      max_length = max(Sepal.Length)
    )

  plot <- df |>
    e_charts_("Species") |>
    e_barRange_(
      lower = "min_length",
      upper = "max_length",
      y_index = 10,
      x_index = 10
    )

  expect_gt(length(plot$x$opts$xAxis), 10)
  expect_gt(length(plot$x$opts$yAxis), 10)
})

test_that("e_barRange_ timeline works", {
  df <- iris |>
    dplyr::group_by(Species) |>
    dplyr::summarise(
      min_length = min(Sepal.Length),
      max_length = max(Sepal.Length)
    )

  plot <- df |>
    dplyr::group_by(Species) |>
    e_charts(Species, timeline = TRUE) |>
    e_barRange_(lower = "min_length", upper = "max_length")
  expect_true(plot$x$tl)

  # Time series
  expect_equal(
    plot$x$opts$baseOption$timeline$data,
    list("setosa", "versicolor", "virginica")
  )

  expect_equal(plot$x$opts$baseOption$series[[1]]$type, "custom")
})

test_that("e_barRange_ expects error when missing e", {
  expect_error(e_barRange_(), "must pass e")
})

# contour -----------------------------------------------------------------
test_that("e_contour_ works", {
  plot <- mtcars |>
    e_charts(mpg) |>
    e_contour_(serie = "mpg", name = "TEST")

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

test_that("e_contour_ timeline works", {
  plot <- mtcars |>
    dplyr::group_by(vs) |>
    e_charts(mpg, timeline = TRUE) |>
    e_contour_(serie = "mpg") |>
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

test_that("e_contour_ expects error when missing e", {
  expect_error(e_contour_(), "must pass e")
})


# lineRange ---------------------------------------------------------------

test_that("e_lineRange_ works", {
  df <- iris |>
    dplyr::group_by(Species) |>
    dplyr::summarise(
      min_length = min(Sepal.Length),
      max_length = max(Sepal.Length)
    )

  plot <- df |>
    e_chart(Species) |>
    e_lineRange_(
      lower = "min_length",
      upper = "max_length",
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
    dplyr::summarise(
      min_length = min(Sepal.Length),
      max_length = max(Sepal.Length)
    )

  plot <- df |>
    e_chart(Species) |>
    e_line(min_length) |>

    e_lineRange_(
      lower = "min_length",
      upper = "max_length",
      areaStyle = list(color = "red"),
      y_index = 10,
      x_index = 10
    )
  expect_gt(length(plot$x$opts$xAxis), 10)
  expect_gt(length(plot$x$opts$yAxis), 10)
})

test_that("e_lineRange_ expects error when missing e", {
  df <- iris |>
    dplyr::group_by(Species) |>
    dplyr::summarise(
      min_length = min(Sepal.Length),
      max_length = max(Sepal.Length)
    )

  expect_error(e_lineRange_(), "must pass e")
  expect_error(
    df |>
      dplyr::group_by(Species) |>
      e_chart(Species, timeline = TRUE) |>
      e_lineRange_(lower = "min_length", upper = "max_length"),
    "timeline not supported"
  )
  expect_error(
    df |>
      dplyr::group_by(Species) |>
      e_chart(Species, timeline = TRUE) |>
      e_lineRange_(),
    "must pass lower, or upper"
  )
})

# stage -------------------------------------------------------------------

test_that("e_stage_ works", {
  df <- data.frame(
    start = as.POSIXct(c(
      "2024-09-07 06:12",
      "2024-09-07 06:15",
      "2024-09-07 05:45",
      "2024-09-07 04:57",
      "2024-09-07 06:12",
      "2024-09-07 06:18"
    )),

    end = as.POSIXct(c(
      "2024-09-07 06:12",
      "2024-09-07 06:18",
      "2024-09-07 06:12",
      "2024-09-07 05:45",
      "2024-09-07 06:15",
      "2024-09-07 07:37"
    )),

    stage = c(
      "Awake",
      "Awake",
      "REM",
      "Core",
      "Core",
      "Deep"
    ),
    stringsAsFactors = FALSE
  )

  plot <- df |>
    e_charts() |>
    e_stage_(start = "start", end = "end", stage = "stage", name = "TEST") |>
    e_x_axis(type = 'time') |>
    e_y_axis(type = 'category')

  expect_s3_class(plot, "echarts4r")
  expect_s3_class(plot, "htmlwidget")

  expect_equal(
    plot$x$opts$dataset[[1]][[1]],
    list(c(df$start[1]), df$end[1], df$stage[1])
  )

  expect_equal(
    plot$x$opts$series[[1]]$type,
    "custom"
  )

  # Legend name is correct
  expect_equal(plot$x$opts$legend$data[[1]], "TEST")
})

test_that("e_stage_ timeline works", {
  df <- data.frame(
    start = as.POSIXct(c(
      "2024-09-07 06:12",
      "2024-09-07 06:15",
      "2024-09-07 05:45",
      "2024-09-07 04:57",
      "2024-09-07 06:12",
      "2024-09-07 06:18"
    )),

    end = as.POSIXct(c(
      "2024-09-07 06:12",
      "2024-09-07 06:18",
      "2024-09-07 06:12",
      "2024-09-07 05:45",
      "2024-09-07 06:15",
      "2024-09-07 07:37"
    )),

    stage = c(
      "Awake",
      "Awake",
      "REM",
      "Core",
      "Core",
      "Deep"
    ),
    stringsAsFactors = FALSE
  )
  plot <- df |>
    dplyr::group_by(stage) |>
    e_charts(timeline = TRUE) |>
    e_stage_(start = "start", end = "end", stage = "stage", name = "TEST") |>
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

test_that("e_stage_ expects error when missing e", {
  expect_error(e_stage_(), "must pass e")
})
