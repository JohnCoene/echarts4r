test_that("echarts4rBox returns list with options and bgColor", {
  df <- data.frame(x = 1:5, y = 6:10)

  result <- echarts4rBox(df, x, y, type = "bar")

  expect_type(result, "list")
  expect_true("options" %in% names(result))
  expect_true("bgColor" %in% names(result))
})


test_that("echarts4rBox converts area type to line with areaStyle", {
  df <- data.frame(x = 1:5, y = 6:10)

  result <- echarts4rBox(df, x, y, type = "area")

  expect_equal(result$options$series[[1]]$type, "line")
  expect_type(result$options$series[[1]]$areaStyle, "list")
})

test_that("echarts4rBox converts step type to line with step property", {
  df <- data.frame(x = 1:5, y = 6:10)

  result <- echarts4rBox(df, x, y, type = "step", step = "middle")

  expect_equal(result$options$series[[1]]$type, "line")
  expect_equal(result$options$series[[1]]$step, "middle")
})

test_that("echarts4rBox sets default colors", {
  df <- data.frame(x = 1:5, y = 6:10)

  result <- echarts4rBox(df, x, y, type = "bar")

  expect_equal(result$options$series[[1]]$color, "#ffffff")
  expect_equal(result$options$title$textStyle$color, "#ffffff")
  expect_equal(result$bgColor, "#293c55")
})

test_that("echarts4rBox accepts custom colors", {
  df <- data.frame(x = 1:5, y = 6:10)

  result <- echarts4rBox(df, x, y, color = "#ff0000", text_color = "#00ff00", background_color = "#0000ff", type = "bar")

  expect_equal(result$options$series[[1]]$color, "#ff0000")
  expect_equal(result$options$title$textStyle$color, "#00ff00")
  expect_equal(result$bgColor, "#0000ff")
})

test_that("echarts4rBox sets title and subtext", {
  df <- data.frame(x = 1:5, y = 6:10)

  result <- echarts4rBox(df, x, y, text = "Main Title", subtext = "Subtitle", type = "bar")

  expect_equal(result$options$title$text, "Main Title")
  expect_equal(result$options$title$subtext, "Subtitle")
})

test_that("echarts4rBox hides axes by default", {
  df <- data.frame(x = 1:5, y = 6:10)

  result <- echarts4rBox(df, x, y, type = "bar")

  expect_false(result$options$xAxis$show)
  expect_false(result$options$yAxis$show)
})

test_that("echarts4rBox sets grid with zero margins", {
  df <- data.frame(x = 1:5, y = 6:10)

  result <- echarts4rBox(df, x, y, type = "bar")

  expect_equal(result$options$grid$left, 0)
  expect_equal(result$options$grid$top, 0)
  expect_equal(result$options$grid$bottom, 0)
  expect_equal(result$options$grid$right, 0)
})

test_that("echarts4rBox accepts additional title arguments", {
  df <- data.frame(x = 1:5, y = 6:10)

  result <- echarts4rBox(df, x, y, title_args = list(left = "center", top = 10), type = "bar")

  expect_equal(result$options$title$left, "center")
  expect_equal(result$options$title$top, 10)
})

test_that("echarts4rBox accepts custom tooltip", {
  df <- data.frame(x = 1:5, y = 6:10)

  result <- echarts4rBox(df, x, y, tooltip = list(trigger = "item"), type = "bar")

  expect_equal(result$options$tooltip$trigger, "item")
})

test_that("echarts4rBox accepts additional serie arguments", {
  df <- data.frame(x = 1:5, y = 6:10)

  result <- echarts4rBox(df, x, y, smooth = TRUE, type = "bar")

  expect_true(result$options$series[[1]]$smooth)
})
