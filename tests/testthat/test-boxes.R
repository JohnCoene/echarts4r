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

# renderEcharts4rBox ------------------------------------------------------
test_that("renderEcharts4rBox returns a function", {
  df <- data.frame(x = 1:5, y = 6:10)

  result <- renderEcharts4rBox({
    echarts4rBox(df, x, y, type = "bar")
  })

  expect_type(result, "closure")
})

test_that("renderEcharts4rBox evaluates expression correctly", {
  df <- data.frame(x = 1:5, y = 6:10)

  render_func <- renderEcharts4rBox({
    echarts4rBox(df, x, y, type = "bar")
  })

  result <- render_func()

  expect_type(result, "list")
  expect_true("options" %in% names(result))
  expect_true("bgColor" %in% names(result))
  expect_true("deps" %in% names(result))
})

test_that("renderEcharts4rBox adds dependencies", {
  df <- data.frame(x = 1:5, y = 6:10)

  render_func <- renderEcharts4rBox({
    echarts4rBox(df, x, y, type = "bar")
  })

  result <- render_func()

  expect_type(result$deps, "list")
  expect_length(result$deps, 1)
})

test_that("renderEcharts4rBox works with quoted expression", {
  df <- data.frame(x = 1:5, y = 6:10)

  expr <- quote(echarts4rBox(df, x, y, type = "bar"))
  render_func <- renderEcharts4rBox(expr, quoted = TRUE)

  result <- render_func()

  expect_type(result, "list")
  expect_true("options" %in% names(result))
})

test_that("renderEcharts4rBox preserves box options and bgColor", {
  df <- data.frame(x = 1:5, y = 6:10)

  render_func <- renderEcharts4rBox({
    echarts4rBox(df, x, y, text = "Title", background_color = "#ff0000", type = "bar")
  })

  result <- render_func()

  expect_equal(result$options$title$text, "Title")
  expect_equal(result$bgColor, "#ff0000")
})

# echarts4rBoxOutput ------------------------------------------------------

test_that("echarts4rBoxOutput errors informatively without id", {
  expect_error(echarts4rBoxOutput(), "Missing id")
})

test_that("echarts4rBoxOutput creates div with correct id", {
  result <- echarts4rBoxOutput("test_box")

  expect_s3_class(result, "shiny.tag")
  expect_equal(result$attribs$id, "echarts4r-box-container-test_box")
  expect_equal(result$attribs$class, "panel")
})

test_that("echarts4rBoxOutput converts numeric height to px", {
  result <- echarts4rBoxOutput("test_box", height = 200)

  inner_div <- result$children[[1]]
  expect_match(inner_div$attribs$style, "height:200px")
})

test_that("echarts4rBoxOutput accepts string height", {
  result <- echarts4rBoxOutput("test_box", height = "50%")

  inner_div <- result$children[[1]]
  expect_match(inner_div$attribs$style, "height:50%")
})

test_that("echarts4rBoxOutput uses default height of 150", {
  result <- echarts4rBoxOutput("test_box")

  inner_div <- result$children[[1]]
  expect_match(inner_div$attribs$style, "height:150px")
})

test_that("echarts4rBoxOutput inner div has correct id and class", {
  result <- echarts4rBoxOutput("test_box")

  inner_div <- result$children[[1]]
  expect_equal(inner_div$attribs$id, "test_box")
  expect_equal(inner_div$attribs$class, "echarts4rBox")
})

test_that("echarts4rBoxOutput has dependencies attached", {
  result <- echarts4rBoxOutput("test_box")

  deps <- htmltools::htmlDependencies(result)
  expect_length(deps, 1)
  expect_equal(deps[[1]]$name, "echarts4rBox")
  expect_equal(deps[[1]]$version, "1.0.0")
})

test_that("echarts4rBoxOutput width is always 100%", {
  result <- echarts4rBoxOutput("test_box", height = 200)

  inner_div <- result$children[[1]]
  expect_match(inner_div$attribs$style, "width:100%")
})
