test_that("errors informatively without e", {
  expect_error(e_visual_map(), "must pass e")
  expect_error(e_visual_map_(), "must pass e")
  expect_error(e_legend(), "must pass e")
  expect_error(e_toolbox_feature(), "must pass e")
  expect_error(e_toolbox(), "must pass e")
  expect_error(e_datazoom(), "must pass e")
  expect_error(e_charts() |> e_datazoom(x_index = 1, y_index = 1),"pass x_index or y_index, not both")
  expect_error(e_brush(), "must pass e")
  expect_error(e_charts() |> e_brush(x_index = 1, y_index = 1),"pass x_index or y_index, not both")
  expect_error(e_title(), "must pass e")
  expect_error(e_polar(), "must pass e")

  expect_error(e_axis_pointer(), "must pass e")
  expect_error(e_animation(), "must pass e")
  expect_error(e_utc(), "must pass e")
  expect_error(e_flip_coords(), "must pass e")
  expect_error(e_text_style(), "must pass e")

  expect_error(e_connect(), "must pass e")
  expect_error(e_charts() |> e_connect(),"missing ids")
  expect_error(e_group(), "must pass e")
  expect_error(e_charts() |> e_group(),"missing group")
  expect_error(e_connect_group(), "must pass e")
  expect_error(e_charts() |> e_connect_group(),"missing group")
  expect_error(e_disconnect_group(), "must pass e")
  expect_error(e_dims(), "must pass e")
  expect_error(e_locale(), "must pass e")
  expect_error(e_charts() |> e_locale(),"missing locale")
  expect_error(e_locale_manual(), "must pass e")
  expect_error(e_charts() |> e_locale_manual(),"missing locale")
  expect_error(e_charts() |> e_locale_manual(locale = "x"),"missing path")
  })

# e_tooltip ---------------------------------------------------------------

test_that("e_tooltip errors informatively without e", {
  expect_error(e_tooltip(), "must pass e")
})

test_that("e_tooltip sets tooltip with default trigger", {
  e <- mtcars |> e_charts(mpg)
  result <- e_tooltip(e)
  expect_equal(result$x$opts$tooltip$trigger, "item")
})

test_that("e_tooltip sets tooltip with axis trigger", {
  e <- mtcars |> e_charts_("mpg")
  result <- e_tooltip(e, trigger = "axis")
  expect_equal(result$x$opts$tooltip$trigger, "axis")
})

test_that("e_tooltip passes additional arguments", {
  e <- mtcars |> e_charts(mpg)
  result <- e_tooltip(e, backgroundColor = "red")
  expect_equal(result$x$opts$tooltip$backgroundColor, "red")
})

test_that("e_tooltip works with timeline", {
  e <- mtcars |> dplyr::group_by(cyl) |> e_charts(mpg, timeline = TRUE)
  result <- e_tooltip(e)
  expect_equal(result$x$opts$baseOption$tooltip$trigger, "item")
  expect_true(e$x$tl)
})

test_that("e_tooltip works without formatter", {
  e <- mtcars |> e_charts(mpg)
  result <- e_tooltip(e)
  expect_null(result$x$opts$tooltip$formatter)
})

test_that("e_tooltip works with formatter", {
  e <- mtcars |> e_charts(mpg) |> e_line(cyl)
  result <- e_tooltip(e, formatter = e_tooltip_item_formatter(style = "percent"))
  expect_equal(result$x$opts$tooltip$trigger, "item")
})


# Tooltip formatter -------------------------------------------------------

test_that("e_tooltip_item_formatter sets formatter for item_formatter", {
  fmt <- e_tooltip_item_formatter(style = "decimal", digits = 2)
  expect_snapshot(fmt)
})

test_that("e_tooltip_choro_formatter sets formatter for item_formatter", {
  fmt <- e_tooltip_choro_formatter(style = "decimal", digits = 2)
  expect_snapshot(fmt)
})

test_that("e_tooltip_pie_formatter sets formatter for item_formatter", {
  fmt <- e_tooltip_pie_formatter(style = "decimal", digits = 2)
  expect_snapshot(fmt)
})

test_that("e_tooltip_pointer_formatter sets formatter for item_formatter", {
  fmt <- e_tooltip_pointer_formatter(style = "decimal", digits = 2)
  expect_snapshot(fmt)
})

# e_legend ----------------------------------------------------------------
test_that("e_legend errors informatively without e", {
  expect_error(e_tooltip(), "must pass e")
})

test_that("e_legend sets legend with default show", {
  e <- mtcars |> e_charts(mpg) |> e_scatter(wt)
  result <- e_legend(e)
  expect_true(result$x$opts$legend$show)
})

test_that("e_legend hides legend when show is FALSE", {
  e <- mtcars |> e_charts(mpg) |> e_scatter(wt)
  result <- e_legend(e, show = FALSE)
  expect_false(result$x$opts$legend$show)
})

test_that("e_legend sets legend type", {
  e <- mtcars |> e_charts(mpg) |> e_scatter(wt) |> e_scatter(mpg)
  result <- e_legend(e, type = "scroll")
  expect_equal(result$x$opts$legend$type, "scroll")
})

test_that("e_legend passes additional arguments", {
  e <- mtcars |> e_charts(mpg) |> e_scatter(wt) |> e_scatter(mpg)
  result <- e_legend(e, orient = "vertical")
  expect_equal(result$x$opts$legend$orient, "vertical")
})

test_that("e_legend works with timeline", {
  e <- mtcars |> dplyr::group_by(cyl) |> e_charts(mpg, timeline = TRUE) |> e_scatter(wt)
  result <- e_legend(e)
  expect_equal(result$x$opts$baseOption$legend$show, TRUE)
})

test_that("e_legend sets single icon", {
  e <- mtcars |> e_charts(mpg) |> e_scatter(wt)
  result <- e_legend(e, icons = list("square"))
  expect_equal(result$x$opts$legend$icon, list("square"))
})

test_that("e_legend errors informatively with too few icons", {
  e <- mtcars |> e_charts_("mpg") |> e_scatter(wt) |> e_scatter(qsec)
  expect_snapshot(e_legend(e, icons = list("circle")), error = TRUE)
})

test_that("e_legend sets multiple icons", {
  e <- mtcars |> e_charts(mpg) |> e_scatter(wt) |> e_scatter(qsec)
  result <- e_legend(e, icons = list("circle", "rect"))
  expect_equal(result$x$opts$legend$data[[1]]$name, "wt")
  expect_equal(result$x$opts$legend$data[[1]]$icon, "circle")
  expect_equal(result$x$opts$legend$data[[2]]$name, "qsec")
  expect_equal(result$x$opts$legend$data[[2]]$icon, "rect")
})

# e_toolbox_feature ---------------------------------------------------------
test_that("e_toolbox_feature errors includes every feature when missing", {
  e <- mtcars |> e_charts_("mpg") |> e_scatter(wt) |> e_toolbox_feature()
  e_feats <- e$x$opts$toolbox$feature |> names()

  expect_all_true(e_feats %in% c("saveAsImage", "restore", "dataView", "dataZoom", "magicType", "brush"))
})

test_that("e_toolbox_feature include options", {
  e <- mtcars |> e_charts_("mpg") |> e_scatter(wt) |> e_toolbox_feature(feature = "dataView", title = 'The Title')
  expect_equal(e$x$opts$toolbox$feature[["dataView"]]$title, "The Title")
})

test_that("e_toolbox_feature with timeline", {
  e <- mtcars |> dplyr::group_by(cyl) |> e_charts_("mpg", timeline = TRUE) |> e_scatter(wt) |> e_toolbox_feature(feature = "dataView", title = 'The Title')
  expect_equal(e$x$opts$baseOption$toolbox$feature[["dataView"]]$title, "The Title")
})

# e_toolbox ---------------------------------------------------------------
test_that("e_toolbox works", {
  e <- mtcars |> e_charts_("mpg") |> e_scatter(wt) |>  e_toolbox(itemSize = 20) |> e_toolbox_feature()

  expect_equal(e$x$opts$toolbox$itemSize, 20)
})

test_that("e_toolbox with timeline", {
  e <- mtcars |> dplyr::group_by(cyl) |> e_charts_("mpg", timeline = TRUE) |> e_scatter(wt) |> e_toolbox(itemSize = 20) |> e_toolbox_feature()
  expect_equal(e$x$opts$baseOption$toolbox$itemSize, 20)
})

# e_datazoom --------------------------------------------------------------
test_that("e_datazoom works", {
  e <- mtcars |> e_charts_("mpg") |> e_scatter(wt) |>  e_datazoom(y_index = c(0, 1))
  expect_equal(e$x$opts$dataZoom[[1]]$yAxisIndex, c(0,1))
})

test_that("e_datazoom works with timeline", {
  e <- mtcars |> dplyr::group_by(cyl) |> e_charts_("mpg", timeline = TRUE) |> e_scatter(wt) |>  e_datazoom(y_index = c(0, 1))
  expect_equal(e$x$opts$baseOption$dataZoom[[1]]$yAxisIndex, c(0,1))
})

# e_brush -----------------------------------------------------------------
test_that("e_brush initializes brush list when not existing", {
  e <- mtcars |> e_charts(mpg) |> e_scatter(wt)
  result <- e_brush(e)
  expect_true(length(result$x$opts$brush) > 0)

  # Adds toolbox
  expect_equal(result$x$opts$toolbox$feature$brush, list())
})

test_that("e_brush sets brush_link", {
  e <- mtcars |> e_charts(mpg) |> e_scatter(wt)
  result <- e_brush(e, brush_link = "none")
  expect_equal(result$x$opts$brush$brushLink, "none")
})

test_that("e_brush sets x_index", {
  e <- mtcars |> e_charts(mpg) |> e_scatter(wt)
  result <- e_brush(e, x_index = 1)
  expect_equal(result$x$opts$brush$xAxisIndex, 1)
})

test_that("e_brush sets y_index", {
  e <- mtcars |> e_charts(mpg) |> e_scatter(wt)
  result <- e_brush(e, y_index = 1)
  expect_equal(result$x$opts$brush$yAxisIndex, 1)
})

test_that("e_brush passes additional arguments", {
  e <- mtcars |> e_charts(mpg) |> e_scatter(wt)
  result <- e_brush(e, throttleDelay = 100)
  expect_equal(result$x$opts$brush$throttleDelay, 100)
})

test_that("e_brush works with timeline", {
  e <- mtcars |> dplyr::group_by(cyl) |> e_charts(mpg, timeline = TRUE) |> e_scatter(wt)
  result <- e_brush(e)
  expect_true(length(result$x$opts$baseOption$brush) > 0)
  expect_true(e$x$tl)
})

# e_axis_pointer ----------------------------------------------------------
test_that("e_axis_pointer sets axisPointer options", {
  e <- mtcars |> e_charts(mpg) |> e_scatter(wt)
  result <- e_axis_pointer(e, type = "cross")
  expect_equal(result$x$opts$axisPointer$type, "cross")
})

test_that("e_axis_pointer passes additional arguments", {
  e <- mtcars |> e_charts(mpg) |> e_scatter(wt)
  result <- e_axis_pointer(e, show = TRUE, snap = TRUE)
  expect_true(result$x$opts$axisPointer$show)
  expect_true(result$x$opts$axisPointer$snap)
})

test_that("e_axis_pointer works with timeline", {
  e <- mtcars |> dplyr::group_by(cyl) |> e_charts(mpg, timeline = TRUE) |> e_scatter(wt)
  result <- e_axis_pointer(e, type = "shadow")
  expect_equal(result$x$opts$baseOption$axisPointer$type, "shadow")
  expect_true(e$x$tl)
})


# e_animation -------------------------------------------------------------

test_that("e_animation sets animation options when tl is FALSE", {
  e <- mtcars |> e_charts(mpg) |> e_area(wt)
  result <- e_animation(
    e,
    show = FALSE,
    threshold = 100,
    duration = 5000,
    easing = "linear",
    delay = 200,
    duration.update = 3000,
    easing.update = "cubicOut",
    delay.update = 100
  )

  expect_false(result$x$opts$animation)
  expect_equal(result$x$opts$animationThreshold, 100)
  expect_equal(result$x$opts$animationDuration, 5000)
  expect_equal(result$x$opts$animationEasing, "linear")
  expect_equal(result$x$opts$animationDelay, 200)
  expect_equal(result$x$opts$animationDurationUpdate, 3000)
  expect_equal(result$x$opts$animationEasingUpdate, "cubicOut")
  expect_equal(result$x$opts$animationDelayUpdate, 100)
})

test_that("e_animation sets animation options when tl is TRUE", {
  e <- mtcars |> dplyr::group_by(cyl) |> e_charts(mpg, timeline = TRUE) |> e_scatter(wt)
  result <- e_animation(
    e,
    show = TRUE,
    threshold = 50,
    duration = 2000,
    easing = "cubicIn",
    delay = 100,
    duration.update = 1500,
    easing.update = "linear",
    delay.update = 50
  )

  expect_true(result$x$opts$baseOption$animation)
  expect_equal(result$x$opts$baseOption$animationThreshold, 50)
  expect_equal(result$x$opts$baseOption$animationDuration, 2000)
  expect_equal(result$x$opts$baseOption$animationEasing, "cubicIn")
  expect_equal(result$x$opts$baseOption$animationDelay, 100)
  expect_equal(result$x$opts$baseOption$animationDurationUpdate, 1500)
  expect_equal(result$x$opts$baseOption$animationEasingUpdate, "linear")
  expect_equal(result$x$opts$baseOption$animationDelayUpdate, 50)
})

test_that("e_animation uses default show value", {
  e <- mtcars |> e_charts(mpg) |> e_scatter(wt)
  result <- e_animation(e)

  expect_true(result$x$opts$animation)
})

test_that("e_animation handles NULL values", {
  e <- mtcars |> e_charts(mpg) |> e_scatter(wt)
  result <- e_animation(e, show = FALSE)

  expect_null(result$x$opts$animationThreshold)
  expect_null(result$x$opts$animationDuration)
  expect_null(result$x$opts$animationEasing)
  expect_null(result$x$opts$animationDelay)
  expect_null(result$x$opts$animationDurationUpdate)
  expect_null(result$x$opts$animationEasingUpdate)
  expect_null(result$x$opts$animationDelayUpdate)
})

# e_utc -------------------------------------------------------------------
test_that("e_utc sets useUTC to TRUE when tl is FALSE", {
  e <- mtcars |> e_charts(mpg) |> e_scatter(wt)
  result <- e_utc(e)
  expect_true(result$x$opts$useUTC)
})

test_that("e_utc sets useUTC to TRUE when tl is TRUE", {
  e <- mtcars |> dplyr::group_by(cyl) |> e_charts(mpg, timeline = TRUE) |> e_scatter(wt)
  result <- e_utc(e)
  expect_true(result$x$opts$baseOption$useUTC)
})

test_that("e_utc returns the echarts object", {
  e <- mtcars |> e_charts(mpg) |> e_scatter(wt)
  result <- e_utc(e)
  expect_s3_class(result, "echarts4r")
  expect_s3_class(result, "htmlwidget")
})


# e_flip_coords -----------------------------------------------------------
test_that("e_flip_coords flips axis names when not timeline", {
  df <- data.frame(x = LETTERS[1:5], y = 1:5)
  e <- df |> e_charts(x) |> e_bar(y)
  result <- e_flip_coords(e)

  expect_true("yAxis" %in% names(result$x$opts))
  expect_true("xAxis" %in% names(result$x$opts))
})

test_that("e_flip_coords reverses series data values when not timeline", {
  df <- data.frame(x = LETTERS[1:2], y = 1:2)
  e <- df |> e_charts(x) |> e_bar(y)
  result <- e_flip_coords(e)

  original_value <- e$x$opts$series[[1]]$data[[1]]$value
  flipped_value <- result$x$opts$series[[1]]$data[[1]]$value

  expect_equal(flipped_value, rev(original_value))
})

test_that("e_flip_coords flips axis names when timeline", {
  df <- data.frame(x = rep(LETTERS[1:3], 2), y = 1:6, grp = rep(c("A", "B"), each = 3))
  e <- df |> dplyr::group_by(grp) |> e_charts(x, timeline = TRUE) |> e_bar(y)
  result <- e_flip_coords(e)

  expect_true("yAxis" %in% names(result$x$opts$baseOption))
  expect_true("xAxis" %in% names(result$x$opts$baseOption))
})

test_that("e_flip_coords reverses series data values when timeline", {
  df <- data.frame(x = rep(LETTERS[1:2], 2), y = 1:4, grp = rep(c("A", "B"), each = 2))
  e <- df |> dplyr::group_by(grp) |> e_charts(x, timeline = TRUE) |> e_bar(y)
  result <- e_flip_coords(e)

  original_value <- e$x$opts$options[[1]]$series[[1]]$data[[1]]$value
  flipped_value <- result$x$opts$options[[1]]$series[[1]]$data[[1]]$value

  expect_equal(flipped_value, rev(original_value))
})

test_that("e_flip_coords returns echarts4r object", {
  df <- data.frame(x = LETTERS[1:3], y = 1:3)
  e <- df |> e_charts(x) |> e_bar(y)
  result <- e_flip_coords(e)

  expect_s3_class(result, "echarts4r")
  expect_s3_class(result, "htmlwidget")
})
# e_text_style ------------------------------------------------------------
test_that("e_text_style sets textStyle when tl is FALSE", {
  e <- mtcars |> e_charts(mpg) |> e_scatter(wt)
  result <- e_text_style(e, fontSize = 20, fontStyle = "italic")
  expect_equal(result$x$opts$textStyle$fontSize, 20)
  expect_equal(result$x$opts$textStyle$fontStyle, "italic")
})

test_that("e_text_style sets textStyle when tl is TRUE", {
  e <- mtcars |> dplyr::group_by(cyl) |> e_charts(mpg, timeline = TRUE) |> e_scatter(wt)
  # Red is color of timeline text
  result <- e_text_style(e, color = "red", fontSize = 14)
  expect_equal(result$x$opts$baseOption$textStyle$color, "red")
  expect_equal(result$x$opts$baseOption$textStyle$fontSize, 14)
})

test_that("e_text_style passes multiple arguments", {
  e <- mtcars |> e_charts(mpg) |> e_scatter(wt)
  result <- e_text_style(e, fontFamily = "Arial", fontWeight = "bold")
  expect_equal(result$x$opts$textStyle$fontFamily, "Arial")
  expect_equal(result$x$opts$textStyle$fontWeight, "bold")
})

test_that("e_text_style returns the echarts object", {
  e <- mtcars |> e_charts(mpg) |> e_scatter(wt)
  result <- e_text_style(e, fontWeight = "bolder")
  expect_s3_class(result, "echarts4r")
  expect_s3_class(result, "htmlwidget")
})

# e_connect ---------------------------------------------------------------
test_that("can connect, group, and disconnect", {
  e1 <- cars |>
    e_charts(
      speed,
      height = 200
    ) |>
    e_scatter(dist) |>
    e_datazoom(show = FALSE) |>
    e_group("grp") # assign group

  e2 <- cars |>
    e_charts(
      dist,
      height = 200
    ) |>
    e_scatter(speed) |>
    e_datazoom() |>
    e_group("grp") |> # assign group
    e_connect_group("grp") # connect

  e1_con <- e_connect(e1, "group")

  # They belong in the same group
  expect_equal(e1$x$chartGroup, "grp")
  expect_equal(e2$x$chartGroup, "grp")

  expect_equal(e2$x$groupConnect, "grp")

  expect_s3_class(e1, "echarts4r")
  expect_s3_class(e1, "htmlwidget")
  expect_s3_class(e2, "echarts4r")
  expect_s3_class(e2, "htmlwidget")

  expect_equal(e1_con$x$connect, list("group"))

  e2_dis <- e_disconnect_group(e2, group = "grp")
  expect_equal(e2_dis$x$groupDisconnect, "grp")
})

# e_arrange ---------------------------------------------------------------
test_that("e_arrange returns browsable when not in knitr", {
  withr::local_options(knitr.in.progress = NULL)
  e1 <- cars |> e_charts(speed, height = 200) |> e_scatter(dist)
  e2 <- cars |> e_charts(dist, height = 200) |> e_scatter(speed)

  result <- e_arrange(e1, e2)
  expect_s3_class(result, "shiny.tag")
})

test_that("e_arrange returns tagList when in knitr", {
  withr::local_options(knitr.in.progress = TRUE)
  e1 <- cars |> e_charts(speed, height = 200) |> e_scatter(dist)
  e2 <- cars |> e_charts(dist, height = 200) |> e_scatter(speed)

  result <- e_arrange(e1, e2)
  expect_s3_class(result, "shiny.tag.list")
})

test_that("e_arrange defaults rows to length of plots", {
  withr::local_options(knitr.in.progress = TRUE)
  e1 <- cars |> e_charts(speed, height = 200) |> e_scatter(dist)
  e2 <- cars |> e_charts(dist, height = 200) |> e_scatter(speed)

  result <- e_arrange(e1, e2)
  expect_match(as.character(result), "height:200px")
  expect_s3_class(result, "shiny.tag.list")
})

test_that("e_arrange defaults cols to 1", {
  withr::local_options(knitr.in.progress = TRUE)
  e1 <- cars |> e_charts(speed, height = 200) |> e_scatter(dist)

  result <- e_arrange(e1, cols = NULL)
  expect_s3_class(result, "shiny.tag.list")
})

test_that("e_arrange respects rows and cols arguments", {
  withr::local_options(knitr.in.progress = TRUE)
  e1 <- cars |> e_charts(speed, height = 200) |> e_scatter(dist)
  e2 <- cars |> e_charts(dist, height = 200) |> e_scatter(speed)

  result <- e_arrange(e1, e2, rows = 1, cols = 2)
  expect_s3_class(result, "shiny.tag.list")
})

test_that("e_arrange includes title when provided", {
  withr::local_options(knitr.in.progress = TRUE)
  e1 <- cars |> e_charts(speed, height = 200) |> e_scatter(dist)

  result <- e_arrange(e1, title = "Test Title")
  expect_match(as.character(result), "<div> | Test Title")
  expect_s3_class(result, "shiny.tag")
})

test_that("e_arrange handles empty cells when more grid spaces than plots", {
  withr::local_options(knitr.in.progress = TRUE)
  e1 <- cars |> e_charts(speed, height = 200) |> e_scatter(dist)

  result <- e_arrange(e1, rows = 2, cols = 2)

  # Two cols in an empty row
  expect_match(as.character(result), '<div class="row">
  <div class="col-xs-6"></div>
  <div class="col-xs-6"></div>
</div>')
  expect_s3_class(result, "shiny.tag.list")
})

# e_dims ------------------------------------------------------------------
test_that("e_dims sets height and width", {
  e <- mtcars |> e_charts(mpg)
  result <- e_dims(e, height = "500px", width = "800px")
  expect_equal(result$x$mainOpts$height, "500px")
  expect_equal(result$x$mainOpts$width, "800px")
})

test_that("e_dims defaults to auto", {
  e <- mtcars |> e_charts(mpg)
  result <- e_dims(e)
  expect_equal(result$x$mainOpts$height, "auto")
  expect_equal(result$x$mainOpts$width, "auto")
})

# e_locale ----------------------------------------------------------------
test_that("e_locale sets locale", {
  e <- mtcars |> e_charts(mpg)
  result <- e_locale(e, "FR")
  expect_equal(result$x$mainOpts$locale, "FR")
})

test_that("e_locale adds dependency for non-default locales", {
  e <- mtcars |> e_charts(mpg)
  result <- e_locale(e, "FR")
  dep_names <- sapply(result$dependencies, function(x) x$name)
  expect_true("FR-echarts4r-locale" %in% dep_names)
})

test_that("e_locale does not add dependency for ZH or EN", {
  e <- mtcars |> e_charts(mpg)
  result_zh <- e_locale(e, "ZH")
  result_en <- e_locale(e, "EN")
  expect_equal(length(result_zh$dependencies), 0)
  expect_equal(length(result_en$dependencies), 0)
})

# e_locale_manual ---------------------------------------------------------
test_that("e_locale_manual sets locale and adds dependency", {
  e <- mtcars |> e_charts(mpg)
  temp_file <- tempfile(fileext = ".js")
  writeLines("console.log('test');", temp_file)
  result <- e_locale_manual(e, "CUSTOM", temp_file)
  expect_equal(result$x$mainOpts$locale, "CUSTOM")
  dep_names <- sapply(result$dependencies, function(x) x$name)
  expect_true("CUSTOM-manual-echarts4r-locale" %in% dep_names)
  unlink(temp_file)
})
