
# e_mark ------------------------------------------------------------------
test_that("e_mark_point plot has the good data structure and type", {

  df <- data.frame(
    x = seq(3),
    y = c(1, 3, 9),
    z = c(2, 5, 4),
    w = c(3, 4, 3)
  )

  plot <- df |>
    e_charts(x) |>
    e_line(z) |>
    e_mark_point("z", data = list(
      name = "Max",
      type = "max"
    ),
    title = "max!")

  expect_s3_class(plot, "echarts4r")
  expect_s3_class(plot, "htmlwidget")

  expect_equal(
    plot$x$opts$series[[1]]$markPoint$data[[1]],
    list(
      name = "Max",
      type = "max"
    , label = list(formatter = "max!", position = NULL)))
})

test_that("e_mark_point timeline works", {

  df <- data.frame(
    x = seq(3),
    y = c(1, 3, 9),
    z = c(2, 5, 4),
    w = c(3, 4, 3),
    group = c("A", "A", "B")
  )

  plot <- df |> dplyr::group_by(group) |> e_chart(x, timeline = TRUE) |>
    e_line(z) |>
    e_mark_point("z", data = list(
      name = "Max",
      type = "max"
    )) |>
    e_mark_point("z", data = list(
      name = "Min",
      type = "min"
    ))

  expect_true(plot$x$tl)

  # Time series
  expect_equal(
    plot$x$opts$baseOption$timeline$data,
    list("A", "B")
  )

  # 2nd timeline - only 1 point
  expect_equal(
  plot$x$opts$options[[2]]$series[[1]]$data[[1]]$value,
  c(3,4))

  expect_equal(plot$x$opts$baseOption$series[[1]]$type, "line")
})

test_that("e_mark_line plot has the good data structure and type", {

  df <- data.frame(
    x = seq(3),
    y = c(1, 3, 9),
    z = c(2, 5, 4),
    w = c(3, 4, 3)
  )

  plot <- df |>
    e_charts(x) |>
    e_line(z) |>
    e_mark_line("z", data = list(
      name = "Max",
      type = "max"
    ))

  expect_s3_class(plot, "echarts4r")
  expect_s3_class(plot, "htmlwidget")

  expect_equal(
    plot$x$opts$series[[1]]$markLine$data[[1]],
    list(
      name = "Max",
      type = "max"
    ))
})

test_that("e_mark_point timeline works", {

  df <- data.frame(
    x = seq(3),
    y = c(1, 3, 9),
    z = c(2, 5, 4),
    w = c(3, 4, 3),
    group = c("A", "A", "B")
  )

  plot <- df |> dplyr::group_by(group) |> e_chart(x, timeline = TRUE) |>
    e_line(z) |>
    e_mark_line(data = list(
      name = "Max",
      type = "max"
    )) |>
    e_mark_line(data = list(
      name = "Min",
      type = "min"
    ))

  expect_true(plot$x$tl)

  # Time series
  expect_equal(
    plot$x$opts$baseOption$timeline$data,
    list("A", "B")
  )

  # 2nd timeline - only 1 point
  expect_equal(
    plot$x$opts$options[[2]]$series[[1]]$data[[1]]$value,
    c(3,4))

  expect_equal(plot$x$opts$baseOption$series[[1]]$type, "line")
})


test_that("e_mark_area plot has the good data structure and type", {

  df <- data.frame(
    x = seq(3),
    y = c(1, 3, 9),
    z = c(2, 5, 4),
    w = c(3, 4, 3)
  )

  plot <- df |>
    e_charts(x) |>
    e_line(z) |>
    e_mark_area("z", data = list(
      list(xAxis = "min", yAxis = "min"),
      list(xAxis = "max", yAxis = "max")
    ))

  expect_s3_class(plot, "echarts4r")
  expect_s3_class(plot, "htmlwidget")

  expect_equal(
    plot$x$opts$series[[1]]$markArea$data[[1]],
    list(
      list(xAxis = "min", yAxis = "min"),
      list(xAxis = "max", yAxis = "max")
    ))
})


test_that("e_mark_area timeline works", {

  df <- data.frame(
    x = seq(3),
    y = c(1, 3, 9),
    z = c(2, 5, 4),
    w = c(3, 4, 3),
    group = c("A", "A", "B")
  )

  plot <- df |> dplyr::group_by(group) |> e_chart(x, timeline = TRUE) |>
    e_line(z) |>
    e_mark_area(data = list(
      list(xAxis = "min", yAxis = "min"),
      list(xAxis = "max", yAxis = "max")
    ))

  expect_true(plot$x$tl)

  # Time series
  expect_equal(
    plot$x$opts$baseOption$timeline$data,
    list("A", "B")
  )

  # 2nd timeline - only 1 point
  expect_equal(
    plot$x$opts$options[[2]]$series[[1]]$data[[1]]$value,
    c(3,4))

  expect_equal(plot$x$opts$baseOption$series[[1]]$type, "line")
})

test_that("e_mark expects error when missing e", {
  expect_error(e_mark_point(), "must pass e")
  expect_error(e_mark_line(), "must pass e")
  expect_error(e_mark_area(), "must pass e")
  expect_error(e_mark_p.echarts4r(), "must pass e")
  expect_error(e_mark_p_(), "must pass e")
  expect_error(e_charts() |> e_mark_p_(), "must pass type")
  expect_error(e_charts() |> e_mark_p_(type = "FAKE"), "type must be line,point or area")
})
