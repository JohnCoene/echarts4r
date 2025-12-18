### Tests of the functions in the tab "Chart types"
### https://echarts4r.john-coene.com/articles/chart_types.html

# e_line ------------------------------------------------------------------
test_that("e_line plot has the good data structure and type", {

  df <- data.frame(
    x = seq(3),
    y = c(1, 3, 9),
    z = c(2, 5, 4),
    w = c(3, 4, 3)
  )

  plot <- df |>
    e_charts(x) |>
    e_line(z)

  expect_s3_class(plot, "echarts4r")
  expect_s3_class(plot, "htmlwidget")

  expect_equal(
    plot$x$opts$series[[1]]$data,
    list(list(value = c(1, 2)), list(value = c(2, 5)), list(value = c(3, 4)))
  )
  expect_equal(
    plot$x$opts$series[[1]]$type,
    "line"
  )
})

test_that("e_line.echarts4rProxy plot responds", {

  test_data <-  data.frame(
    x = seq(3),
    y = c(1, 3, 9),
    z = c(2, 5, 4),
    w = c(3, 4, 3)
  )

  server <- function(input, output, session) {
    proxy_called <- shiny::reactiveVal(FALSE)

    output$line <- renderEcharts4r({
      test_data |>
        e_charts(x) |>
        e_line(z)
    })

    observeEvent(input$update, {
      echarts4rProxy("line",
                     data = test_data) |>
        e_line(y) |>
        e_execute()

      proxy_called(TRUE)
    })
  }

  shiny::testServer(server, {

    expect_false(proxy_called())

    json <- jsonlite::fromJSON(output$line)

    # Data matches - this is the first point
    expect_identical(
      json$x$opts$series$data[[1]]$value[[1]],
      c(1L, 2L))

    session$setInputs(update = 1)
    session$flushReact()

    json <- jsonlite::fromJSON(output$line)

    # Proxy was called with no errors
    expect_true(proxy_called())

    expect_equal(
      json$x$opts$series$type,
      "line"
    )
  })
})

test_that("e_line.echarts4r and e_line_ expects error when missing e or serie", {
  expect_error(iris |> e_charts() |> e_line.echarts4r(), "must pass serie")
  expect_error(e_line.echarts4r(), "must pass e")

  expect_error(iris |> e_charts() |> e_line_(), "must pass serie")
  expect_error( e_line_(), "must pass e")
})

# e_area ------------------------------------------------------------------

test_that("e_area plot has the good data structure and type", {
  df <- data.frame(
    x = seq(3),
    y = c(1, 3, 9),
    z = c(2, 5, 4),
    w = c(3, 4, 3)
  )
  plot <- df |>
    e_charts(x) |>
    e_area(w)

  expect_s3_class(plot, "echarts4r")
  expect_s3_class(plot, "htmlwidget")

  expect_equal(
    plot$x$opts$series[[1]]$data,
    list(list(value = c(1, 3)), list(value = c(2, 4)), list(value = c(3, 3)))
  )
  expect_equal(
    plot$x$opts$series[[1]]$type,
    "line"
  )
})

test_that("e_area.echarts4rProxy plot responds", {

  test_data <-  data.frame(
    x = seq(3),
    y = c(1, 3, 9),
    z = c(2, 5, 4),
    w = c(3, 4, 3)
  )

  server <- function(input, output, session) {
    proxy_called <- shiny::reactiveVal(FALSE)

    output$area <- renderEcharts4r({
      test_data |>
        e_charts(x) |>
        e_area(z)
    })

    observeEvent(input$update, {
      echarts4rProxy("area",
                     data = test_data) |>
        e_area(y) |>
        e_execute()

      proxy_called(TRUE)
    })
  }

  shiny::testServer(server, {

    expect_false(proxy_called())
    json <- jsonlite::fromJSON(output$area)

    # Data matches - this is the first point
    expect_identical(
      json$x$opts$series$data[[1]]$value[[1]],
      c(1L, 2L))

    session$setInputs(update = 1)
    session$flushReact()

    json <- jsonlite::fromJSON(output$area)

    # Proxy was called with no errors
    expect_true(proxy_called())

    expect_equal(
      json$x$opts$series$type,
      "line"
    )
  })
})

test_that("e_area.echarts4r and e_area_ expects error when missing e or serie", {
  expect_error(iris |> e_charts() |> e_area.echarts4r() , "must pass serie")
  expect_error(e_area.echarts4r(), "must pass e")

  expect_error(iris |> e_charts() |> e_area_(), "must pass serie")
  expect_error( e_area_(), "must pass e")
})


# e_bar -------------------------------------------------------------------

test_that("e_bar plot has the good data structure and type", {
  df <- data.frame(
    x = seq(3),
    y = c(1, 3, 9),
    z = c(2, 5, 4),
    w = c(3, 4, 3)
  )
  plot <- df |>
    e_charts(x) |>
    e_bar(y, name = "Serie 1")

  expect_s3_class(plot, "echarts4r")
  expect_s3_class(plot, "htmlwidget")

  expect_equal(
    plot$x$opts$series[[1]]$data,
    list(list(value = c(1, 1)), list(value = c(2, 3)), list(value = c(3, 9)))
  )
  expect_equal(
    plot$x$opts$series[[1]]$type,
    "bar"
  )
})

test_that("e_bar.echarts4rProxy plot responds", {

  test_data <-  data.frame(
    x = seq(3),
    y = c(1, 3, 9),
    z = c(2, 5, 4),
    w = c(3, 4, 3)
  )

  server <- function(input, output, session) {
    proxy_called <- shiny::reactiveVal(FALSE)

    output$bar <- renderEcharts4r({
      test_data |>
        e_charts(x) |>
        e_bar(y, name = "Serie 1")
    })

    observeEvent(input$update, {
      echarts4rProxy("bar",
                     data = test_data) |>
        e_bar(y, name = "Serie 1") |>
        e_execute()

      proxy_called(TRUE)
    })
  }

  shiny::testServer(server, {

    expect_false(proxy_called())
    json <- jsonlite::fromJSON(output$bar)

    # Data matches - this is the first point
    expect_identical(
      json$x$opts$series$data[[1]]$value[[1]],
      c(1L, 1L))

    session$setInputs(update = 1)
    session$flushReact()

    json <- jsonlite::fromJSON(output$bar)

    # Proxy was called with no errors
    expect_true(proxy_called())

    expect_equal(
      json$x$opts$series$type,
      "bar"
    )
  })
})

test_that("e_bar.echarts4r and e_bar_ expects error when missing e or serie", {
  expect_error(iris |> e_charts() |> e_bar.echarts4r(),
    "must pass serie")

  expect_error(e_bar.echarts4r(), "must pass e")

  expect_error(iris |> e_charts() |> e_bar_(), "must pass serie")
  expect_error(e_bar_(), "must pass e")
})

# e_step ------------------------------------------------------------------
test_that("e_step plot has the good data structure and type", {
  df <- data.frame(
    x = seq(3),
    y = c(1, 3, 9),
    z = c(2, 5, 4),
    w = c(3, 4, 3)
  )
  plot <- df |>
    e_charts(x) |>
    e_step(z, name = "Serie 2")

  expect_s3_class(plot, "echarts4r")
  expect_s3_class(plot, "htmlwidget")

  expect_equal(
    plot$x$opts$series[[1]]$data,
    list(list(value = c(1, 2)), list(value = c(2, 5)), list(value = c(3, 4)))
  )
  expect_equal(
    plot$x$opts$series[[1]]$type,
    "line"
  )
})

test_that("e_step.echarts4r and e_step_ expects error when missing e or serie or wrong step", {
  expect_error(iris |> e_charts() |> e_step.echarts4r(), "must pass serie")
  expect_error(e_step.echarts4r(), "must pass e")
  expect_error(  iris |> e_charts() |> e_step.echarts4r(serie = Sepal.Length, step = "WRONG"), "wrong step")

  expect_error(iris |> e_charts() |> e_step_() , "must pass serie")
  expect_error(e_step_(), "must pass e")
  expect_error( iris |> e_charts() |> e_step_(serie = "Sepal.Length", step = "WRONG"), "wrong step")
})


# e_scatter ---------------------------------------------------------------

test_that("e_scatter plot has the good data structure and type", {
  df <- data.frame(
    x = seq(3),
    y = c(1, 3, 9),
    z = c(2, 5, 4),
    w = c(3, 4, 3)
  )
  plot <- df |>
    e_charts(x) |>
    e_scatter(y)

  expect_s3_class(plot, "echarts4r")
  expect_s3_class(plot, "htmlwidget")

  expect_equal(
    plot$x$opts$series[[1]]$data,
    list(list(value = c(1, 1)), list(value = c(2, 3)), list(value = c(3, 9)))
  )
  expect_equal(
    plot$x$opts$series[[1]]$type,
    "scatter"
  )

  plot2 <- df |>
    e_charts(x) |>
    e_scatter(y) |>
    e_visual_map(y, scale = e_scale)

  expect_s3_class(plot2, "echarts4r")
  expect_s3_class(plot2, "htmlwidget")

  expect_equal(
    plot2$x$opts$series[[1]]$data,
    list(list(value = c(1, 1)), list(value = c(2, 3)), list(value = c(3, 9)))
  )
  expect_equal(
    plot2$x$opts$series[[1]]$type,
    "scatter"
  )
})

test_that("e_scatter.echarts4r and e_scatter_ expects error when missing e", {
  expect_error(iris |> e_charts() |> e_scatter.echarts4r(), "must pass serie")
  expect_error(e_scatter.echarts4r(), "must pass e")

  expect_error(iris |> e_charts() |> e_scatter_() , "must pass serie")
  expect_error(e_scatter_(), "must pass e")
})


# e_effect_scatter --------------------------------------------------------

test_that("e_effect_scatter plot has the good data structure and type", {
  df <- data.frame(
    x = seq(3),
    y = c(1, 3, 9),
    z = c(2, 5, 4),
    w = c(3, 4, 3)
  )
  plot <- df |>
    e_charts(x) |>
    e_effect_scatter(y)

  expect_s3_class(plot, "echarts4r")
  expect_s3_class(plot, "htmlwidget")

  expect_equal(
    plot$x$opts$series[[1]]$data,
    list(list(value = c(1, 1)), list(value = c(2, 3)), list(value = c(3, 9)))
  )
  expect_equal(
    plot$x$opts$series[[1]]$type,
    "effectScatter"
  )


  plot2 <- df |>
    e_charts(x) |>
    e_effect_scatter(y, ) |>
    e_visual_map(z, scale = e_scale)

  expect_s3_class(plot2, "echarts4r")
  expect_s3_class(plot2, "htmlwidget")

  expect_equal(
    plot2$x$opts$series[[1]]$data,
    list(list(value = c(1, 1)), list(value = c(2, 3)), list(value = c(3, 9)))
  )
  expect_equal(
    plot2$x$opts$series[[1]]$type,
    "effectScatter"
  )
})


test_that("e_effect_scatter.echarts4r and e_effect_scatter_ expects error when missing e", {
  expect_error(iris |> e_charts() |> e_effect_scatter.echarts4r(), "must pass serie")
  expect_error(e_effect_scatter.echarts4r(), "must pass e")

  expect_error(iris |> e_charts() |> e_effect_scatter_(),
               "must pass serie")
  expect_error( e_effect_scatter_(), "must pass e")
})


# e_polar -----------------------------------------------------------------

test_that("e_polar plot has the good data structure and type", {
  df <- data.frame(
    x = seq(3),
    y = c(1, 3, 9),
    z = c(2, 5, 4),
    w = c(3, 4, 3)
  )
  plot <- df |>
    e_charts(x) |>
    e_polar() |>
    e_angle_axis(x) |> # angle = x
    e_radius_axis() |>
    e_bar(y, coord_system = "polar")


  expect_s3_class(plot, "echarts4r")
  expect_s3_class(plot, "htmlwidget")

  expect_equal(
    plot$x$opts$series[[1]]$data,
    c(list(c(1)), list(c(3)), list(c(9)))
  )
  expect_equal(
    plot$x$opts$series[[1]]$type,
    "bar"
  )
})

test_that("e_polar plot has the good data structure and type", {
  df <- data.frame(
    x = seq(3),
    y = c(1, 3, 9),
    z = c(2, 5, 4),
    w = c(3, 4, 3)
  )
  plot <- df |>
    e_charts(x) |>
    e_polar() |>
    e_angle_axis_("x") |> # angle = x
    e_radius_axis_() |>
    e_bar_("y", coord_system = "polar")


  expect_s3_class(plot, "echarts4r")
  expect_s3_class(plot, "htmlwidget")

  expect_equal(
    plot$x$opts$series[[1]]$data,
    c(list(c(1)), list(c(3)), list(c(9)))
  )
  expect_equal(
    plot$x$opts$series[[1]]$type,
    "bar"
  )
})

# e_radius ----------------------------------------------------------------

# test_that("e_radius plot has the good data structure and type", {
# df <- data.frame(
#   x = seq(3),
#   y = c(1, 3, 9),
#   z = c(2, 5, 4),
#   w = c(3, 4, 3)
# )
#   plot <- df |>
#     head(10) |>
#     e_charts(x) |>
#     e_radius_axis(x)
#
#   plot2 <- df |>
#     head(10) |>
#     e_charts(x) |>
#     e_radius_axis(x) |>
#     e_bar(y, coord_system = "polar") |>
#     e_scatter(z, coord_system = "polar")
#
#   expect_s3_class(plot, "echarts4r")
#   expect_s3_class(plot, "htmlwidget")
#
#   expect_s3_class(plot2, "echarts4r")
#   expect_s3_class(plot2, "htmlwidget")
# })


# e_candle ----------------------------------------------------------------

test_that("e_candle plot has the good data structure and type", {
  date <- c(
    "2017-01-01",
    "2017-01-02",
    "2017-01-03",
    "2017-01-04",
    "2017-03-05",
    "2017-01-06",
    "2017-01-07"
  )

  stock <- data.frame(
    date = date,
    opening = c(200.60, 200.22, 198.43, 199.05, 203.54, 203.40, 208.34),
    closing = c(200.72, 198.85, 199.05, 203.73, 204.08, 208.11, 211.88),
    low = c(197.82, 198.07, 197.90, 198.10, 202.00, 201.50, 207.60),
    high = c(203.32, 200.67, 200.00, 203.95, 204.90, 208.44, 213.17)
  )

  plot <- stock |>
    e_charts(date) |>
    e_candle(opening, closing, low, high) |>
    e_y_axis(min = 190, max = 220)

  expect_s3_class(plot, "echarts4r")
  expect_s3_class(plot, "htmlwidget")

  expect_equal(
    plot$x$opts$series[[1]]$data,
    list(
        list(value = c(200.60, 200.72, 197.82, 203.32)),
        list(value = c(200.22, 198.85, 198.07, 200.67)),
        list(value = c(198.43, 199.05, 197.90, 200.00)),
        list(value = c(199.05, 203.73, 198.10, 203.95)),
        list(value = c(203.54, 204.08, 202.00, 204.90)),
        list(value = c(203.40, 208.11, 201.50, 208.44)),
        list(value = c(208.34, 211.88, 207.60, 213.17))
      )
  )
  expect_equal(
    plot$x$opts$series[[1]]$type,
    "candlestick"
  )
})

test_that("e_candle.echarts4r and e_candle_ expects error when missing e and an input", {
  expect_error(iris |> e_charts() |> e_candle.echarts4r(), "missing inputs")
  expect_error(e_candle.echarts4r(), "must pass e")

  expect_error(iris |> e_charts() |> e_candle_() , "missing inputs")
  expect_error(e_candle_() , "must pass e")
})


test_that("e_candle.echarts4r and e_candle_ expects error when missing e and an input", {
  expect_error(iris |> e_charts() |> e_candle.echarts4r(), "missing inputs")
  expect_error(e_candle.echarts4r(), "must pass e")

  expect_error(iris |> e_charts() |> e_candle_() , "missing inputs")
  expect_error(e_candle_() , "must pass e")
})


# e_funnel ----------------------------------------------------------------

test_that("e_funnel plot has the good data structure and type", {
  funnel <- data.frame(stage = c("View", "Click", "Purchase"), value = c(80, 30, 20))

  plot <- funnel |>
    e_charts() |>
    e_funnel(value, stage) |>
    e_title("Funnel")

  expect_s3_class(plot, "echarts4r")
  expect_s3_class(plot, "htmlwidget")

  expect_equal(
    plot$x$opts$series[[1]]$data,
    list(list(value = c(80), name = "View"), list(value = c(30), name = "Click"), list(value = c(20), name = "Purchase"))
  )
  expect_equal(
    plot$x$opts$series[[1]]$type,
    "funnel"
  )
})

# e_sankey ----------------------------------------------------------------

test_that("e_sankey plot has the good data structure and type", {
  sankey <- data.frame(
    source = c("a", "b", "c", "d", "c"),
    target = c("b", "c", "d", "e", "e"),
    value = ceiling(rnorm(5, 10, 1)),
    stringsAsFactors = FALSE
  )

  plot <- sankey |>
    e_charts() |>
    e_sankey(source, target, value) |>
    e_title("Sankey chart")

  expect_s3_class(plot, "echarts4r")
  expect_s3_class(plot, "htmlwidget")

  expect_equal(
    plot$x$opts$series[[1]]$data,
    list(list(name = c("a")), list(name = c("b")), list(name = c("c")), list(name = c("d")), list(name = c("e")))
  )
  expect_equal(
    plot$x$opts$series[[1]]$type,
    "sankey"
  )
})


test_that("e_sankey.echarts4r and e_sankey_ expects error when missing e and values/labels", {
  expect_error(iris |> e_charts() |> e_sankey.echarts4r() , "missing source, target or values")
 expect_error( e_sankey.echarts4r(), "must pass e")

 expect_error( iris |> e_charts() |> e_sankey_() , "missing source, target or values")
 expect_error( e_sankey_(), "must pass e")
})


# e_heatmap ---------------------------------------------------------------

### Careful : heatmap not full to reduce the data to write in expect_equal

test_that("e_heatmap plot has the good data structure and type", {
  set.seed(1)

  v <- LETTERS[1:5]

  matrix <- data.frame(
    x = sample(v, 5, replace = TRUE),
    y = sample(v, 5, replace = TRUE),
    z = rnorm(5, 10, 1),
    stringsAsFactors = FALSE
  ) |>
    dplyr::group_by(x, y) |>
    dplyr::summarise(z = sum(z)) |>
    dplyr::ungroup()

  plot <- matrix |>
    e_charts(x) |>
    e_heatmap(y, z) |>
    e_visual_map(z) |>
    e_title("Heatmap")

  expect_s3_class(plot, "echarts4r")
  expect_s3_class(plot, "htmlwidget")

  expect_equal(
    plot$x$opts$series[[1]]$data,
    list(list(value = c("A", "C", "22.109933")), list(value = c("B", "C", "10.763593")), list(value = c("D", "B", " 9.994233")), list(value = c("E", "A", " 9.200991")))
  )
  expect_equal(
    plot$x$opts$series[[1]]$type,
    "heatmap"
  )
})
test_that("e_heatmap.echarts4r and e_heatmap_ expects error when missing e and y", {
  expect_error(iris |> e_charts() |> e_heatmap.echarts4r(), "must pass y")
  expect_error( e_heatmap.echarts4r(), "must pass e")

  expect_error(iris |> e_charts() |> e_heatmap_(), "must pass y")
  expect_error(e_heatmap_(), "must pass e")
})

test_that("e_heatmap.echarts4r and e_heatmap_ expects error when missing e and y", {
  expect_error(iris |> e_charts() |> e_heatmap.echarts4r(), "must pass y")
  expect_error( e_heatmap.echarts4r(), "must pass e")

  expect_error(iris |> e_charts() |> e_heatmap_(), "must pass y")
  expect_error(e_heatmap_(), "must pass e")
})


# e_parallel --------------------------------------------------------------

test_that("e_parallel plot has the good data structure and type", {
  df <- data.frame(
    price = c(2, 4, 3),
    amount = c(12, 3, 1),
    letter = LETTERS[1:3]
  )

  plot <- df |>
    e_charts() |>
    e_parallel(price, amount, letter) |>
    e_title("Parallel chart")

  expect_s3_class(plot, "echarts4r")
  expect_s3_class(plot, "htmlwidget")

  expect_equal(
    plot$x$opts$series$data,
    list(list(c("2"), c("12"), c("A")), list(c("4"), c("3"), c("B")), list(c("3"), c("1"), c("C")))
  )
  expect_equal(
    plot$x$opts$series$type,
    "parallel"
  )
})

# e_pie -------------------------------------------------------------------

test_that("e_pie plot has the good data structure and type", {
  plot <- mtcars |>
    head(5) |>
    tibble::rownames_to_column("model") |>
    e_charts(model) |>
    e_pie(carb) |>
    e_title("Pie chart")

  expect_s3_class(plot, "echarts4r")
  expect_s3_class(plot, "htmlwidget")

  expect_equal(
    plot$x$opts$series[[1]]$data,
    list(list(value = c(4), name = c("Mazda RX4")), list(value = c(4), name = c("Mazda RX4 Wag")), list(value = c(1), name = c("Datsun 710")), list(value = c(1), name = c("Hornet 4 Drive")), list(value = c(2), name = c("Hornet Sportabout")))
  )
  expect_equal(
    plot$x$opts$series[[1]]$type,
    "pie"
  )
})

test_that("e_pie.echarts4rProxy plot responds", {

    test_data <- shiny::reactiveVal(data.frame(
      name = c("A", "B"),
      value = c(40, 60)
    ))

    server <- function(input, output, session) {
      proxy_called <- shiny::reactiveVal(FALSE)

      output$pie <- renderEcharts4r({
        e_charts(test_data(), name) |>
          e_pie(value, radius = c("30%", "70%"))
      })

      observeEvent(input$update, {
        echarts4rProxy("pie",
                       data = test_data(), x = value) |>
          e_pie(value, radius = c("30%", "70%")) |>
          e_execute()

        proxy_called(TRUE)
      })
    }

    shiny::testServer(server, {

      expect_false(proxy_called())

      pie_json <- jsonlite::fromJSON(output$pie)

      # Data matches
      expect_identical(
        pie_json$x$opts$series$data[[1]]$value,
        c(40L, 60L))

      session$setInputs(update = 1)
      session$flushReact()

      pie_json <- jsonlite::fromJSON(output$pie)

      # Proxy was called with no errors
      expect_true(proxy_called())

      expect_equal(
        pie_json$x$opts$series$type,
        "pie"
      )
    })
})

test_that("e_pie.echarts4r.echarts4r and e_pie_ expects error when missing e and serie", {
   expect_error(iris |> e_charts() |> e_pie.echarts4r(), "must pass serie")
   expect_error(e_pie.echarts4r(), "must pass e")

   expect_error(iris |> e_charts() |> e_pie_() , "must pass serie")
   expect_error(e_pie_(), "must pass e")
})


# e_donut -----------------------------------------------------------------
# TODO this is not a donut
test_that("e_donut plot has the good data structure and type", {
  plot <- mtcars |>
    head(5) |>
    tibble::rownames_to_column("model") |>
    e_charts(model) |>
    e_pie(carb, radius = c("50%", "70%")) |>
    e_title("Donut chart")

  expect_s3_class(plot, "echarts4r")
  expect_s3_class(plot, "htmlwidget")

  expect_equal(
    plot$x$opts$series[[1]]$data,
    list(list(value = c(4), name = c("Mazda RX4")), list(value = c(4), name = c("Mazda RX4 Wag")), list(value = c(1), name = c("Datsun 710")), list(value = c(1), name = c("Hornet 4 Drive")), list(value = c(2), name = c("Hornet Sportabout")))
  )
  expect_equal(
    plot$x$opts$series[[1]]$type,
    "pie"
  )
})


# e_rosetype --------------------------------------------------------------
# test_that("e_rosetype plot has the good data structure and type", {
#   plot <- mtcars |>
#     head(5) |>
#     tibble::rownames_to_column("model") |>
#     e_charts(model) |>
#     e_pie(hp, roseType = "radius")
#
#   expect_s3_class(plot, "echarts4r")
#   expect_s3_class(plot, "htmlwidget")
#
#   expect_equal(
#     plot$x$opts$series[[1]]$data,
#     list(list(value = c(110), name = c("Mazda RX4")), list(value = c(110), name = c("Mazda RX4 Wag")), list(value = c(93), name = c("Datsun 710")), list(value = c(110), name = c("Hornet 4 Drive")), list(value = c(175), name = c("Hornet Sportabout")))
#   )
#   expect_equal(
#     plot$x$opts$series[[1]]$type,
#     "pie"
#   )
# })


# e_sunburst --------------------------------------------------------------

### TODO make the equivalent of plot$x$opts$series[[1]]$data in expect_equal()

test_that("e_sunburst plot has the good data structure and type", {
  df <- dplyr::tibble(
    name = c("earth", "mars", "venus"), value = c(30, 40, 30),        # 1st level
    itemStyle = dplyr::tibble(color = c(NA, 'red', 'blue')),
    children = list(
      dplyr::tibble(name = c("land", "ocean"), value = c(10,20),             # 2nd level
             children = list(
               dplyr::tibble(name = c("forest", "river"), value = c(3,7)),   # 3rd level
               dplyr::tibble(name = c("fish", "kelp"), value = c(10,5),
                      children = list(
                        dplyr::tibble(name = c("shark", "tuna"), value = c(2,6)),  # 4th level
                        NULL  # kelp
                      ))
             )),
      dplyr::tibble(name = c("crater", "valley"), value = c(20,20)),
      NULL  # venus
    )
  )

  plot <- df |>
    e_charts() |>
    e_sunburst() |>
    e_title("Sunburst")

  expect_s3_class(plot, "echarts4r")
  expect_s3_class(plot, "htmlwidget")

  level_one <- plot$x$opts$series[[1]]$data
  level_one_names <- lapply(1:length(level_one), \(x) level_one[[x]]$name) |> unlist()
  expect_equal(level_one_names,  c("earth", "mars", "venus"))


  level_two <- plot$x$opts$series[[1]]$data[[1]]$children
  level_two_names <-
    lapply(1:length(level_two), \(x) level_two[[x]]$name) |> unlist()
  expect_equal(level_two_names,  c("land", "ocean"))

  level_three <- plot$x$opts$series[[1]]$data[[1]]$children[[1]]$children
  level_three_names <-
    lapply(1:length(level_three), \(x) level_three[[x]]$name) |> unlist()
  expect_equal(level_three_names,  c("forest", "river"))

  expect_equal(
    plot$x$opts$series[[1]]$type,
    "sunburst"
  )
})

test_that("e_sunburst.echarts4r and e_sunburst_ expects error when missing e", {
   expect_error(e_sunburst.echarts4r(), "must pass e")
   expect_error(e_sunburst_(), "must pass e")
})

test_that("e_tree plot has the good data structure and type", {
  tree <- dplyr::tibble(
    name = "earth",        # 1st level
    children = list(
      dplyr::tibble(name = c("land", "ocean"),             # 2nd level
             children = list(
               dplyr::tibble(name = c("forest", "river")),   # 3rd level
               dplyr::tibble(name = c("fish", "kelp"),
                      children = list(
                        dplyr::tibble(name = c("shark", "tuna"),  # 4th level
                               NULL  # kelp
                        ))
               )
             ))
    )
  )

  plot <- tree |>
    e_charts() |>
    e_tree() |>
    e_title("Tree graph")

  expect_s3_class(plot, "echarts4r")
  expect_s3_class(plot, "htmlwidget")

  level_one <- plot$x$opts$series[[1]]$data
  level_one_names <- lapply(1:length(level_one), \(x) level_one[[x]]$name) |> unlist()
  expect_equal(level_one_names,  c("earth"))


  level_two <- plot$x$opts$series[[1]]$data[[1]]$children
  level_two_names <-
    lapply(1:length(level_two), \(x) level_two[[x]]$name) |> unlist()
  expect_equal(level_two_names,  c("land", "ocean"))

  level_three <- plot$x$opts$series[[1]]$data[[1]]$children[[1]]$children
  level_three_names <-
    lapply(1:length(level_three), \(x) level_three[[x]]$name) |> unlist()
  expect_equal(level_three_names,  c("forest", "river"))

  expect_equal(
    plot$x$opts$series[[1]]$type,
    "tree"
  )
})

# e_treemao ---------------------------------------------------------------

test_that("e_tree.echarts4r and e_tree_ expects error when missing e", {
  expect_error(e_tree.echarts4r() , "must pass e")
  expect_error(e_tree_(), "must pass e")
})

test_that("e_treemap plot has the good data structure and type", {
  df <- dplyr::tibble(
    name = c("earth", "mars", "venus"), value = c(30, 40, 30),        # 1st level
    itemStyle = dplyr::tibble(color = c(NA, 'red', 'blue')),
    children = list(
      dplyr::tibble(name = c("land", "ocean"), value = c(10,20),             # 2nd level
             children = list(
               dplyr::tibble(name = c("forest", "river"), value = c(3,7)),   # 3rd level
               dplyr::tibble(name = c("fish", "kelp"), value = c(10,5),
                      children = list(
                        dplyr::tibble(name = c("shark", "tuna"), value = c(2,6)),  # 4th level
                        NULL  # kelp
                      ))
             )),
      dplyr::tibble(name = c("crater", "valley"), value = c(20,20)),
      NULL  # venus
    )
  )

  plot <- df |>
    e_charts() |>
    e_treemap() |>
    e_title("Treemap chart")

  expect_s3_class(plot, "echarts4r")
  expect_s3_class(plot, "htmlwidget")

  level_one <- plot$x$opts$series[[1]]$data
  level_one_names <- lapply(1:length(level_one), \(x) level_one[[x]]$name) |> unlist()
  expect_equal(level_one_names,  c("earth", "mars", "venus"))


  level_two <- plot$x$opts$series[[1]]$data[[1]]$children
  level_two_names <-
    lapply(1:length(level_two), \(x) level_two[[x]]$name) |> unlist()
  expect_equal(level_two_names,  c("land", "ocean"))

  level_three <- plot$x$opts$series[[1]]$data[[1]]$children[[1]]$children
  level_three_names <-
    lapply(1:length(level_three), \(x) level_three[[x]]$name) |> unlist()
  expect_equal(level_three_names,  c("forest", "river"))

  expect_equal(
    plot$x$opts$series[[1]]$type,
    "treemap"
  )
})

test_that("e_treemap.echarts4r and e_treemap_ expects error when missing e", {
  expect_error(e_treemap.echarts4r() , "must pass e")
  expect_error(e_treemap_(), "must pass e")
})


# e_river -----------------------------------------------------------------

test_that("e_river plot has the good data structure and type", {
  set.seed(1)

  dates <- c("2020-10-08", "2020-10-09", "2020-10-10")

  river <- data.frame(
    dates = dates,
    apples = runif(length(dates)),
    bananas = runif(length(dates)),
    pears = runif(length(dates))
  )

  plot <- river |>
    e_charts(dates) |>
    e_river(apples) |>
    e_river(bananas) |>
    e_tooltip(trigger = "axis") |>
    e_title("River charts", "(Streamgraphs)")

  expect_s3_class(plot, "echarts4r")
  expect_s3_class(plot, "htmlwidget")

  expect_equal(
    plot$x$opts$series[[1]]$data,
    list(
      list(c("2020-10-08"), c("0.2655087"), c("apples")),
      list(c("2020-10-09"), c("0.3721239"), c("apples")),
      list(c("2020-10-10"), c("0.5728534"), c("apples")),
      list(c("2020-10-08"), c("0.9082078"), c("bananas")),
      list(c("2020-10-09"), c("0.2016819"), c("bananas")),
      list(c("2020-10-10"), c("0.8983897"), c("bananas"))
    )
  )
  expect_equal(
    plot$x$opts$series[[1]]$type,
    "themeRiver"
  )
})
test_that("e_river.echarts4r and e_river_ expects error when missing e and serie", {
  expect_error(iris |> e_charts() |> e_river.echarts4r(), "must pass serie")
  expect_error(e_river.echarts4r(), "must pass e")

  expect_error( iris |> e_charts() |> e_river_(), "must pass serie")
  expect_error(e_river_(), "must pass e")
})


# e_calendar --------------------------------------------------------------
test_that("e_calendar plot has the good data structure and type", {
  set.seed(1)
  dates <- seq.Date(as.Date("2017-01-01"), as.Date("2017-01-05"), by = "day")
  values <- rnorm(length(dates), 20, 6)

  year <- data.frame(date = dates, values = values)

  plot <- year |>
    e_charts(date) |>
    e_calendar(range = "2017") |>
    e_heatmap(values, coord_system = "calendar") |>
    e_visual_map(max = 30) |>
    e_title("Calendar", "Heatmap")


  expect_s3_class(plot, "echarts4r")
  expect_s3_class(plot, "htmlwidget")

  expect_equal(
    plot$x$opts$series[[1]]$data,
    list(list(value = c("2017-01-01", "16.24128")), list(value = c("2017-01-02", "21.10186")), list(value = c("2017-01-03", "14.98623")), list(value = c("2017-01-04", "29.57168")), list(value = c("2017-01-05", "21.97705")))
  )
  expect_equal(
    plot$x$opts$series[[1]]$type,
    "heatmap"
  )
})

test_that("e_calendar plot works with timeline", {
  set.seed(1)
  dates <- seq.Date(as.Date("2017-01-01"), as.Date("2017-01-05"), by = "day")
  values <- rnorm(length(dates), 20, 6)

  year <- data.frame(date = dates, values = values)

  plot <- year |>
    dplyr::group_by(date) |>
    e_charts(date, timeline = TRUE) |>
    e_calendar(range = "2017")

  expect_s3_class(plot, "echarts4r")
  expect_s3_class(plot, "htmlwidget")

  # Matches range
  expect_equal(
    plot$x$opts$baseOption$calendar[[1]][[1]],
    "2017"
  )
})

test_that("e_calendar expects error when missing e or range", {
  expect_error(iris |> e_charts() |> e_calendar() , "missing e or range")
  expect_error(e_calendar(), "missing e or range")
})

# e_gauge -----------------------------------------------------------------
test_that("e_gauge plot has the good data structure and type", {
  plot <- e_charts() |>
    e_gauge(41, "PERCENT") |>
    e_title("Gauge")

  expect_s3_class(plot, "echarts4r")
  expect_s3_class(plot, "htmlwidget")

  expect_equal(
    plot$x$opts$series[[1]]$data,
    list(list(value = 41, name = "PERCENT"))
  )
  expect_equal(
    plot$x$opts$series[[1]]$type,
    "gauge"
  )
})

test_that("e_gauge expects error when missing e or range", {
  expect_error(iris |> e_charts() |> e_gauge.echarts4r() , "missing e, name, or value")
  expect_error(e_gauge_(), "missing e, name, or value")
})

# e_radar -----------------------------------------------------------------
test_that("e_radar plot has the good data structure and type", {
  set.seed(1)

  df <- data.frame(
    x = LETTERS[1:5],
    y = round(runif(5, 1, 5), 6),
    z = round(runif(5, 3, 7), 6)
  )

  plot <- df |>
    e_charts(x) |>
    e_radar(y, max = 7, name = "radar") |>
    e_radar(z, max = 7, name = "chart") |>
    e_tooltip(trigger = "item")

  expect_s3_class(plot, "echarts4r")
  expect_s3_class(plot, "htmlwidget")

  expect_equal(
    plot$x$opts$series[[1]]$data,
    list(
      list(value = c(2.062035, 2.488496, 3.291413, 4.632831, 1.806728), name = "radar"),
      list(value = c(6.593559, 6.778701, 5.643191, 5.516456, 3.247145), name = "chart")
    )
  )
  expect_equal(
    plot$x$opts$series[[1]]$type,
    "radar"
  )
})

test_that("e_radar.echarts4r and e_radar_ expects error when missing e and serie", {
  expect_error(iris |> e_charts() |> e_radar.echarts4r() , "must pass serie")
  expect_error(e_radar.echarts4r(), "must pass e")

  expect_error(iris |> e_charts() |> e_radar_(), "must pass serie")
  expect_error(e_radar_(), "must pass e")
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


# e_liquid ----------------------------------------------------------------

test_that("e_liquid plot has the good data structure and type", {
  liquid <- data.frame(val = c(0.6, 0.5, 0.4))

  plot <- liquid |>
    e_charts() |>
    e_liquid(val)

  expect_s3_class(plot, "echarts4r")
  expect_s3_class(plot, "htmlwidget")

  expect_equal(
    plot$x$opts$series[[1]]$data,
    c(0.6, 0.5, 0.4)
  )
  expect_equal(
    plot$x$opts$series[[1]]$type,
    "liquidFill"
  )
})


# e_mark_p ----------------------------------------------------------------
test_that("e_mark_p has good data structure", {
  data(EuStockMarkets)
  dd <- as.data.frame(EuStockMarkets) |>
    dplyr::slice_head(n = 50) |>
    dplyr::mutate(day = 1:dplyr::n())

  plot <- dd |>
    e_charts(day) |>
    e_line(SMI, symbol = "none") |>
    e_mark_p(
      type = "line",
      serie_index = 1,
      data = list(
        list(xAxis = dd$day[10], yAxis = dd$SMI[10]),
        list(xAxis = dd$day[37], yAxis = dd$SMI[37])
      )
    )

  expect_equal(
    plot$x$opts$series[[1]]$markLine$data[[1]][[1]]$xAxis,
    10
  )
  expect_equal(
    plot$x$opts$series[[1]]$markLine$data[[1]][[1]]$yAxis,
    1716.3
  )
})


# e_modularity ------------------------------------------------------------

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


# e_doughnut --------------------------------------------------------------
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
