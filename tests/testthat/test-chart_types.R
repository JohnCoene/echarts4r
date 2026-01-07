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

test_that("e_line timeline works", {

  plot <- iris |> dplyr::group_by(Species) |> e_chart(Species, timeline = TRUE) |>
    e_line(Sepal.Length
    ) |> e_timeline_serie(
      title = list(
        list(text = "setosa"),
        list(text = "versicolor"),
        list(text = "virginica")
      )
    )
  expect_true(plot$x$tl)

  # Time series
  expect_equal(
    plot$x$opts$baseOption$timeline$data,
    list("setosa", "versicolor", "virginica")
  )

  expect_equal(plot$x$opts$baseOption$series[[1]]$type, "line")
})

test_that("e_line x and y index works", {
  plot <- iris |> e_chart(Species) |>
    e_line(Sepal.Length,
          y_index = 10,
          x_index = 10
    )
  expect_gt(length(plot$x$opts$xAxis), 10)
  expect_gt(length(plot$x$opts$yAxis), 10)
})

test_that("e_line polar works", {

  plot <- iris |> e_chart(Species) |>
    e_polar() |>
    e_angle_axis(Species) |>
    e_radius_axis() |>
    e_line(Sepal.Length, coord_system = "polar")

  expect_length(plot$x$opts$polar, 1)

  expect_length(plot$x$opts$series[[1]]$data, nrow(iris))
  expect_equal(as.character(unlist(plot$x$opts$angleAxis$data)), c("setosa", "versicolor", "virginica"))

  expect_equal(plot$x$opts$series[[1]]$type, "line")
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
    proxy_chart <- shiny::reactiveVal(NULL)

    output$line <- renderEcharts4r({
      test_data |>
        e_charts(x) |>
        e_line(z)
    })

    observeEvent(input$update, {
      chart <- echarts4rProxy("line",
                     data = test_data) |>
        e_line(y) |>
        e_execute()
      proxy_chart(chart)

      proxy_called(TRUE)
    })
  }

  shiny::testServer(server, {

    expect_false(proxy_called())

    json <- jsonlite::fromJSON(output$line)

    expect_equal(
      do.call(rbind, json$x$opts$series$data[[1]]$value),
      unname(as.matrix( test_data[c("x", "z")]))
    )

    session$setInputs(update = 1)
    session$flushReact()

    # Update to line x
    new_values <- proxy_chart()$chart$x$opts$series[[1]]$data |> unlist() |> unname()

    expect_equal(new_values,c(1,3,9))
    # Proxy was called with no errors
    expect_true(proxy_called())

    expect_equal(
      proxy_chart()$chart$x$opts$series[[1]]$type,
      "line"
    )

    expect_error(echarts4rProxy("line", data = test_data) |>
                   e_line(), "must pass serie")
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

test_that("e_area timeline works", {

  plot <- iris |> dplyr::group_by(Species) |> e_chart(Species, timeline = TRUE) |>
    e_area(Sepal.Length
    ) |> e_timeline_serie(
      title = list(
        list(text = "setosa"),
        list(text = "versicolor"),
        list(text = "virginica")
      )
    )
  expect_true(plot$x$tl)

  # Time series
  expect_equal(
    plot$x$opts$baseOption$timeline$data,
    list("setosa", "versicolor", "virginica")
  )

  expect_equal(plot$x$opts$baseOption$series[[1]]$type, "line")
})

test_that("e_area x and y index works", {
  plot <- iris |> e_chart(Species) |>
    e_area(Sepal.Length,
           y_index = 10,
           x_index = 10
    )
  expect_gt(length(plot$x$opts$xAxis), 10)
  expect_gt(length(plot$x$opts$yAxis), 10)
})
test_that("e_area polar works", {

  plot <- iris |> e_chart(Species) |>
    e_polar() |>
    e_angle_axis(Species) |>
    e_radius_axis() |>
    e_area(Sepal.Length, coord_system = "polar"
    )
  # Time series
  expect_equal(
    plot$x$opts$angleAxis$data |> unlist() |> as.character(),
    c("setosa", "versicolor", "virginica")
  )

  expect_equal(plot$x$opts$series[[1]]$coordinateSystem, "polar")
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
    proxy_chart <- shiny::reactiveVal(NULL)

    output$area <- renderEcharts4r({
      test_data |>
        e_charts(x) |>
        e_area(z)
    })

    observeEvent(input$update, {
      chart <- echarts4rProxy("area",
                     data = test_data) |>
        e_area(y) |>
        e_execute()

      proxy_chart(chart)
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

    # Update to line x
    new_values <- proxy_chart()$chart$x$opts$series[[1]]$data |> unlist() |> unname()

    expect_equal(new_values,c(1,3,9))
    # Proxy was called with no errors
    expect_true(proxy_called())

    expect_equal(
      proxy_chart()$chart$x$opts$series[[1]]$type,
      "line"
    )

    expect_error(echarts4rProxy("area", data = test_data) |>
                   e_area(), "must pass serie")
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
test_that("e_bar x and y index works", {
  plot <- iris |> e_chart(Species) |>
    e_bar(Sepal.Length,
                y_index = 10,
                x_index = 10
    )
  expect_gt(length(plot$x$opts$xAxis), 10)
  expect_gt(length(plot$x$opts$yAxis), 10)
})

test_that("e_bar timeline works", {

  plot <- iris |> dplyr::group_by(Species) |> e_chart(Species, timeline = TRUE) |>
    e_bar(Sepal.Length
    ) |> e_timeline_serie(
      title = list(
        list(text = "setosa"),
        list(text = "versicolor"),
        list(text = "virginica")
      )
    )
  expect_true(plot$x$tl)

  # Time series
  expect_equal(
    plot$x$opts$baseOption$timeline$data,
    list("setosa", "versicolor", "virginica")
  )

  expect_equal(plot$x$opts$baseOption$series[[1]]$type, "bar")
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
    proxy_chart <- shiny::reactiveVal(NULL)

    output$bar <- renderEcharts4r({
      test_data |>
        e_charts(x) |>
        e_bar(y, name = "Serie 1")
    })

    observeEvent(input$update, {
      chart <- echarts4rProxy("bar",
                     data = test_data) |>
        e_bar(y, name = "Serie 1") |>
        e_execute()

      proxy_chart(chart)
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

    # Update to line x
    new_values <- proxy_chart()$chart$x$opts$series[[1]]$data |> unlist() |> unname()
    expect_equal(new_values,c(1,3,9))

    # Proxy was called with no errors
    expect_true(proxy_called())

    expect_equal(
      proxy_chart()$chart$x$opts$series[[1]]$type,
      "bar"
    )
    expect_error(echarts4rProxy("bar", data = test_data) |>
      e_bar(), "must pass serie")
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

test_that("e_step timeline works", {

  plot <- iris |> dplyr::group_by(Species) |> e_chart(Species, timeline = TRUE) |>
    e_step(Sepal.Length
    ) |> e_timeline_serie(
      title = list(
        list(text = "setosa"),
        list(text = "versicolor"),
        list(text = "virginica")
      )
    )
  expect_true(plot$x$tl)

  # Time series
  expect_equal(
    plot$x$opts$baseOption$timeline$data,
    list("setosa", "versicolor", "virginica")
  )

  expect_equal(plot$x$opts$baseOption$series[[1]]$type, "line")
})
test_that("e_step x and y index works", {
  plot <- iris |> e_chart(Species) |>
    e_step(Sepal.Length,
           y_index = 10,
           x_index = 10
    )
  expect_gt(length(plot$x$opts$xAxis), 10)
  expect_gt(length(plot$x$opts$yAxis), 10)
})

test_that("e_step polar works", {

  plot <- iris |> e_chart(Species) |>
    e_polar() |>
    e_angle_axis(Species) |>
    e_radius_axis() |>
    e_step(Sepal.Length, coord_system = "polar")

  expect_length(plot$x$opts$polar, 1)

  expect_length(plot$x$opts$series[[1]]$data, nrow(iris))
  expect_equal(as.character(unlist(plot$x$opts$angleAxis$data)), c("setosa", "versicolor", "virginica"))

  expect_equal(plot$x$opts$series[[1]]$type, "line")
})

test_that("e_step.echarts4rProxy plot responds", {

  test_data <-  data.frame(
    x = seq(3),
    y = c(1, 3, 9),
    z = c(2, 5, 4),
    w = c(3, 4, 3)
  )

  server <- function(input, output, session) {
    proxy_called <- shiny::reactiveVal(FALSE)
    proxy_chart <- shiny::reactiveVal(NULL)

    output$step <- renderEcharts4r({
      test_data |>
        e_charts(x) |>
        e_step(y, name = "Serie 1")
    })

    observeEvent(input$update, {
      chart <- echarts4rProxy("step",
                              data = test_data) |>
        e_step(y, name = "Serie 1") |>
        e_execute()

      proxy_chart(chart)
      proxy_called(TRUE)
    })
  }

  shiny::testServer(server, {

    expect_false(proxy_called())
    json <- jsonlite::fromJSON(output$step)

    # Data matches - this is the first point
    expect_identical(
      json$x$opts$series$data[[1]]$value[[1]],
      c(1L, 1L))

    session$setInputs(update = 1)
    session$flushReact()

    # Update to line x
    new_values <- proxy_chart()$chart$x$opts$series[[1]]$data |> unlist() |> unname()
    expect_equal(new_values,c(1,3,9))

    # Proxy was called with no errors
    expect_true(proxy_called())

    expect_equal(
      proxy_chart()$chart$x$opts$series[[1]]$type,
      "line"
    )
    expect_error(echarts4rProxy("step", data = test_data) |>
                   e_step(), "must pass serie")
  })
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

test_that("e_scatter timeline works", {

  plot <- iris |> dplyr::group_by(Species) |> e_chart(Species, timeline = TRUE) |>
    e_scatter(Sepal.Length
    )
  expect_true(plot$x$tl)

  # Time series
  expect_equal(
    plot$x$opts$baseOption$timeline$data,
    list("setosa", "versicolor", "virginica")
  )

  expect_equal(plot$x$opts$baseOption$series[[1]]$type, "scatter")
})
test_that("e_scatter x and y index works", {
  plot <- iris |> e_chart(Species) |>
    e_scatter(Sepal.Length,
           y_index = 10,
           x_index = 10
    )
  expect_gt(length(plot$x$opts$xAxis), 10)
  expect_gt(length(plot$x$opts$yAxis), 10)
})

test_that("e_scatter.echarts4rProxy plot responds", {

  test_data <-  data.frame(
    x = seq(3),
    y = c(1, 3, 9),
    z = c(2, 5, 4),
    w = c(3, 4, 3)
  )

  server <- function(input, output, session) {
    proxy_called <- shiny::reactiveVal(FALSE)
    proxy_chart <- shiny::reactiveVal(NULL)

    output$scatter <- renderEcharts4r({
      test_data |>
        e_charts(x) |>
        e_scatter(y, name = "Serie 1")
    })

    observeEvent(input$update, {
      chart <- echarts4rProxy("scatter",
                              data = test_data) |>
        e_scatter(y, name = "Serie 1") |>
        e_execute()

      proxy_chart(chart)
      proxy_called(TRUE)
    })
  }

  shiny::testServer(server, {
    expect_false(proxy_called())
    json <- jsonlite::fromJSON(output$scatter)

    # Data matches - this is the first point
    expect_identical(
      json$x$opts$series$data[[1]]$value[[1]],
      c(1L, 1L))

    session$setInputs(update = 1)
    session$flushReact()

    # Update to line x
    new_values <- proxy_chart()$chart$x$opts$series[[1]]$data |> unlist() |> unname()
    expect_equal(new_values,c(1,3,9))

    # Proxy was called with no errors
    expect_true(proxy_called())

    expect_equal(
      proxy_chart()$chart$x$opts$series[[1]]$type,
      "scatter"
    )
    expect_error(echarts4rProxy("scatter", data = test_data) |>
                   e_scatter(), "must pass serie")
  })
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

test_that("e_effect_scatter timeline works", {

  plot <- iris |> dplyr::group_by(Species) |> e_chart(Species, timeline = TRUE) |>
    e_effect_scatter(Sepal.Length
    )
  expect_true(plot$x$tl)

  # Time series
  expect_equal(
    plot$x$opts$baseOption$timeline$data,
    list("setosa", "versicolor", "virginica")
  )

  expect_equal(plot$x$opts$baseOption$series[[1]]$type, "effectScatter")
})
test_that("e_effect_scatter x and y index works", {
  plot <- iris |> e_chart(Species) |>
    e_effect_scatter(Sepal.Length,
              y_index = 10,
              x_index = 10
    )
  expect_gt(length(plot$x$opts$xAxis), 10)
  expect_gt(length(plot$x$opts$yAxis), 10)
})
test_that("e_effect_scatter polar works", {

  plot <- iris |> e_chart(Species) |>
    e_polar() |>
    e_angle_axis(Species) |>
    e_radius_axis() |>
    e_effect_scatter(Sepal.Length, coord_system = "polar")

  expect_length(plot$x$opts$polar, 1)

  expect_length(plot$x$opts$series[[1]]$data, nrow(iris))
  expect_equal(as.character(unlist(plot$x$opts$angleAxis$data)), c("setosa", "versicolor", "virginica"))
})

test_that("e_effect_scatter singleAxis works", {

  plot <- iris |> e_chart(Species) |>
    e_single_axis() |>
    e_effect_scatter(Sepal.Length, coord_system = "singleAxis")

  expect_length(plot$x$opts$singleAxis, 2)

  expect_length(plot$x$opts$series[[1]]$data, nrow(iris))
  expect_equal(as.character(unlist(plot$x$opts$singleAxis$data)), c("setosa", "versicolor", "virginica"))
})

test_that("e_effect_scatter.echarts4rProxy plot responds", {

  test_data <-  data.frame(
    x = seq(3),
    y = c(1, 3, 9),
    z = c(2, 5, 4),
    w = c(3, 4, 3)
  )

  server <- function(input, output, session) {
    proxy_called <- shiny::reactiveVal(FALSE)
    proxy_chart <- shiny::reactiveVal(NULL)

    output$scatter <- renderEcharts4r({
      test_data |>
        e_charts(x) |>
        e_effect_scatter(y, name = "Serie 1")
    })

    observeEvent(input$update, {
      chart <- echarts4rProxy("scatter",
                              data = test_data) |>
        e_effect_scatter(y, name = "Serie 1") |>
        e_execute()

      proxy_chart(chart)
      proxy_called(TRUE)
    })
  }

  shiny::testServer(server, {
    expect_false(proxy_called())
    json <- jsonlite::fromJSON(output$scatter)

    # Data matches - this is the first point
    expect_identical(
      json$x$opts$series$data[[1]]$value[[1]],
      c(1L, 1L))

    session$setInputs(update = 1)
    session$flushReact()

    # Update to line x
    new_values <- proxy_chart()$chart$x$opts$series[[1]]$data |> unlist() |> unname()
    expect_equal(new_values,c(1,3,9))

    # Proxy was called with no errors
    expect_true(proxy_called())

    expect_equal(
      proxy_chart()$chart$x$opts$series[[1]]$type,
      "effectScatter"
    )
    expect_error(echarts4rProxy("scatter", data = test_data) |>
                   e_effect_scatter(), "must pass serie")
  })
})

test_that("e_effect_scatter.echarts4r and e_effect_scatter_ expects error when missing e", {
  expect_error(iris |> e_charts() |> e_effect_scatter.echarts4r(), "must pass serie")
  expect_error(e_effect_scatter.echarts4r(), "must pass e")

  expect_error(iris |> e_charts() |> e_effect_scatter_(),
               "must pass serie")
  expect_error( e_effect_scatter_(), "must pass e")
})

# e_scatter_3d ------------------------------------------------------------

test_that("e_scatter_3d plot has the good data structure and type", {
  df <- data.frame(
    x = seq(3),
    y = c(1, 3, 9),
    z = c(2, 5, 4)
  )
  plot <- df |>
    e_charts(x) |>
    e_scatter_3d(y, z)

  expect_s3_class(plot, "echarts4r")
  expect_s3_class(plot, "htmlwidget")

  expect_equal(
    plot$x$opts$series[[1]]$data,
    list(list(value = c(1, 1, 2)), list(value = c(2, 3, 5)), list(value = c(3, 9, 4)))
  )
  expect_equal(
    plot$x$opts$series[[1]]$type,
    "scatter3D"
  )

  plot2 <- df |>
    e_charts(x) |>
    e_scatter_3d(y, z) |>
    e_visual_map(z, scale = e_scale)

  expect_s3_class(plot2, "echarts4r")
  expect_s3_class(plot2, "htmlwidget")

  expect_equal(
    plot2$x$opts$series[[1]]$data,
    list(list(value = c(1, 1, 2)), list(value = c(2, 3, 5)), list(value = c(3, 9, 4)))
  )

  expect_equal(
    plot2$x$opts$series[[1]]$type,
    "scatter3D"
  )
})

test_that("e_scatter_3d timeline works", {

  plot <- iris |> dplyr::group_by(Species) |> e_chart(Species, timeline = TRUE) |>
    e_scatter_3d(Sepal.Length, Sepal.Width
    ) |> e_timeline_serie(
      title = list(
        list(text = "setosa"),
        list(text = "versicolor"),
        list(text = "virginica")
      )
    )
  expect_true(plot$x$tl)

  # Time series
  expect_equal(
    plot$x$opts$baseOption$timeline$data,
    list("setosa", "versicolor", "virginica")
  )

  expect_equal(plot$x$opts$baseOption$series[[1]]$type, "scatter3D")
})

test_that("e_scatter_3d.echarts4rProxy plot responds", {

  test_data <-  data.frame(
    x = seq(3),
    y = c(1, 3, 9),
    z = c(2, 5, 4),
    w = c(3, 4, 3)
  )

  server <- function(input, output, session) {
    proxy_called <- shiny::reactiveVal(FALSE)
    proxy_chart <- shiny::reactiveVal(NULL)

    output$scatter <- renderEcharts4r({
      test_data |>
        e_charts(x) |>
        e_scatter_3d(y, z, name = "Serie 1")
    })

    observeEvent(input$update, {
      chart <- echarts4rProxy("scatter",
                              data = test_data,
                              x = x) |>
        e_scatter_3d(y, y, name = "Serie 1") |>
        e_execute()

      proxy_chart(chart)
      proxy_called(TRUE)
    })
  }

  shiny::testServer(server, {
    expect_false(proxy_called())
    json <- jsonlite::fromJSON(output$scatter)

    # Data matches - this is the first point
    expect_identical(
      json$x$opts$series$data[[1]]$value[[1]],
      c(1L, 1L, 2L))

    session$setInputs(update = 1)
    session$flushReact()

    # Update to line x
    new_values <- proxy_chart()$chart$x$opts$series[[1]]$data |> unlist() |> unname()
    expect_equal(new_values,c(1, 1, 1, 2, 3, 3, 3, 9, 9))

    # Proxy was called with no errors
    expect_true(proxy_called())

    expect_equal(
      proxy_chart()$chart$x$opts$series[[1]]$type,
      "scatter3D"
    )
    expect_error(echarts4rProxy("scatter", data = test_data) |>
                   e_scatter_3d(), "must pass y and z")
  })
})

test_that("e_scatter_3d.echarts4r and e_scatter_3d_ expects error when missing e", {
  expect_error(iris |> e_charts() |> e_scatter_3d.echarts4r(), "must pass y and z")
  expect_error(e_scatter_3d.echarts4r(), "must pass e")

  expect_error(iris |> e_charts() |> e_scatter_3d_(),
               "must pass y and z")
  expect_error( e_scatter_3d_(), "must pass e")
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

test_that("e_polar timeline works", {

  plot <- iris |> dplyr::group_by(Species) |> e_chart(Species, timeline = TRUE) |>
    e_polar() |>
    e_angle_axis(x) |>
    e_radius_axis() |>
    e_bar(Sepal.Length, coord_system = "polar") |>
     e_timeline_serie(
      title = list(
        list(text = "setosa"),
        list(text = "versicolor"),
        list(text = "virginica")
      )
    )
  expect_true(plot$x$tl)

  # Time series
  expect_equal(
    plot$x$opts$baseOption$timeline$data,
    list("setosa", "versicolor", "virginica")
  )

  expect_equal(plot$x$opts$baseOption$series[[1]]$type, "bar")
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
    e_angle_axis_("x") |>
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

test_that("e_radius plot has the good data structure and type", {
  df <- data.frame(
    x = seq(3),
    y = c(1, 3, 9),
    z = c(2, 5, 4),
    w = c(3, 4, 3)
  )
  # plot <- df |>
  #   head(10) |>
  #   e_charts(x) |>
  #   e_radius_axis(x)

  plot2 <- df |>
    head(10) |>

    e_charts(x) |>
    e_polar() |>
    e_angle_axis() |>
    e_radius_axis(x) |>
    e_bar(y, coord_system = "polar") |>
    e_scatter_("z", coord_system = "polar")

  # expect_s3_class(plot, "echarts4r")
  # expect_s3_class(plot, "htmlwidget")

  expect_s3_class(plot2, "echarts4r")
  expect_s3_class(plot2, "htmlwidget")
})

# e_candle ----------------------------------------------------------------
test_that("e_candle plot has the good data structure and type", {
  date <- c(
    "2017-01-01",
    "2017-01-02",
    "2017-01-03",
    "2017-01-04",
    "2017-01-05",
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

# Timeline works, but data is on the wrong x-axis
test_that("e_candle timeline works", {

  date <- c(
    "2017-01-01",
    "2017-01-02",
    "2017-01-03",
    "2017-01-04",
    "2017-01-05",
    "2017-01-06",
    "2017-01-07"
  )

  stock <- data.frame(
    date = date,
    opening = c(200.60, 200.22, 198.43, 199.05, 203.54, 203.40, 208.34),
    closing = c(200.72, 198.85, 199.05, 203.73, 204.08, 208.11, 211.88),
    low = c(197.82, 198.07, 197.90, 198.10, 202.00, 201.50, 207.60),
    high = c(203.32, 200.67, 200.00, 203.95, 204.90, 208.44, 213.17),
    group = c("A", "A", "A", "B", "B", "B", "B")
  )
  plot <- stock |> dplyr::group_by(group) |> e_chart(date, timeline = TRUE) |> e_candle_("opening", "closing", "low", "high")
  expect_true(plot$x$tl)

  # Time series
  expect_equal(
    plot$x$opts$baseOption$timeline$data,
    list("A", "B")
  )

  expect_equal(plot$x$opts$baseOption$series[[1]]$type, "candlestick")
})

test_that("e_candle.echarts4rProxy plot responds", {

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

  server <- function(input, output, session) {
    proxy_called <- shiny::reactiveVal(FALSE)
    proxy_chart <- shiny::reactiveVal(NULL)

    output$candle <- renderEcharts4r({
      stock |>
        e_charts(date) |>
        e_candle(opening, closing, low, high) |>
        e_y_axis(min = 190, max = 220)
    })

    observeEvent(input$update, {
      chart <- echarts4rProxy("candle",
                              data = stock) |>
        e_candle(opening, opening, low, low, name = "Serie 1") |>
        e_execute()

      proxy_chart(chart)
      proxy_called(TRUE)
    })
  }

  shiny::testServer(server, {
    expect_false(proxy_called())
    json <- jsonlite::fromJSON(output$candle)

    data_points <- do.call(rbind, json$x$opts$series$data[[1]]$value) |> unname()
    expected_data_points <- stock[,2:5] |> as.matrix() |> unname()

    # Data matches
    expect_equal(data_points, expected_data_points)

    session$setInputs(update = 1)
    session$flushReact()

    # New data only has opening and low
    new_data_points <- proxy_chart()$chart$x$opts$series[[1]]$data |> unlist()
    new_expected_data_points <- stock[,c(2,4)] |> unlist()
    expect_all_true(new_data_points %in% new_expected_data_points)

    # Proxy was called with no errors
    expect_true(proxy_called())

    expect_equal(
      proxy_chart()$chart$x$opts$series[[1]]$type,
      "candlestick"
    )
    expect_error(echarts4rProxy("candle", data = stock) |>
                   e_candle(), "missing inputs")
  })
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

test_that("e_funnel timeline works", {

  plot <- iris |> dplyr::group_by(Species) |> e_chart(Species, timeline = TRUE) |>
    e_funnel(Petal.Width, Species
    ) |> e_timeline_serie(
      title = list(
        list(text = "setosa"),
        list(text = "versicolor"),
        list(text = "virginica")
      )
    )
  expect_true(plot$x$tl)

  # Time series
  expect_equal(
    plot$x$opts$baseOption$timeline$data,
    list("setosa", "versicolor", "virginica")
  )

  expect_equal(plot$x$opts$baseOption$series[[1]]$type, "funnel")
})

test_that("e_funnel.echarts4rProxy plot responds", {

  test_data <-  data.frame(
    stage = letters[1:3],
    y = c(1, 3, 9),
    z = c(2, 5, 4)
  )

  server <- function(input, output, session) {
    proxy_called <- shiny::reactiveVal(FALSE)
    proxy_chart <- shiny::reactiveVal(NULL)

    output$funnel <- renderEcharts4r({
      test_data |>
        e_charts() |>
        e_funnel(y, stage, name = "Serie 1")
    })

    observeEvent(input$update, {

      chart <- echarts4rProxy("funnel", data = test_data) |>
        e_funnel(z, stage) |>
        e_execute()

      proxy_chart(chart)
      proxy_called(TRUE)
    })
  }
  shiny::testServer(server, {
    expect_false(proxy_called())
    json <- jsonlite::fromJSON(output$funnel)

    expect_identical(
      json$x$opts$series$data[[1]]$value,
      c(1L, 3L, 9L))

    session$setInputs(update = 1)
    session$flushReact()

    # Update to line x
    new_values <- lapply(proxy_chart()$chart$x$opts$series[[1]]$data, \(x) x[[1]]) |> unlist()
    expect_equal(new_values,c(2L, 5L, 4L))

    # Proxy was called with no errors
    expect_true(proxy_called())

    expect_equal(
      proxy_chart()$chart$x$opts$series[[1]]$type,
      "funnel"
    )
    expect_error(echarts4rProxy("scatter", data = test_data) |>
                   e_funnel(), "missing values or labels")
  })
})

test_that("e_funnel.echarts4r and e_funnel_ expects error when missing e and values", {
  expect_error(iris |> e_charts() |> e_funnel.echarts4r() , "missing values or labels")
  expect_error(e_funnel.echarts4r(), "must pass e")

  expect_error(iris |> e_charts() |> e_funnel_(), "missing values or labels")
  expect_error(e_funnel_(), "must pass e")
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
  expect_error(sankey |> dplyr::group_by(source) |> e_chart(timeline = TRUE) |>
                 e_sankey(source, target, value) , "timeline not supported")

  expect_error(sankey |> dplyr::group_by(source) |> e_chart(timeline = TRUE) |>
                 e_sankey_("source", "target", "value") , "timeline not supported")
})

test_that("e_sankey.echarts4rProxy plot responds", {

  sankey <- data.frame(
    source = c("a", "b", "c", "d", "c"),
    target = c("b", "c", "d", "e", "e"),
    new_target = c("c", "c", "a", "c", "a"),
    value = ceiling(rnorm(5, 10, 1)),
    stringsAsFactors = FALSE
  )

  server <- function(input, output, session) {
    proxy_called <- shiny::reactiveVal(FALSE)
    proxy_chart <- shiny::reactiveVal(NULL)

    output$sankey <- renderEcharts4r({
      sankey |>
        e_charts() |>
        e_sankey(source, target, value)
    })

    observeEvent(input$update, {
      chart <- echarts4rProxy("sankey",
                              data = sankey) |>
        e_sankey(source, new_target, value) |>
        e_execute()

      proxy_chart(chart)
      proxy_called(TRUE)
    })
  }

  shiny::testServer(server, {
    expect_false(proxy_called())
    json <- jsonlite::fromJSON(output$sankey)

    data_points <- json$x$opts$series$links[[1]]$target
    expected_data_points <- sankey[["target"]]

    # Data matches
    expect_equal(data_points, expected_data_points)

    session$setInputs(update = 1)
    session$flushReact()

    # New data only
    new_data_points <- lapply(proxy_chart()$chart$x$opts$series[[1]]$links, \(x) x$target) |> unlist()
    new_expected_data_points <- sankey[["new_target"]]
    expect_all_true(new_data_points %in% new_expected_data_points)

    # Proxy was called with no errors
    expect_true(proxy_called())

    expect_equal(
      proxy_chart()$chart$x$opts$series[[1]]$type,
      "sankey"
    )
    expect_error(echarts4rProxy("sankey", data = iris) |>
                   e_sankey(), "missing source, target or values")
  })
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

test_that("e_heatmap timeline works", {

  set.seed(1)

  v <- LETTERS[1:5]

  matrix <- data.frame(
    x = sample(v, 5, replace = TRUE),
    y = sample(v, 5, replace = TRUE),
    z = rnorm(5, 10, 1),
    group = c("A", "A", "A", "A", "A"),
    stringsAsFactors = FALSE
  ) |>
    dplyr::group_by(x, y, group) |>
    dplyr::summarise(z = sum(z)) |>
    dplyr::ungroup()

  plot <- matrix |> dplyr::group_by(group) |> e_chart(x, timeline = TRUE) |> e_heatmap(y, z) |>
    e_visual_map(z) |>
    e_title("Heatmap")|> e_timeline_serie(
      title = list(
        list(text = "A")
      )
    )
  expect_true(plot$x$tl)

  # Time series
  expect_equal(
    plot$x$opts$baseOption$timeline$data,
    list("A")
  )

  expect_equal(plot$x$opts$baseOption$series[[1]]$type, "heatmap")
})

test_that("e_heatmap.echarts4rProxy plot responds", {

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

  server <- function(input, output, session) {
    proxy_called <- shiny::reactiveVal(FALSE)
    proxy_chart <- shiny::reactiveVal(NULL)

    output$line <- renderEcharts4r({
      matrix |>
        e_charts(x) |>
        e_heatmap(y, z)
    })

    observeEvent(input$update, {
      chart <- echarts4rProxy("line",
                              data = matrix,
                              x = x) |>
        e_heatmap(y, z) |>
        e_execute()
      proxy_chart(chart)

      proxy_called(TRUE)
    })
  }

  shiny::testServer(server, {

    expect_false(proxy_called())

    json <- jsonlite::fromJSON(output$line)
    expect_equal(
      do.call(rbind, json$x$opts$series$data[[1]]$value),
      unname(as.matrix( matrix[c("x", "y", "z")]))
    )

    session$setInputs(update = 1)
    session$flushReact()

    # Update to line x
    new_values <- proxy_chart()$chart$x$opts$series[[1]]$data[[1]]$value
    expected_values <- matrix[1,]  |> as.character()

    expect_equal(new_values[1], expected_values[1])
    expect_equal(new_values[2], expected_values[2])
    expect_equal(new_values[3] |> as.numeric() |> round(2), expected_values[3]  |> as.numeric() |> round(2))
    # Proxy was called with no errors
    expect_true(proxy_called())

    expect_equal(
      proxy_chart()$chart$x$opts$series[[1]]$type,
      "heatmap"
    )

    expect_error(echarts4rProxy("line", data = matrix) |>
                   e_heatmap(), "must pass y")
  })
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
e <- df |> dplyr::group_by(price) |> e_chart(timeline = TRUE)
  expect_error(e |>
                 e_parallel(source, target, value) , "timeline not supported")
  expect_error(e |>
                 e_parallel_("source", "target", "value") , "timeline not supported")
})

test_that("e_parallel_ plot has the good data structure and type", {
  df <- data.frame(
    price = c(2, 4, 3),
    amount = c(12, 3, 1),
    letter = LETTERS[1:3]
  )

  plot <- df |>
    e_charts() |>
    e_parallel_("price", "amount", "letter") |>
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

test_that("e_parallel.echarts4rProxy plot responds", {

  df <- data.frame(
    price = rnorm(5, 10),
    amount = rnorm(5, 15),
    letter = LETTERS[1:5]
  )
 server <- function(input, output, session) {
    proxy_called <- shiny::reactiveVal(FALSE)
    proxy_chart <- shiny::reactiveVal(NULL)

    output$line <- renderEcharts4r({
      df |>
        e_charts() |>
        e_parallel(price, amount, letter, opts = list(smooth = TRUE))
    })

    observeEvent(input$update, {
      chart <- echarts4rProxy("line", data = df) |>
        e_parallel(price, price, letter, opts = list(smooth = TRUE)) |>
        e_execute()

      proxy_chart(chart)
      proxy_called(TRUE)
    })
  }

  shiny::testServer(server, {

    expect_false(proxy_called())

    json <- jsonlite::fromJSON(output$line)
    expect_equal(
      json$x$opts$series$data[,1] |> as.numeric(),
      unname(as.matrix(df))[,1] |> as.numeric()
    )

    session$setInputs(update = 1)
    session$flushReact()

    # First point matches
    new_values <- proxy_chart()$chart$x$opts$series$data[[1]][1] |>
      unlist() |> as.numeric() |> round(3)
    expected_values <- df[1, c("price")] |> as.numeric() |> round(3)

    # Matches first data point - not using amount
    expect_equal(new_values, expected_values)
    # Proxy was called with no errors
    expect_true(proxy_called())

    expect_equal(
      proxy_chart()$chart$x$opts$series$type,
      "parallel"
    )
  })
})

test_that("e_parallel.echarts4r and e_parallel_ expects error when missing e and y", {
  expect_error( e_parallel.echarts4r(), "must pass e")
  expect_error(e_parallel_(), "must pass e")
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

test_that("e_pie timeline works", {

  plot <- iris |> dplyr::group_by(Species) |> e_chart(Sepal.Length, timeline = TRUE) |>
    e_pie(Sepal.Length
    ) |> e_timeline_serie(
      title = list(
        list(text = "setosa"),
        list(text = "versicolor"),
        list(text = "virginica")
      )
    )
  expect_true(plot$x$tl)

  # Time series
  expect_equal(
    plot$x$opts$baseOption$timeline$data,
    list("setosa", "versicolor", "virginica")
  )

  expect_equal(plot$x$opts$baseOption$series[[1]]$type, "pie")
})

test_that("e_pie.echarts4rProxy plot responds", {

    test_data <- shiny::reactiveVal(data.frame(
      name = c("A", "B"),
      value = c(40, 60),
      new_value = c(20, 80)
    ))

    server <- function(input, output, session) {
      proxy_called <- shiny::reactiveVal(FALSE)
      proxy_chart <- shiny::reactiveVal(NULL)

      output$pie <- renderEcharts4r({
        e_charts(test_data(), name) |>
          e_pie(value, radius = c("30%", "70%"))
      })

      observeEvent(input$update, {
        chart <- echarts4rProxy("pie",
                       data = test_data(), x = new_value) |>
          e_pie(new_value, radius = c("30%", "70%")) |>
          e_execute()

        proxy_chart(chart)
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

      # Update to new values
      new_values <- lapply(proxy_chart()$chart$x$opts$series[[1]]$data, \(x) x$value) |> unlist() |> unname()

      expect_identical(new_values, test_data()[["new_value"]])

      # Proxy was called with no errors
      expect_true(proxy_called())

      expect_equal(
        proxy_chart()$chart$x$opts$series[[1]]$type,
        "pie"
      )

      expect_error(echarts4rProxy("pie", data = test_data) |>
                         e_pie(), "must pass serie")

    })
})

test_that("e_pie.echarts4r.echarts4r and e_pie_ expects error when missing e and serie", {
   expect_error(iris |> e_charts() |> e_pie.echarts4r(), "must pass serie")
   expect_error(e_pie.echarts4r(), "must pass e")

   expect_error(iris |> e_charts() |> e_pie_() , "must pass serie")
   expect_error(e_pie_(), "must pass e")
})

test_that("e_pie plot has the good data structure and type", {
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
test_that("e_rosetype plot has the good data structure and type", {
  plot <- mtcars |>
    head(5) |>
    tibble::rownames_to_column("model") |>
    e_charts(model) |>
    e_pie(hp, roseType = "radius")

  expect_s3_class(plot, "echarts4r")
  expect_s3_class(plot, "htmlwidget")

  expect_equal(
    plot$x$opts$series[[1]]$data,
    list(list(value = c(110), name = c("Mazda RX4")), list(value = c(110), name = c("Mazda RX4 Wag")), list(value = c(93), name = c("Datsun 710")), list(value = c(110), name = c("Hornet 4 Drive")), list(value = c(175), name = c("Hornet Sportabout")))
  )
  expect_equal(
    plot$x$opts$series[[1]]$type,
    "pie"
  )
})

# e_sunburst --------------------------------------------------------------
test_that("e_sunburst plot with levels", {
  df <- tibble::tibble(
    name = c("earth", "mars", "venus"),
    value = c(30, 40, 30),
    # 1st level
    itemStyle = tibble::tibble(color = c(NA, "red", "blue")),
    # embedded styles, optional
    children = list(
      tibble::tibble(
        name = c("land", "ocean"),
        value = c(10, 20),
        # 2nd level
        children = list(
          tibble::tibble(name = c("forest", "river"), value = c(3, 7)),
          # 3rd level
          tibble::tibble(
            name = c("fish", "kelp"),
            value = c(10, 5),
            children = list(
              tibble::tibble(name = c("shark", "tuna"), value = c(2, 6)),
              # 4th level
              NULL # kelp
            )
          )
        )
      ),
      tibble::tibble(name = c("crater", "valley"), value = c(20, 20)),
      NULL # venus
    )
  )
  myStyles <- c(list(color = "green"), list(color = "magenta")) # custom styles defined
  myNames <- list(c("land", "river"), "crater") # names to style
  myLevels <- list(2, c(3, 4)) # hierarchical levels to style

  plot <- df |>
    e_charts() |>
    e_sunburst(myStyles, myNames, myLevels)

  expect_s3_class(plot, "echarts4r")
  expect_s3_class(plot, "htmlwidget")

  expect_equal(
    plot$x$opts$series[[1]]$type,
    "sunburst"
  )

  expect_error(df |> dplyr::group_by(name) |> e_chart(timeline = TRUE) |>
                 e_sunburst(myStyles, myNames, myLevels) , "timeline not supported")

})

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
  level_one_names <- lapply(seq_along(level_one), \(x) level_one[[x]]$name) |> unlist()
  expect_equal(level_one_names,  c("earth", "mars", "venus"))


  level_two <- plot$x$opts$series[[1]]$data[[1]]$children
  level_two_names <-
    lapply(seq_along(level_two), \(x) level_two[[x]]$name) |> unlist()
  expect_equal(level_two_names,  c("land", "ocean"))

  level_three <- plot$x$opts$series[[1]]$data[[1]]$children[[1]]$children
  level_three_names <-
    lapply(seq_along(level_three), \(x) level_three[[x]]$name) |> unlist()
  expect_equal(level_three_names,  c("forest", "river"))

  expect_equal(
    plot$x$opts$series[[1]]$type,
    "sunburst"
  )
})

test_that("e_sunburst.echarts4rProxy plot responds", {

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

  server <- function(input, output, session) {
    proxy_called <- shiny::reactiveVal(FALSE)
    proxy_chart <- shiny::reactiveVal(NULL)

    output$sun <- renderEcharts4r({
      df |>
        e_charts() |>
        e_sunburst()

    })

    observeEvent(input$update, {
      chart <- echarts4rProxy("sun",
                              data = df) |>
        e_sunburst() |>
        e_execute()

      proxy_chart(chart)
      proxy_called(TRUE)
    })
  }

  shiny::testServer(server, {

    expect_false(proxy_called())

    sun_json <- jsonlite::fromJSON(output$sun)

    # Data matches
    expect_identical(
      sun_json$x$opts$series$data[[1]]$value |> as.double(),
      df[["value"]])

    session$setInputs(update = 1)
    session$flushReact()

    # Update to new values
    new_values <- lapply(proxy_chart()$chart$x$opts$series[[1]]$data, \(x) x$value) |> unlist() |> unname()

    expect_identical(new_values |> as.double(), df[["value"]])

    # Proxy was called with no errors
    expect_true(proxy_called())

    expect_equal(
      proxy_chart()$chart$x$opts$series[[1]]$type,
      "sunburst"
    )

  })
})


test_that("e_sunburst.echarts4r and e_sunburst_ expects error when missing e", {
   expect_error(e_sunburst.echarts4r(), "must pass e")
   expect_error(e_sunburst_(), "must pass e")
})


# e_tree ------------------------------------------------------------------

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
  level_one_names <- lapply(seq_along(level_one), \(x) level_one[[x]]$name) |> unlist()
  expect_equal(level_one_names,  c("earth"))


  level_two <- plot$x$opts$series[[1]]$data[[1]]$children
  level_two_names <-
    lapply(seq_along(level_two), \(x) level_two[[x]]$name) |> unlist()
  expect_equal(level_two_names,  c("land", "ocean"))

  level_three <- plot$x$opts$series[[1]]$data[[1]]$children[[1]]$children
  level_three_names <-
    lapply(seq_along(level_three), \(x) level_three[[x]]$name) |> unlist()
  expect_equal(level_three_names,  c("forest", "river"))

  expect_equal(
    plot$x$opts$series[[1]]$type,
    "tree"
  )

  expect_error(tree |> dplyr::group_by(name) |> e_chart(timeline = TRUE) |>
                 e_tree() , "timeline not supported")

})

test_that("e_tree.echarts4rProxy plot responds", {

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
  server <- function(input, output, session) {
    proxy_called <- shiny::reactiveVal(FALSE)
    proxy_chart <- shiny::reactiveVal(NULL)

    output$area <- renderEcharts4r({
      tree |>
        e_charts() |>
        e_tree()
    })

    observeEvent(input$update, {
      new_tree <- tree |> dplyr::mutate(name = "new earth")
      chart <- echarts4rProxy("area",
                              data = new_tree) |>
        e_tree()

      proxy_chart(chart)
      proxy_called(TRUE)
    })
  }

  shiny::testServer(server, {

    expect_false(proxy_called())
    json <- jsonlite::fromJSON(output$area)

    # Data matches
    expect_identical(
      json$x$opts$series$data[[1]]$name,
      "earth")

    session$setInputs(update = 1)
    session$flushReact()

    # Update to line x
    new_values <- proxy_chart()$chart$x$opts$series[[1]]$data[[1]]$name

    expect_equal(new_values, "new earth")
    # Proxy was called with no errors
    expect_true(proxy_called())

    expect_equal(
      proxy_chart()$chart$x$opts$series[[1]]$type,
      "tree"
    )
  })
})
# e_treemap ---------------------------------------------------------------

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
  level_one_names <- lapply(seq_along(level_one), \(x) level_one[[x]]$name) |> unlist()
  expect_equal(level_one_names,  c("earth", "mars", "venus"))


  level_two <- plot$x$opts$series[[1]]$data[[1]]$children
  level_two_names <-
    lapply(seq_along(level_two), \(x) level_two[[x]]$name) |> unlist()
  expect_equal(level_two_names,  c("land", "ocean"))

  level_three <- plot$x$opts$series[[1]]$data[[1]]$children[[1]]$children
  level_three_names <-
    lapply(seq_along(level_three), \(x) level_three[[x]]$name) |> unlist()
  expect_equal(level_three_names,  c("forest", "river"))

  expect_equal(
    plot$x$opts$series[[1]]$type,
    "treemap"
  )
  expect_error(df |> dplyr::group_by(name) |> e_chart(timeline = TRUE) |>
                 e_treemap() , "timeline not supported")
})

test_that("e_treemap.echarts4rProxy plot responds", {

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

  server <- function(input, output, session) {
    proxy_called <- shiny::reactiveVal(FALSE)
    proxy_chart <- shiny::reactiveVal(NULL)

    output$sun <- renderEcharts4r({
      df |>
        e_charts() |>
        e_treemap()

    })

    observeEvent(input$update, {
      chart <- echarts4rProxy("sun",
                              data = df) |>
        e_treemap() |>
        e_execute()

      proxy_chart(chart)
      proxy_called(TRUE)
    })
  }

  shiny::testServer(server, {

    expect_false(proxy_called())

    sun_json <- jsonlite::fromJSON(output$sun)

    # Data matches
    expect_identical(
      sun_json$x$opts$series$data[[1]]$value |> as.double(),
      df[["value"]])

    session$setInputs(update = 1)
    session$flushReact()

    # Update to new values
    new_values <- lapply(proxy_chart()$chart$x$opts$series[[1]]$data, \(x) x$value) |> unlist() |> unname()

    expect_identical(new_values |> as.double(), df[["value"]])

    # Proxy was called with no errors
    expect_true(proxy_called())

    expect_equal(
      proxy_chart()$chart$x$opts$series[[1]]$type,
      "treemap"
    )

  })
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

  expect_error(river |> dplyr::group_by(dates) |> e_chart(timeline = TRUE) |>
                 e_river(apples) , "timeline not supported")
})

test_that("e_river.echarts4rProxy plot responds", {

  dates <- c("2020-10-08", "2020-10-09", "2020-10-10")

  river <- data.frame(
    dates = dates,
    apples = runif(length(dates)),
    bananas = runif(length(dates)),
    pears = runif(length(dates))
  )

  server <- function(input, output, session) {
    proxy_called <- shiny::reactiveVal(FALSE)
    proxy_chart <- shiny::reactiveVal(NULL)

    output$sun <- renderEcharts4r({
      river |>
        e_charts(dates) |>
        e_river(apples)
    })

    observeEvent(input$update, {

      chart <- echarts4rProxy("sun",
                              data = river,
                              x = dates) |>
        e_river(apples) |>
        e_river(bananas) |>
        e_execute()

      proxy_chart(chart)
      proxy_called(TRUE)
    })
  }

shiny::testServer(server, {

  expect_false(proxy_called())

  sun_json <- jsonlite::fromJSON(output$sun)

  # Data matches
  expect_equal(
    do.call(rbind, sun_json$x$opts$series$data)[,1:2],
    unname(as.matrix( river[c("dates", "apples")]))
  )

  session$setInputs(update = 1)
  session$flushReact()

  # Update to new values
  new_values <- lapply(proxy_chart()$chart$x$opts$series[[1]]$data, \(x) x[[2]]) |> unlist()

  # Last 3 added should be bananas
  last_river <- new_values[4:6] |> as.double() |> round(4)

  expect_identical(last_river , river[["bananas"]] |> round(4))

  # Proxy was called with no errors
  expect_true(proxy_called())

  expect_equal(
    proxy_chart()$chart$x$opts$series[[1]]$type,
    "themeRiver"
  )

  expect_error(echarts4rProxy("line", data = river) |>
                 e_river(), "must pass serie")

})
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

  year <- data.frame(date = dates, values = values, group = c("A", "A", "B", "B", "B"))

  plot <- year |>
    dplyr::group_by(group) |>
    e_charts(date, timeline = TRUE) |>
    e_heatmap(values, coord_system = "calendar") |>
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

test_that("e_gauge timeline works", {

    plot <- data.frame(time = 2015:2017) |>
    group_by(time) |>
    e_charts(timeline = TRUE) |>
    e_gauge(
      c(57, 23, 65),
      c("percent", "percentage", "cases")
    )
  expect_true(plot$x$tl)

  # Time series
  expect_equal(
    plot$x$opts$baseOption$timeline$data,
    list("2015", "2016", "2017")
  )

  expect_equal(plot$x$opts$baseOption$series[[1]]$type, "gauge")
})

test_that("e_gauge_ timeline works", {

  plot <- data.frame(time = 2015:2017) |>
    group_by(time) |>
    e_charts(timeline = TRUE) |>
    e_gauge_(
      c(57, 23, 65),
      c("percent", "percentage", "cases")
    )
  expect_true(plot$x$tl)

  # Time series
  expect_equal(
    plot$x$opts$baseOption$timeline$data,
    list("2015", "2016", "2017")
  )

  expect_equal(plot$x$opts$baseOption$series[[1]]$type, "gauge")
})

test_that("e_gauge_ plot has the good data structure and type", {
  plot <- e_charts() |>
    e_gauge_(41, "PERCENT") |>
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

test_that("e_gauge.echarts4rProxy plot responds", {

  server <- function(input, output, session) {
    proxy_called <- shiny::reactiveVal(FALSE)
    proxy_chart <- shiny::reactiveVal(NULL)

    output$line <- renderEcharts4r({
      e_charts() |>
        e_gauge(41, "PERCENT")
    })

    # This should not require data
    observeEvent(input$update, {
      chart <- echarts4rProxy("line", data = iris) |>
        e_gauge(value= 2) |>
        e_execute()
      proxy_chart(chart)

      proxy_called(TRUE)
    })
  }

  shiny::testServer(server, {

    expect_false(proxy_called())

    json <- jsonlite::fromJSON(output$line)
    expect_equal(
      json$x$opts$series$data[[1]]$value,
    41L )

    session$setInputs(update = 1)
    session$flushReact()

    new_values <- proxy_chart()$chart$x$opts$series[[1]]$data |> unlist() |> unname()

    expect_equal(new_values,2L)
    # Proxy was called with no errors
    expect_true(proxy_called())

    expect_equal(
      proxy_chart()$chart$x$opts$series[[1]]$type,
      "gauge"
    )
    expect_error(echarts4rProxy("radar", data = iris) |>
                   e_gauge(), "missing value")
  })
})

test_that("e_gauge expects error when missing e or range", {
  expect_error(iris |> e_charts() |> e_gauge.echarts4r() , "missing e or value")
  expect_error(e_gauge_(), "missing e or value")
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
  expect_error(df |> dplyr::group_by(x) |> e_chart(timeline = TRUE) |>
                 e_radar(y) , "timeline not supported")
})

test_that("e_radar.echarts4rProxy plot responds", {

  test_data <-  data.frame(
    x = seq(3),
    y = c(1, 3, 9),
    z = c(2, 5, 4),
    w = c(3, 4, 3)
  )

  server <- function(input, output, session) {
    proxy_called <- shiny::reactiveVal(FALSE)
    proxy_chart <- shiny::reactiveVal(NULL)

    output$radar <- renderEcharts4r({
      test_data |>
        e_charts(x) |>
        e_radar(y, name = "Serie 1")
    })

    observeEvent(input$update, {
      chart <- echarts4rProxy("radar",
                              data = test_data,
                              x = x) |>
        e_radar(z, name = "Serie 1") |>
        e_execute()

      proxy_chart(chart)
      proxy_called(TRUE)
    })
  }

  shiny::testServer(server, {

    expect_false(proxy_called())
    json <- jsonlite::fromJSON(output$radar)
    # Data matches - this is the first point
    expect_identical(
      json$x$opts$series$data[[1]]$value[[1]],
      c(1L, 3L, 9L))

    session$setInputs(update = 1)
    session$flushReact()

    # Update to line x
    new_values <- proxy_chart()$chart$x$opts$series[[1]]$data[[1]]$value |> unlist() |> unname()
    expect_equal(new_values,c(2L, 5L, 4L))

    # Proxy was called with no errors
    expect_true(proxy_called())

    expect_equal(
      proxy_chart()$chart$x$opts$series[[1]]$type,
      "radar"
    )
    expect_error(echarts4rProxy("radar", data = test_data) |>
                   e_radar(), "must pass serie")
  })
})

test_that("e_radar.echarts4r and e_radar_ expects error when missing e and serie", {
  expect_error(iris |> e_charts() |> e_radar.echarts4r() , "must pass serie")
  expect_error(e_radar.echarts4r(), "must pass e")

  expect_error(iris |> e_charts() |> e_radar_(), "must pass serie")
  expect_error(e_radar_(), "must pass e")
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
  expect_error(liquid |> dplyr::group_by(val) |> e_chart(timeline = TRUE) |>
                 e_liquid() , "timeline not supported")
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

test_that("e_mark_p timeline works", {

  grouped <- iris |> dplyr::group_by(Species)
  plot <- grouped |> e_chart(Species, timeline = TRUE) |>
    e_line(Sepal.Length) |>
    e_mark_p( type = "line",
              # serie_index = 1,
              data = list(
                     list(xAxis = "setosa", yAxis = 2),
                     list(xAxis = "setosa", yAxis = 4)
                   )
    ) |> e_timeline_serie(
      title = list(
        list(text = "setosa"),
        list(text = "versicolor"),
        list(text = "virginica")
      )
    )
  expect_true(plot$x$tl)

  # Time series
  expect_equal(
    plot$x$opts$baseOption$timeline$data,
    list("setosa", "versicolor", "virginica")
  )

  expect_equal(plot$x$opts$baseOption$series[[1]]$type, "line")
})

# e_lines_3d ---------------------------------------------------------------

test_that("e_lines_3d plot has the good data structure and type", {

  plot <- flights |>
    e_charts() |>
    e_lines_3d(
      start_lon,
      start_lat,
      end_lon,
      end_lat,
      name = "flights",
      effect = list(show = TRUE)
    )

  expect_s3_class(plot, "echarts4r")
  expect_s3_class(plot, "htmlwidget")

  start_lon_values <- lapply(plot$x$opts$series[[1]]$data, \(x) x$coords[[1]][1]) |> unlist()

  expect_identical(
    start_lon_values, flights[["start_lon"]]
  )
  expect_equal(plot$x$opts$series[[1]]$coordinateSystem, "globe")
  expect_equal(
    plot$x$opts$series[[1]]$type,
    "lines3D"
  )
})

test_that("e_lines_3d plot with source, target, value", {

  plot <- flights |>
    e_charts() |>
    e_globe() |>
    e_lines_3d(
      start_lon,
      start_lat,
      end_lon,
      end_lat,
      airport1,
      airport2,
      coord_system = "globe",
      cnt,
      name = "flights"
    ) |> suppressMessages()

  expect_s3_class(plot, "echarts4r")
  expect_s3_class(plot, "htmlwidget")
  expect_equal(plot$x$opts$series[[1]]$coordinateSystem, "globe")

  start_lon_values <- lapply(plot$x$opts$series[[1]]$data, \(x) x$coords[[1]][1]) |> unlist()

  expect_identical(
    start_lon_values, flights[["start_lon"]]
  )
  expect_equal(
    plot$x$opts$series[[1]]$type,
    "lines3D"
  )
})

test_that("e_lines_3d timeline works", {
  plot <- flights |> head(5) |> dplyr::group_by(airport1) |> e_chart( timeline = TRUE) |>
    e_globe() |>
    e_lines_3d(
      start_lon,
      start_lat,
      end_lon,
      end_lat,
      name = "flights",
      effect = list(show = TRUE)
    ) |> suppressMessages()

  expect_true(plot$x$tl)
  # Time series
  expect_equal(
    plot$x$opts$baseOption$timeline$data,
    list("DFW", "ORD", "SJU")
  )

  expect_equal(plot$x$opts$baseOption$series[[1]]$type, "lines3D")
})


test_that("e_lines_3d_ plot has the good data structure and type", {

  plot <- flights |>
    e_charts() |>
    # e_globe(
    #   displacementScale = 0.05
    # ) |>
    e_lines_3d_(
      "start_lon",
      "start_lat",
      "end_lon",
      "end_lat",
      name = "flights",
      effect = list(show = TRUE)
    )

  expect_s3_class(plot, "echarts4r")
  expect_s3_class(plot, "htmlwidget")

  start_lon_values <- lapply(plot$x$opts$series[[1]]$data, \(x) x$coords[[1]][1]) |> unlist()

  expect_identical(
    start_lon_values, flights[["start_lon"]]
  )
  expect_equal(
    plot$x$opts$series[[1]]$type,
    "lines3D"
  )
})
test_that("e_lines_3d expects error when missing e and serie", {
  expect_error(flights |> e_charts() |> e_lines_3d.echarts4r(), "missing coordinates")
  expect_error(e_lines_3d.echarts4r(), "must pass e")

  expect_error(flights |> e_charts() |> e_lines_3d_(), "missing coordinates")
  expect_error(e_lines_3d_(), "must pass e")
})

test_that("e_lines_3d.echarts4rProxy plot responds", {

  server <- function(input, output, session) {
    proxy_called <- shiny::reactiveVal(FALSE)
    proxy_chart <- shiny::reactiveVal(NULL)

    output$line <- renderEcharts4r({
      plot <- flights |>
        e_charts() |>
        # e_globe(
        #   displacementScale = 0.05
        # ) |>
        e_lines_3d(
          start_lon,
          start_lat,
          end_lon,
          end_lat,
          name = "flights",
          effect = list(show = TRUE)
        )
    })

    observeEvent(input$update, {

      chart <- echarts4rProxy("line",
                              data = flights) |>
        e_lines_3d(
          end_lon,
          end_lat,
          start_lon,
          start_lat,
          effect = list(show = TRUE)
        ) |>
        e_execute()
      proxy_chart(chart)
      proxy_called(TRUE)
    })
  }

  shiny::testServer(server, {

    expect_false(proxy_called())

    json <- jsonlite::fromJSON(output$line)

    session$setInputs(update = 1)
    session$flushReact()

    # Proxy was called with no errors
    expect_true(proxy_called())

    # These were turned to lat in the proxy
    new_start_lon_values <- lapply(proxy_chart()$chart$x$opts$series[[1]]$data, \(x) x$coords[[1]][1]) |> unlist()

    expect_identical(
      new_start_lon_values, (flights[["end_lon"]])
    )

    expect_equal(
      proxy_chart()$chart$x$opts$series[[1]]$type,
      "lines3D"
    )

    expect_error(echarts4rProxy("line", data = flights) |>
                   e_lines_3d(), "missing coordinates")
  })
})


# e_line_3d ---------------------------------------------------------------

test_that("e_line_3d plot has the good data structure and type", {
  test_data <-  data.frame(
    x = seq(3),
    y = c(1, 3, 9),
    z = c(2, 5, 4),
    w = c(3, 4, 3)
  )

  plot <- test_data |>
    e_charts(x) |>
    e_line_3d(y, z)


  expect_s3_class(plot, "echarts4r")
  expect_s3_class(plot, "htmlwidget")

  expect_equal(
    plot$x$opts$series[[1]]$data,
    list(list(value = c(1, 1, 2)), list(value = c(2, 3, 5)), list(value = c(3, 9, 4)))
  )

  expect_equal(
    plot$x$opts$series[[1]]$type,
    "line3D"
  )
})

test_that("e_line_3d timeline works", {
  test_data <-  data.frame(
    group = c("A", "A", "B", "B"),
    x = seq(4),
    y = c(1, 3, 9, 11),
    z = c(2, 5, 4, 2),
    w = c(3, 4, 3, 8)
  )
  plot <- test_data |>
    dplyr::group_by(group) |>
    e_charts(x, timeline = TRUE) |>
    e_line_3d(y, z)

  expect_true(plot$x$tl)

  # Time series
  expect_equal(
    plot$x$opts$baseOption$timeline$data,
    list("A", "B")
  )

  expect_equal(plot$x$opts$baseOption$series[[1]]$type, "line3D")
})

test_that("e_line_3d.echarts4rProxy plot responds", {

  test_data <-  data.frame(
    x = seq(3),
    y = c(1, 3, 9),
    z = c(2, 5, 4)
  )

  server <- function(input, output, session) {
    proxy_called <- shiny::reactiveVal(FALSE)
    proxy_chart <- shiny::reactiveVal(NULL)

    output$bar <- renderEcharts4r({
      test_data |>
        e_charts(x) |>
        e_line_3d(y, z)
    })

    observeEvent(input$update, {
      chart <- echarts4rProxy("bar",
                              data = test_data,
                              x = x) |>
        e_line_3d(y, y) |>
        e_execute()

      proxy_chart(chart)
      proxy_called(TRUE)
    })
  }

  shiny::testServer(server, {

    expect_false(proxy_called())
    json <- jsonlite::fromJSON(output$bar)

    # Data matches - this is the first point
    expect_identical(
      json$x$opts$series$data[[1]]$value[[1]],
      c(1L, 1L, 2L))

    session$setInputs(update = 1)
    session$flushReact()

    new_values <- proxy_chart()$chart$x$opts$series[[1]]$data |> unlist() |> unname()
    expect_equal(new_values, c(1, 1, 1, 2, 3, 3, 3, 9, 9))

    # Proxy was called with no errors
    expect_true(proxy_called())

    expect_equal(
      proxy_chart()$chart$x$opts$series[[1]]$type,
      "line3D"
    )
    expect_error(echarts4rProxy("bar", data = test_data) |>
                   e_line_3d(), "must pass y and z")
  })
})

test_that("e_line_3d expects error when missing e and serie", {
  expect_error(flights |> e_charts() |> e_line_3d.echarts4r(), "must pass y and z")
  expect_error(e_line_3d.echarts4r(), "must pass e")

  expect_error(flights |> e_charts() |> e_line_3d_(), "must pass y and z")
  expect_error(e_line_3d_(), "must pass e")
})

# e_bar_3d -------------------------------------------------------------------

test_that("e_bar_3d plot has the good data structure and type", {
  df <- data.frame(
    x = seq(3),
    y = c(1, 3, 9),
    z = c(2, 5, 4)
  )
  plot <- df |>
    e_charts(x) |>
    e_bar_3d(y, z, name = "Serie 1")

  expect_s3_class(plot, "echarts4r")
  expect_s3_class(plot, "htmlwidget")

  expect_equal(
    plot$x$opts$series[[1]]$data,
    list(list(value = c(1, 1, 2)), list(value = c(2, 3, 5)), list(value = c(3, 9, 4)))
  )
  expect_equal(
    plot$x$opts$series[[1]]$type,
    "bar3D"
  )
})

test_that("e_bar_3d timeline works", {
  test_data <-  data.frame(
    group = c("A", "A", "B", "B"),
    x = seq(4),
    y = c(1, 3, 9, 11),
    z = c(2, 5, 14, 12),
    w = c(3, 4, 3, 8)
  )
  plot <- test_data |>
    dplyr::group_by(group) |>
    e_charts(x, timeline = TRUE) |>
    e_bar_3d(y, z, coord_system = "cartesian3D")

  expect_true(plot$x$tl)

  # Time series
  expect_equal(
    plot$x$opts$baseOption$timeline$data,
    list("A", "B")
  )

  expect_equal(plot$x$opts$baseOption$series[[1]]$type, "bar3D")
})

test_that("e_bar_3d.echarts4rProxy plot responds", {

  test_data <-  data.frame(
    x = seq(3),
    y = c(1, 3, 9),
    z = c(2, 5, 4)
  )

  server <- function(input, output, session) {
    proxy_called <- shiny::reactiveVal(FALSE)
    proxy_chart <- shiny::reactiveVal(NULL)

    output$bar <- renderEcharts4r({
      test_data |>
        e_charts(x) |>
        e_bar_3d(y, z)
    })

    observeEvent(input$update, {
      chart <- echarts4rProxy("bar",
                              data = test_data,
                              x = x) |>
        e_bar_3d(y, y) |>
        e_execute()

      proxy_chart(chart)
      proxy_called(TRUE)
    })
  }

  shiny::testServer(server, {

    expect_false(proxy_called())
    json <- jsonlite::fromJSON(output$bar)

    # Data matches - this is the first point
    expect_identical(
      json$x$opts$series$data[[1]]$value[[1]],
      c(1L, 1L, 2L))

    session$setInputs(update = 1)
    session$flushReact()

    new_values <- proxy_chart()$chart$x$opts$series[[1]]$data |> unlist() |> unname()
    expect_equal(new_values, c(1, 1, 1, 2, 3, 3, 3, 9, 9))

    # Proxy was called with no errors
    expect_true(proxy_called())

    expect_equal(
      proxy_chart()$chart$x$opts$series[[1]]$type,
      "bar3D"
    )
    expect_error(echarts4rProxy("bar", data = test_data) |>
                   e_bar_3d(), "must pass y and z")
  })
})

test_that("e_bar_3d.echarts4r and e_bar_3d_ expects error when missing e or serie", {
  expect_error(iris |> e_charts() |> e_bar_3d.echarts4r(),
               "must pass y and z")

  expect_error(e_bar_3d.echarts4r(), "must pass e")

  expect_error(iris |> e_charts() |>e_bar_3d_(), "must pass y and z")
  expect_error(e_bar_3d_(), "must pass e")
})

# e_pictorial -------------------------------------------------------------

test_that("e_pictorial plot has the good data structure and type", {
  set.seed(1)
  y <- rnorm(10, 10, 2)
  df <- data.frame(
    x = 1:10,
    y = y,
    z = y - rnorm(10, 5, 1)
  )

  plot <- df |>
    e_charts(x) |>
    e_bar(z, barWidth = 10) |>
    e_pictorial(
      y,
      symbol = "rect",
      symbolRepeat = TRUE,
      z = -1,
      symbolSize = c(10, 4)
    )
  expect_s3_class(plot, "echarts4r")
  expect_s3_class(plot, "htmlwidget")

  expect_equal(
    plot$x$opts$series[[1]]$data[[1]]$value,
    df[1, c("x", "z")] |> as.numeric()
  )

  expect_equal(
    plot$x$opts$series[[2]]$type,
    "pictorialBar"
  )
})

test_that("e_pictorial timeline works", {

  set.seed(1)
  y <- rnorm(10, 10, 2)
  df <- data.frame(
    x = 1:10,
    y = y,
    z = y - rnorm(10, 5, 1),
    group = LETTERS[1:10]
  )

  plot <- df |> dplyr::group_by(group) |> e_chart(x, timeline = TRUE) |>
    e_bar(z, barWidth = 10) |>
    e_pictorial(
      y,
      symbol = "rect",
      symbolRepeat = TRUE,
      z = -1,
      symbolSize = c(10, 4)
    )
  expect_true(plot$x$tl)

  # Time series
  expect_equal(
    unlist(plot$x$opts$baseOption$timeline$data),
    LETTERS[1:10]
  )

  expect_equal(plot$x$opts$baseOption$series[[2]]$type, "pictorialBar")
})

test_that("e_density x and y index works", {
  y <- rnorm(10, 10, 2)
  df <- data.frame(
    x = 1:10,
    y = y,
    z = y - rnorm(10, 5, 1),
    group = LETTERS[1:10]
  )

  plot <- df |> e_chart(x) |>
    e_bar(z, barWidth = 10) |>
    e_pictorial(
      y,
      symbol = "rect",
      symbolRepeat = TRUE,
      z = -1,
      symbolSize = c(10, 4),
              y_index = 10,
              x_index = 10
    )
  expect_gt(length(plot$x$opts$xAxis), 10)
  expect_gt(length(plot$x$opts$yAxis), 10)
})

test_that("e_pictorial.echarts4rProxy plot responds", {

  set.seed(1)
  y <- rnorm(10, 10, 2)
  df <- data.frame(
    x = 1:10,
    y = y,
    z = y - rnorm(10, 5, 1)
  )

  server <- function(input, output, session) {
    proxy_called <- shiny::reactiveVal(FALSE)
    proxy_chart <- shiny::reactiveVal(NULL)

    output$radar <- renderEcharts4r({
      df |>
        e_charts(x) |>
        e_bar(z, barWidth = 10) |>
        e_pictorial(
          y,
          symbol = "rect",
          symbolRepeat = TRUE,
          z = -1,
          symbolSize = c(10, 4))
    })

    observeEvent(input$update, {
      chart <- echarts4rProxy("radar",
                              data = df) |>
        e_pictorial(z, z, name = "Serie 1") |>
        e_execute()

      proxy_chart(chart)
      proxy_called(TRUE)
    })
  }

    shiny::testServer(server, {

      expect_false(proxy_called())
      json <- jsonlite::fromJSON(output$radar)

      # Data matches - this is the first point
      expect_identical(
        json$x$opts$series[[1]][[1]]$value[[1]],
        df[1, c("x", "z")] |> as.numeric()
        )

      session$setInputs(update = 1)
      session$flushReact()

      new_values <- proxy_chart()$chart$x$opts$series[[1]]$data |> unlist() |> unname() |> unique()

      expect_all_true(new_values %in% df[["z"]])

      # Proxy was called with no errors
      expect_true(proxy_called())

      expect_equal(
        proxy_chart()$chart$x$opts$series[[1]]$type,
        "pictorialBar"
      )
      expect_error(echarts4rProxy("radar", data = df) |>
                     e_pictorial(), "must pass serie")
    })
})

test_that("e_pictorial.echarts4r and e_pictorial_ expects error when missing e and serie", {
  expect_error(iris |> e_charts() |> e_pictorial.echarts4r() , "must pass serie and symbol")
  expect_error(e_pictorial.echarts4r(), "must pass e")

  expect_error(iris |> e_charts() |> e_pictorial_(), "must pass serie and symbol")
  expect_error(e_pictorial_(), "must pass e")
})


# e_chord -----------------------------------------------------------------
test_that("e_chord plot has the good data structure and type", {
  sankey <- data.frame(
    source = c("a", "b", "c", "d", "c"),
    target = c("b", "c", "d", "e", "e"),
    value = ceiling(rnorm(5, 10, 1)),
    stringsAsFactors = FALSE
  )

  plot <- sankey |>
    e_charts() |>
    e_chord(source, target, value) |>
    e_title("Chord chart")

  expect_s3_class(plot, "echarts4r")
  expect_s3_class(plot, "htmlwidget")

  expect_equal(
    plot$x$opts$series[[1]]$data,
    list(list(name = c("a")), list(name = c("b")), list(name = c("c")), list(name = c("d")), list(name = c("e")))
  )
  expect_equal(
    plot$x$opts$series[[1]]$type,
    "chord"
  )
  expect_error(sankey |> dplyr::group_by(source) |> e_chart(timeline = TRUE) |>
                 e_chord(source, target, value) , "timeline not supported")
})

test_that("e_chord.echarts4rProxy plot responds", {

  sankey <- data.frame(
    source = c("a", "b", "c", "d", "c"),
    target = c("b", "c", "d", "e", "e"),
    new_target = c("c", "c", "a", "c", "a"),
    value = ceiling(rnorm(5, 10, 1)),
    stringsAsFactors = FALSE
  )

  server <- function(input, output, session) {
    proxy_called <- shiny::reactiveVal(FALSE)
    proxy_chart <- shiny::reactiveVal(NULL)

    output$sankey <- renderEcharts4r({
      sankey |>
        e_charts() |>
        e_chord(source, target, value)
    })

    observeEvent(input$update, {
      chart <- echarts4rProxy("sankey",
                              data = sankey) |>
        e_chord(source, new_target, value) |>
        e_execute()

      proxy_chart(chart)
      proxy_called(TRUE)
    })
  }

  shiny::testServer(server, {
    expect_false(proxy_called())
    json <- jsonlite::fromJSON(output$sankey)

    data_points <- json$x$opts$series$links[[1]]$target
    expected_data_points <- sankey[["target"]]

    # Data matches
    expect_equal(data_points, expected_data_points)

    session$setInputs(update = 1)
    session$flushReact()

    # New data only
    new_data_points <- lapply(proxy_chart()$chart$x$opts$series[[1]]$links, \(x) x$target) |> unlist()
    new_expected_data_points <- sankey[["new_target"]]
    expect_all_true(new_data_points %in% new_expected_data_points)

    # Proxy was called with no errors
    expect_true(proxy_called())

    expect_equal(
      proxy_chart()$chart$x$opts$series[[1]]$type,
      "chord"
    )
    expect_error(echarts4rProxy("sankey", data = iris) |>
                   e_chord(), "missing source, target or values")
  })
})

test_that("e_chord.echarts4r and e_chord_ expects error when missing e and values/labels", {
  expect_error(iris |> e_charts() |> e_chord.echarts4r() , "missing source, target or values")
  expect_error( e_chord.echarts4r(), "must pass e")

  expect_error( iris |> e_charts() |> e_chord_() , "missing source, target or values")
  expect_error( e_chord_(), "must pass e")
})


# e_lines -----------------------------------------------------------------
test_that("e_lines plot has the good data structure and type", {
  flights <- flights[1:5,]

  plot <- flights |>
    e_charts() |>
    e_geo() |>
    e_lines(
      start_lon,
      start_lat,
      end_lon,
      end_lat,
      name = "flights",
      lineStyle = list(normal = list(curveness = 0.3))
    )

  expect_s3_class(plot, "echarts4r")
  expect_s3_class(plot, "htmlwidget")

  # test that difference is near 0 because decimals are problematic
  difference <-
    unlist(plot$x$opts$series[[1]]$data) - unlist(list(
      list(coords = list(
        c(-97.03720, 32.89595),
        c(-106.60919, 35.04022)
      )),
      list(coords = list(
        c(-87.90446, 41.97960),
        c(-97.66987, 30.19453)
      )),
      list(coords = list(
        c(-97.03720, 32.89595),
        c(-72.68323, 41.93887)
      )),
      list(coords = list(
        c(-66.00183, 18.43942),
        c(-72.68323, 41.93887)
      )),
      list(coords = list(
        c(-97.03720, 32.89595),
        c(-86.75355, 33.56294)
      ))
    ))
  difference_test <- difference < 10^-5

  expect_true(unique(difference_test))
  expect_equal(
    plot$x$opts$series[[1]]$type,
    "lines"
  )
})

test_that("e_lines plot with source, target, value", {
  flights <- flights[1:5,]

  plot <- flights |>
    e_charts() |>
    e_geo() |>
    e_lines(
      start_lon,
      start_lat,
      end_lon,
      end_lat,
      source_name = airport1,
      target_name = airport2,
      value = cnt,
      name = "flights",
      lineStyle = list(normal = list(curveness = 0.3))
    )
  expect_s3_class(plot, "echarts4r")
  expect_s3_class(plot, "htmlwidget")
  first_result <- plot$x$opts$series[[1]]$data[[1]]

  expect_equal(first_result$source_name, flights[1, "airport1"])
  expect_equal(first_result$target_name, flights[1, "airport2"])
  expect_equal(first_result$value, flights[1, "cnt"])
})

test_that("e_lines.echarts4rProxy plot responds", {

  server <- function(input, output, session) {
    proxy_called <- shiny::reactiveVal(FALSE)
    proxy_chart <- shiny::reactiveVal(NULL)

    output$line <- renderEcharts4r({
      plot <- flights |>
        e_charts() |>
        e_geo() |>
        e_lines(
          start_lon,
          start_lat,
          end_lon,
          end_lat,
          name = "flights",
          effect = list(show = TRUE)
        )
    })

    observeEvent(input$update, {

      chart <- echarts4rProxy("line",
                              data = flights) |>
        e_lines(
          end_lon,
          end_lat,
          start_lon,
          start_lat,
          effect = list(show = TRUE)
        ) |>
        e_execute()
      proxy_chart(chart)
      proxy_called(TRUE)
    })
  }

  shiny::testServer(server, {

    expect_false(proxy_called())

    json <- jsonlite::fromJSON(output$line)

    session$setInputs(update = 1)
    session$flushReact()

    # Proxy was called with no errors
    expect_true(proxy_called())

    # These were turned to lat in the proxy
    new_start_lon_values <- lapply(proxy_chart()$chart$x$opts$series[[1]]$data, \(x) x$coords[[1]][1]) |> unlist()

    expect_identical(
      new_start_lon_values, (flights[["end_lon"]])
    )

    expect_equal(
      proxy_chart()$chart$x$opts$series[[1]]$type,
      "lines"
    )

    expect_error(echarts4rProxy("line", data = flights) |>
                   e_lines(), "missing coordinates")
  })
})

test_that("e_lines.echarts4r and e_lines_ expects error when missing e and coordinates", {
  expect_error(iris |> e_charts() |> e_lines.echarts4r(), "missing coordinates")
  expect_error(e_lines.echarts4r(), "must pass e")

  expect_error(iris |> e_charts() |> e_lines_(), "missing coordinates")
  expect_error(e_lines_() , "must pass e")
})

test_that("e_scale errors when not numeric", {
  expect_error(e_scale("x must be numeric"))
})



