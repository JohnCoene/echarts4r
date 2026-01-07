# e_append1_p ------------------------------------------------------------
test_that("e_append1_p has no errors", {

  data <- data.frame(x = rnorm(10, 5, 3), y = rnorm(10, 50, 12), z = rnorm(10, 5, 20))

  server <- function(input, output, session) {
    proxy_called <- shiny::reactiveVal(FALSE)
    proxy_chart <- shiny::reactiveVal(NULL)

    output$line <- renderEcharts4r({
      data |>
        e_charts(x) |>
        e_line(z)
    })

    observeEvent(input$update, {

      new_df <- data.frame(new_x = rnorm(10, 5, 2), new_y = rnorm(10, 50, 10), new_z = rnorm(10, 5, 20))

      # z is the name
      chart <- echarts4rProxy("line") |>
        e_append1_p(
          series_index = 0,
          data = new_df,
          new_x, new_y, name = new_z)

      proxy_chart(chart)
      proxy_called(TRUE)
      chart
    })
  }

  # I dont know how to test new output data
  shiny::testServer(server, {

    expect_false(proxy_called())

    json <- jsonlite::fromJSON(output$line)

    result <- do.call(rbind, json$x$opts$series$data[[1]]$value) |> sort()

    expected_data <- as.matrix( data[c("x", "z")]) |> sort()

    expect_equal(
      result, expected_data
    )
    session$setInputs(update = 1)
    session$flushReact()
    # Proxy was called with no errors
    expect_true(proxy_called())
  })
})

test_that("e_append1_p errors informatively without echarts4rProxy", {
  expect_error(e_append1_p(list(), data = mtcars, x = mpg, y = wt), "must pass echarts4rProxy object")
})

# e_append1_p_ ------------------------------------------------------------
test_that("e_append1_p_ has no errors", {

  data <- data.frame(x = rnorm(10, 5, 3), y = rnorm(10, 50, 12), z = rnorm(10, 5, 20))

  server <- function(input, output, session) {
    proxy_called <- shiny::reactiveVal(FALSE)
    proxy_chart <- shiny::reactiveVal(NULL)

    output$line <- renderEcharts4r({
      data |>
        e_charts(x) |>
        e_line(z)
    })

    observeEvent(input$update, {

      new_df <- data.frame(new_x = rnorm(10, 5, 2), new_y = rnorm(10, 50, 10), new_z = rnorm(10, 5, 20))

      # z is the name
      chart <- echarts4rProxy("line") |>
        e_append1_p_(
          series_index = 0,
          data = new_df,
          "new_x", "new_y", name = "new_z")

      proxy_chart(chart)
      proxy_called(TRUE)
      chart
    })
  }

  # I dont know how to test new output data
  shiny::testServer(server, {

    expect_false(proxy_called())

    json <- jsonlite::fromJSON(output$line)

    result <- do.call(rbind, json$x$opts$series$data[[1]]$value) |> sort()

    expected_data <- as.matrix( data[c("x", "z")]) |> sort()

    expect_equal(
      result, expected_data
    )
    session$setInputs(update = 1)
    session$flushReact()
    # Proxy was called with no errors
    expect_true(proxy_called())
  })
})
test_that("e_append1_p_ errors informatively without echarts4rProxy", {
  expect_error(e_append1_p_(list(), data = mtcars, x = "mpg", y = "wt"), "must pass echarts4rProxy object")
})


# e_append2_p ------------------------------------------------------------
test_that("e_append2_p has no errors", {

  data <- data.frame(x = rnorm(10, 5, 3), y = rnorm(10, 50, 12), z = rnorm(10, 5, 20))

  server <- function(input, output, session) {
    proxy_called <- shiny::reactiveVal(FALSE)
    proxy_chart <- shiny::reactiveVal(NULL)

    output$line <- renderEcharts4r({
      data |>
        e_charts(x) |>
        e_line(z)
    })

    observeEvent(input$update, {

      new_df <- data.frame(new_x = rnorm(10, 5, 2), new_y = rnorm(10, 50, 10), new_z = rnorm(10, 5, 20))

      # z is the name
      chart <- echarts4rProxy("line") |>
        e_append2_p(
          series_index = 0,
          data = new_df,
          new_x, new_y, new_z)

      proxy_chart(chart)
      proxy_called(TRUE)
      chart
    })
  }

  # I dont know how to test new output data
  shiny::testServer(server, {

    expect_false(proxy_called())

    json <- jsonlite::fromJSON(output$line)

    result <- do.call(rbind, json$x$opts$series$data[[1]]$value) |> sort()

    expected_data <- as.matrix( data[c("x", "z")]) |> sort()

    expect_equal(
      result, expected_data
    )
    session$setInputs(update = 1)
    session$flushReact()
    # Proxy was called with no errors
    expect_true(proxy_called())
  })
})

test_that("e_append2_p errors informatively without echarts4rProxy", {
  expect_error(e_append2_p(list(), data = mtcars, x = mpg, y = wt), "must pass echarts4rProxy object")
})

# e_append1_p_ ------------------------------------------------------------
test_that("e_append2_p_ has no errors", {

  data <- data.frame(x = rnorm(10, 5, 3), y = rnorm(10, 50, 12), z = rnorm(10, 5, 20))

  server <- function(input, output, session) {
    proxy_called <- shiny::reactiveVal(FALSE)
    proxy_chart <- shiny::reactiveVal(NULL)

    output$line <- renderEcharts4r({
      data |>
        e_charts(x) |>
        e_line(z)
    })

    observeEvent(input$update, {

      new_df <- data.frame(new_x = rnorm(10, 5, 2), new_y = rnorm(10, 50, 10), new_z = rnorm(10, 5, 20))

      # z is the name
      chart <- echarts4rProxy("line") |>
        e_append2_p_(
          series_index = 0,
          data = new_df,
          "new_x", "new_y", "new_z")

      proxy_chart(chart)
      proxy_called(TRUE)
      chart
    })
  }

  # I dont know how to test new output data
  shiny::testServer(server, {

    expect_false(proxy_called())

    json <- jsonlite::fromJSON(output$line)

    result <- do.call(rbind, json$x$opts$series$data[[1]]$value) |> sort()

    expected_data <- as.matrix( data[c("x", "z")]) |> sort()

    expect_equal(
      result, expected_data
    )
    session$setInputs(update = 1)
    session$flushReact()
    # Proxy was called with no errors
    expect_true(proxy_called())
  })
})

test_that("e_append2_p_ errors informatively without echarts4rProxy", {
  expect_error(e_append2_p_(list(), data = mtcars, x = "mpg", y = "wt"), "must pass echarts4rProxy object")
})


# e_remove_serie_p --------------------------------------------------------
test_that("e_append2_p_ has no errors", {

  data <- data.frame(x = rnorm(10, 5, 3), y = rnorm(10, 50, 12), z = rnorm(10, 5, 20))

  server <- function(input, output, session) {
    proxy_called <- shiny::reactiveVal(FALSE)
    proxy_chart <- shiny::reactiveVal(NULL)

    output$line <- renderEcharts4r({
      data |>
        e_charts(x) |>
        e_line(y) |>
        e_line(z)
    })

    observeEvent(input$update, {
      chart <- echarts4rProxy("line") |>
        e_remove_serie_p(serie_name = "z")

      proxy_chart(chart)
      proxy_called(TRUE)
      chart
    })
  }

  # I dont know how to test new output data
  shiny::testServer(server, {

    expect_false(proxy_called())

    json <- jsonlite::fromJSON(output$line)

    result <- do.call(rbind, json$x$opts$series$data[[1]]$value) |> sort()

    expected_data <- as.matrix( data[c("x", "y")]) |> sort()

    expect_equal(
      result, expected_data
    )
    session$setInputs(update = 1)
    session$flushReact()
    # Proxy was called with no errors
    expect_true(proxy_called())
  })
})

test_that("e_remove_serie_p errors informatively without echarts4rProxy", {
  e <- e_charts()
  expect_error(e |>  e_remove_serie_p(), "Must define `serie_index` or `serie_name`")

  expect_error(e_execute(),"missing proxy")
  expect_error(e_merge(),"missing proxy")
})

test_that("e_merge has no errors", {

  data <- data.frame(x = rnorm(10, 5, 3), y = rnorm(10, 50, 12), z = rnorm(10, 5, 20))

  server <- function(input, output, session) {
    proxy_called <- shiny::reactiveVal(FALSE)
    proxy_chart <- shiny::reactiveVal(NULL)

    output$line <- renderEcharts4r({
      data |>
        e_charts(x)

    })

    observeEvent(input$update, {
      chart <- echarts4rProxy("line", data = data) |>
        e_line(y) |>
        e_line(z) |> e_merge() |> e_execute()

      proxy_chart(chart)
      proxy_called(TRUE)
      chart
    })
  }

  # I dont know how to test new output data
  shiny::testServer(server, {

    expect_false(proxy_called())

    json <- jsonlite::fromJSON(output$line)
    session$setInputs(update = 1)
    session$flushReact()

    # Proxy was called with no errors
    expect_true(proxy_called())
  })
})

