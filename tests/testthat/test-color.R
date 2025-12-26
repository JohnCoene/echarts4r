
test_that("sets color when tl is FALSE", {
  e <- mtcars |>
    e_charts(drat) |>
    e_line(mpg) |>
    e_area(qsec)
  e$x$tl <- FALSE

  result <- e_color(e, color = c("red", "blue"), background = "#d3d3d3")

  expect_equal(result$x$opts$color, list("red", "blue"))
  expect_equal(result$x$opts$backgroundColor, "#d3d3d3")
})

test_that("sets color in baseOption when tl is TRUE", {
  df <- data.frame(x = 1:3, y = 4:6, grp = c("A", "A", "B"))
  e <- df |> dplyr::group_by(grp) |> e_charts(x, timeline = TRUE)
  e$x$tl <- TRUE

  result <- e_color(e, color = c("red", "blue"), background = "#d3d3d3")

  expect_equal(result$x$opts$baseOption$color, c("red", "blue"))
  expect_equal(result$x$opts$baseOption$backgroundColor, "#d3d3d3")
})

test_that("only sets color when background is NULL", {
  df <- data.frame(x = 1:3, y = 4:6)
  e <- df |> e_charts(x)
  e$x$tl <- FALSE

  result <- e_color(e, color = c("red", "blue"), background = NULL)

  expect_equal(result$x$opts$color, list("red", "blue"))
  expect_null(result$x$opts$backgroundColor)
})

test_that("only sets background when color is NULL", {
  df <- data.frame(x = 1:3, y = 4:6)
  e <- df |> e_charts(x)
  e$x$tl <- FALSE

  result <- e_color(e, color = NULL, background = "#d3d3d3")

  expect_null(result$x$opts$color)
  expect_equal(result$x$opts$backgroundColor, "#d3d3d3")
})

test_that("returns e unchanged when both color and background are NULL", {
  df <- data.frame(x = 1:3, y = 4:6)
  e <- df |> e_charts(x)
  e$x$tl <- FALSE

  result <- e_color(e, color = NULL, background = NULL)

  expect_null(result$x$opts$color)
  expect_null(result$x$opts$backgroundColor)
})

test_that("e_color.echarts4rProxy plot responds", {

  server <- function(input, output, session) {
    proxy_called <- shiny::reactiveVal(FALSE)
    proxy_chart <- shiny::reactiveVal(NULL)

    output$line <- renderEcharts4r({
      e <- mtcars |>
        e_charts(drat) |>
        e_line(mpg) |>
        e_area(qsec)
    })

    observeEvent(input$update, {
      chart <- echarts4rProxy("line") |>
        e_color(color = c("red", "blue"), background = "#d3d3d3") |>
        e_execute()
      proxy_chart(chart)
      proxy_called(TRUE)
      chart
    })
  }

  shiny::testServer(server, {

    expect_false(proxy_called())

    session$setInputs(update = 1)
    session$flushReact()

    # Proxy was called with no errors
    expect_true(proxy_called())
    expect_equal(proxy_chart()$chart$x$opts$color, list("red", "blue"))
    expect_equal(proxy_chart()$chart$x$opts$backgroundColor, "#d3d3d3")

    expect_s3_class(proxy_chart(), "echarts4rProxy")
  })
})
