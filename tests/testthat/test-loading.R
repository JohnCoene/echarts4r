test_that("e_show_loading plot responds", {

  server <- function(input, output, session) {
    proxy_chart <- reactiveVal(NULL)
      data <- eventReactive(input$update, {
        data.frame(
          x = 1:10,
          y = rnorm(10)
        )
      })

      output$plot <- renderEcharts4r({
      chart <- data() |>
          e_charts(x) |>
          e_bar(y) |>
          e_show_loading(
            text = "TEST",
            color = "#FFF",
            text_color = "#FFF",
            mask_color = "#FFF",
            zlevel = 10
          )
      proxy_chart(chart)
      })
  }

  shiny::testServer(server, {
    expect_null(proxy_chart())
    session$setInputs(update = 1)
    session$flushReact()
    expect_true(proxy_chart()$x$loading)
    expect_true(proxy_chart()$x$hideWhite)

    opts <- proxy_chart()$x$loadingOpts

    expect_equal(opts$text, "TEST")
    expect_equal(opts$color, "#FFF")
    expect_equal(opts$textColor, "#FFF")
    expect_equal(opts$maskColor, "#FFF")
    expect_equal(opts$zlevel, 10)

    expect_s3_class(proxy_chart(), "echarts4r")
    expect_s3_class(proxy_chart(), "htmlwidget")

    chart_hide <- proxy_chart() |> e_hide_loading()

    expect_false(chart_hide$x$loading)
  })
})

