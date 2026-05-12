# Capture event

Add an event capture.

## Usage

``` r
e_capture(e, event)
```

## Arguments

- e:

  An `echarts4r` object as returned by
  [`e_charts`](https://echarts4r.john-coene.com/reference/init.md) or a
  proxy as returned by
  [`echarts4rProxy`](https://echarts4r.john-coene.com/reference/echarts4r-shiny.md).

- event:

  An event name from the [event
  documentation](https://echarts.apache.org/en/api.html#events).

## Details

Many events can be captured, however not all are integrated, you can
pass one that is not implemented with this function.

## Examples

``` r
if (FALSE) { # \dontrun{
# add datazoom
library(shiny)

ui <- fluidPage(
  echarts4rOutput("chart"),
  verbatimTextOutput("zoom")
)

server <- function(input, output) {
  output$chart <- renderEcharts4r({
    mtcars |>
      e_charts(mpg) |>
      e_scatter(qsec) |>
      e_datazoom() |>
      e_capture("datazoom")
  })

  output$zoom <- renderPrint({
    input$chart_datazoom
  })
}

if (interactive()) {
  shinyApp(ui, server)
}
} # }
```
