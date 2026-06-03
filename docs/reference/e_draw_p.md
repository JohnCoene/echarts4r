# Draw

Draw the chart.

## Usage

``` r
e_draw_p(proxy)
```

## Arguments

- proxy:

  An echarts4r proxy as returned by
  [`echarts4rProxy`](https://echarts4r.john-coene.com/reference/echarts4r-shiny.md).

## Details

Useful if you set `draw` to `FALSE` in
[`e_charts`](https://echarts4r.john-coene.com/reference/init.md).

## Examples

``` r
if (FALSE) { # \dontrun{
library(shiny)

ui <- fluidPage(
  echarts4rOutput("chart"),
  actionButton("draw", "draw")
)

server <- function(input, output) {
  output$chart <- renderEcharts4r({
    mtcars |>
      e_charts(mpg, draw = FALSE) |>
      e_scatter(qsec) |>
      e_datazoom()
  })

  observeEvent(input$draw, {
    echarts4rProxy("chart") |>
      e_draw_p()
  })
}

if (interactive()) {
  shinyApp(ui, server)
}
} # }
```
