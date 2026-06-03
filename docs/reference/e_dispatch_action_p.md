# Dispatch Action

Create your own proxies, essentially a wrapper around the [action
API](https://echarts.apache.org/en/api.html#action).

## Usage

``` r
e_dispatch_action_p(proxy, type, ...)
```

## Arguments

- proxy:

  An echarts4r proxy as returned by
  [`echarts4rProxy`](https://echarts4r.john-coene.com/reference/echarts4r-shiny.md).

- type:

  Type of action to dispatch, i.e.: `highlight`.

- ...:

  Named options.

## Examples

``` r
if (FALSE) { # \dontrun{

library(shiny)

ui <- fluidPage(
  fluidRow(
    column(8, echarts4rOutput("chart")),
    column(4, actionButton("zoom", "Zoom"))
  )
)

server <- function(input, output, session) {
  output$chart <- renderEcharts4r({
    cars |>
      e_charts(speed) |>
      e_scatter(dist) |>
      e_datazoom()
  })

  observe({
    req(input$zoom)

    echarts4rProxy("chart") |>
      e_dispatch_action_p("dataZoom", startValue = 1, endValue = 10)
  })
}

if (interactive()) {
  shinyApp(ui, server)
}
} # }
```
