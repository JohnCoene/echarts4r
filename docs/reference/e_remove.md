# Remove Serie

Remove a serie by name or precising its index.

## Usage

``` r
e_remove_serie_p(proxy, serie_name = NULL, serie_index = NULL)

e_remove_serie(proxy, serie_name = NULL, serie_index = NULL)
```

## Arguments

- proxy:

  An echarts4r proxy as returned by
  [`echarts4rProxy`](https://echarts4r.john-coene.com/reference/echarts4r-shiny.md).

- serie_name:

  Name of serie to remove.

- serie_index:

  Index of serie to append to (starts from 0).

## Examples

``` r
library(shiny)

ui <- fluidPage(
  actionButton("rm", "Remove z serie"),
  echarts4rOutput("plot")
)

server <- function(input, output, session) {
  data <- data.frame(
    x = rnorm(10, 5, 3),
    y = rnorm(10, 50, 12),
    z = rnorm(10, 50, 5)
  )

  output$plot <- renderEcharts4r({
    data |>
      e_charts(x) |>
      e_scatter(y) |>
      e_scatter(z)
  })

  observeEvent(input$rm, {
    echarts4rProxy("plot") |>
      e_remove_serie_p(serie_name = "z")
  })
}
if (FALSE) { # \dontrun{
shinyApp(ui, server)
} # }
```
