# Append Proxy

Append data dynamically.

## Usage

``` r
e_append1_p(proxy, series_index = NULL, data, x, y, name = NULL)

e_append1_p_(proxy, series_index = NULL, data, x, y, name = NULL)

e_append2_p(
  proxy,
  series_index = NULL,
  data,
  x,
  y,
  z,
  scale = NULL,
  symbol_size = 1
)

e_append2_p_(
  proxy,
  series_index = NULL,
  data,
  x,
  y,
  z,
  scale = NULL,
  symbol_size = 1
)
```

## Arguments

- proxy:

  An echarts4r proxy as returned by
  [`echarts4rProxy`](https://echarts4r.john-coene.com/reference/echarts4r-shiny.md).

- series_index:

  Index of serie to append to (starts from 0).

- data:

  Data.frame containing data to append.

- x, y, z:

  Columns names to plot.

- name:

  if using \`bind\` with e.g \`e_scatter\` this can be used to supply
  the colname for the name attribute bind is mapping to

- scale:

  A scaling function as passed to
  [`e_scatter`](https://echarts4r.john-coene.com/reference/scatter.md).

- symbol_size:

  Multiplier of scaling function as in
  [`e_scatter`](https://echarts4r.john-coene.com/reference/scatter.md).

## Details

Currently not all types of series supported incremental rendering when
using appendData. Only these types of series support it:
[`e_scatter`](https://echarts4r.john-coene.com/reference/scatter.md) and
[`e_line`](https://echarts4r.john-coene.com/reference/e_line.md) of pure
echarts, and
[`e_scatter_3d`](https://echarts4r.john-coene.com/reference/e_scatter_3d.md),
and [`e_line_3d`](https://echarts4r.john-coene.com/reference/line3D.md)
of echarts-gl.

## Examples

``` r
if (FALSE) { # \dontrun{
library(shiny)

ui <- fluidPage(
  actionButton("add", "Add Data to y"),
  echarts4rOutput("plot"),
  h4("Brush"),
  verbatimTextOutput("selected"),
  h4("Legend select change"),
  verbatimTextOutput("legend")
)

server <- function(input, output, session) {
  data <- data.frame(x = rnorm(10, 5, 3), y = rnorm(10, 50, 12), z = rnorm(10, 5, 20))

  react <- eventReactive(input$add, {
    set.seed(sample(1:1000, 1))
    data.frame(x = rnorm(10, 5, 2), y = rnorm(10, 50, 10), z = rnorm(10, 5, 20))
  })

  output$plot <- renderEcharts4r({
    data |>
      e_charts(x) |>
      e_scatter(y, z, scale = NULL) |>
      e_scatter(z) |>
      e_brush()
  })

  observeEvent(input$add, {

    echarts4rProxy("plot") |>
      e_append2_p(0, react(), x, y, z)
  })

  output$selected <- renderPrint({
    input$plot_brush
  })

  output$legend <- renderPrint({
    input$plot_legend_change
  })
}

shinyApp(ui, server)
} # }
```
