# Loading

Show or hide loading.

## Usage

``` r
e_show_loading(
  e,
  hide_overlay = TRUE,
  text = "loading",
  color = "#c23531",
  text_color = "#000",
  mask_color = "rgba(255, 255, 255, 0.8)",
  zlevel = 0
)

e_hide_loading(e)
```

## Arguments

- e:

  An `echarts4r` object as returned by
  [`e_charts`](https://echarts4r.john-coene.com/reference/init.md) or a
  proxy as returned by
  [`echarts4rProxy`](https://echarts4r.john-coene.com/reference/echarts4r-shiny.md).

- hide_overlay:

  Hides the white overlay that appears in shiny when a plot is
  recalculating.

- text:

  Text to display.

- color:

  Color of spinner.

- text_color:

  Color of text.

- mask_color:

  Color of mask.

- zlevel:

  Z level.

## Details

This only applies to Shiny.

## Examples

``` r
if (FALSE) { # \dontrun{

# no redraw
# no loading
library(shiny)
ui <- fluidPage(
  fluidRow(
    column(12, actionButton("update", "Update"))
  ),
  fluidRow(
    column(12, echarts4rOutput("plot"))
  )
)

server <- function(input, output) {
  data <- eventReactive(input$update, {
    data.frame(
      x = 1:10,
      y = rnorm(10)
    )
  })

  output$plot <- renderEcharts4r({
    data() |>
      e_charts(x) |>
      e_bar(y)
  })
}

if (interactive()) {
  shinyApp(ui, server)
}

# add loading
server <- function(input, output) {
  data <- eventReactive(input$update, {
    Sys.sleep(1) # sleep one second to show loading
    data.frame(
      x = 1:10,
      y = rnorm(10)
    )
  })

  output$plot <- renderEcharts4r({
    data() |>
      e_charts(x) |>
      e_bar(y) |>
      e_show_loading()
  })
}

if (interactive()) {
  shinyApp(ui, server)
}
} # }
```
