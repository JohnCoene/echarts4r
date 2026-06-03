# Mark

Mark points, lines, and areas with a proxy (\[echarts4rProxy()\]).

## Usage

``` r
e_mark_p(e, type, serie_index, data, ...)

e_mark_p_(e, type, serie_index, data = NULL, ...)
```

## Arguments

- e:

  An `echarts4r` object as returned by
  [`e_charts`](https://echarts4r.john-coene.com/reference/init.md) or a
  proxy as returned by
  [`echarts4rProxy`](https://echarts4r.john-coene.com/reference/echarts4r-shiny.md).

- type:

  Type of mark: 'point','line' or 'area', defaults to 'point'.

- serie_index:

  Single index of serie to mark on, defaults to 1. Proxy doesn't know
  series' names, so it only uses index.

- data:

  Location of point, line or area, defaults to NULL.

- ...:

  Any other option to pass, check See Also section.

## Details

Allows the three type of marks to work with \[echarts4rProxy()\]

## Examples

``` r
library(shiny)
library(dplyr)

ui <- fluidPage(
  fluidRow(
    column(3, actionButton("pxy", "Marks")),
    column(
      3,
      checkboxInput("tln", "Timeline", value = FALSE)
    )
  ),
  echarts4rOutput("plot")
)

server <- function(input, output) {
  data(EuStockMarkets)

  bb <- as.data.frame(EuStockMarkets) |>
    slice_head(n = 150) |>
    mutate(day = 1:n())

  output$plot <- renderEcharts4r({
    react()
  })

  observeEvent(input$pxy, {
    echarts4rProxy("plot", data = NULL) |>
      e_mark_p(
        type = "line",
        serie_index = 1,
        data = list(type = "average"),
        lineStyle = list(type = "dashed", color = "cyan")
      ) |>
      e_mark_p(
        serie_index = 2,
        data = list(
          xAxis = bb$day[60],
          yAxis = bb$SMI[60],
          value = "pnt"
        )
      ) |>
      e_mark_p(
        type = "line",
        serie_index = 2,
        data = list(
          list(xAxis = bb$day[10], yAxis = bb$SMI[10]),
          list(xAxis = bb$day[37], yAxis = bb$SMI[37])
        ),
        lineStyle = list(type = "solid", color = "yellow")
      ) |>
      e_mark_p(
        type = "area",
        serie_index = 1,
        data = list(
          list(xAxis = bb$day[95]),
          list(xAxis = bb$day[105])
        ),
        itemStyle = list(color = "lightblue"),
        label = list(formatter = "X-area", position = "middle")
      ) |>
      e_merge()
  })

  react <- eventReactive(input$tln, {
    tmp <- bb
    if (input$tln) tmp <- tmp |> group_by(day < 75)

    tmp |>
      e_charts(
        day,
        backgroundColor = "#181818",
        legend = list(textStyle = list(color = "#aaa")),
        timeline = input$tln
      ) |>
      e_y_axis(scale = TRUE, axisLabel = list(color = "#aaa")) |>
      e_line(CAC, symbol = "none", color = "#ff33b8") |>
      e_line(SMI, symbol = "none", color = "green")
  })
}
if (interactive()) {
  shinyApp(ui, server)
}
```
