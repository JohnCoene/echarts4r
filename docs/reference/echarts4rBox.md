# Box

Renders a data box in shiny.

## Usage

``` r
echarts4rBox(
  data,
  x,
  y,
  text = "",
  subtext = "",
  type = c("bar", "line", "scatter", "area", "step"),
  ...,
  color = "#ffffff",
  text_color = "#ffffff",
  background_color = "#293c55",
  step = c("start", "middle", "end"),
  title_args = list(),
  tooltip = list(trigger = "axis")
)
```

## Arguments

- data:

  A dataframe containing data to plot.

- x, y:

  Bare column name of variables to draw.

- text, subtext:

  Title and subtitle of box.

- type:

  Chart type to draw.

- ...:

  Additional arguments to pass to the serie.

- color:

  Color of chart in box.

- text_color:

  Color of text.

- background_color:

  Color of box.

- step:

  Step method, only used if `type = "step"`.

- title_args:

  Additional arguments to add to the title.

- tooltip:

  Tooltip to use.

## See also

[`renderEcharts4rBox`](https://echarts4r.john-coene.com/reference/renderEcharts4rBox.md),
[`echarts4rBoxOutput`](https://echarts4r.john-coene.com/reference/echarts4rBoxOutput.md)

## Examples

``` r
library(shiny)

ui <- fluidPage(
  fluidRow(
    column(3, echarts4rBoxOutput("box1"))
  )
)

server <- function(input, output) {
  output$box1 <- renderEcharts4rBox({
    echarts4rBox(cars, speed, dist, "Cars", type = "bar")
  })
}
if (FALSE) { # \dontrun{
shinyApp(ui, server)
} # }
```
