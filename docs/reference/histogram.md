# Histogram & Density

Add a histogram or density plots.

## Usage

``` r
e_histogram(
  e,
  serie,
  breaks = "Sturges",
  name = "histogram",
  legend = TRUE,
  bar_width = "99%",
  x_index = 0,
  y_index = 0,
  ...
)

e_density(
  e,
  serie,
  breaks = "Sturges",
  name = NULL,
  legend = TRUE,
  x_index = 0,
  y_index = 0,
  smooth = TRUE,
  ...
)

e_histogram_(
  e,
  serie,
  breaks = "Sturges",
  name = "histogram",
  legend = TRUE,
  bar_width = "90%",
  x_index = 0,
  y_index = 0,
  ...
)

e_density_(
  e,
  serie,
  breaks = "Sturges",
  name = NULL,
  legend = TRUE,
  x_index = 0,
  y_index = 0,
  smooth = TRUE,
  ...
)
```

## Arguments

- e:

  An `echarts4r` object as returned by
  [`e_charts`](https://echarts4r.john-coene.com/reference/init.md) or a
  proxy as returned by
  [`echarts4rProxy`](https://echarts4r.john-coene.com/reference/echarts4r-shiny.md).

- serie:

  Column name of serie to plot.

- breaks:

  Passed to [`hist`](https://rdrr.io/r/graphics/hist.html).

- name:

  name of the serie.

- legend:

  Whether to add serie to legend.

- bar_width:

  Width of bars.

- x_index, y_index:

  Indexes of x and y axis.

- ...:

  Any other option to pass, check See Also section.

- smooth:

  Whether to use smoothed lines, passed to
  [`e_line`](https://echarts4r.john-coene.com/reference/e_line.md).

## See also

[Additional arguments for
histogram](https://echarts.apache.org/en/option.html#series-bar),
[Additional arguments for
density](https://echarts.apache.org/en/option.html#series-line)

## Examples

``` r
mtcars |>
  e_charts() |>
  e_histogram(mpg, name = "histogram") |>
  e_density(mpg, areaStyle = list(opacity = .4), smooth = TRUE, name = "density", y_index = 1) |>
  e_tooltip(trigger = "axis")

{"x":{"theme":"","tl":false,"draw":true,"renderer":"canvas","events":[],"buttons":[],"settings":{"crosstalk_key":null,"crosstalk_group":null},"opts":{"yAxis":[{"show":true},{"type":"value"}],"xAxis":[{"type":"value","scale":true}],"legend":{"data":["histogram","density"]},"series":[{"data":[[12.5,6],[17.5,12],[22.5,8],[27.5,2],[32.5,4]],"name":"histogram","type":"bar","barWidth":"99%","yAxisIndex":0,"xAxisIndex":0,"stack":"stackedHistogram"},{"name":"density","type":"line","yAxisIndex":1,"xAxisIndex":0,"smooth":true,"areaStyle":{"opacity":0.4},"data":[[12.5,0.0375],[17.5,0.075],[22.5,0.05],[27.5,0.0125],[32.5,0.025]]}],"tooltip":{"trigger":"axis"}},"dispose":true},"evals":[],"jsHooks":[]}
# timeline
mtcars |>
  group_by(cyl) |>
  e_charts(timeline = TRUE) |>
  e_histogram(mpg, name = "histogram") |>
  e_density(mpg, name = "density", y_index = 1)

{"x":{"theme":"","tl":true,"draw":true,"renderer":"canvas","events":[],"buttons":[],"settings":{"crosstalk_key":null,"crosstalk_group":null},"opts":{"baseOption":{"yAxis":[{"show":true},{"type":"value"}],"timeline":{"data":["4","6","8"],"axisType":"category"},"xAxis":[{"type":"value","scale":true}],"legend":{"data":["histogram","density"]},"series":[{"name":"histogram","type":"bar","barWidth":"99%","yAxisIndex":0,"xAxisIndex":0,"stack":"stackedHistogram"},{"name":"density","type":"line","yAxisIndex":1,"xAxisIndex":0,"smooth":true,"areaStyle":[]}]},"options":[{"series":[{"data":[[21,2],[23,2],[25,2],[27,1],[29,0],[31,2],[33,2]]},{"data":[[21,0.09090909090909091],[23,0.09090909090909091],[25,0.09090909090909091],[27,0.04545454545454546],[29,0],[31,0.09090909090909091],[33,0.09090909090909091]]}]},{"series":[{"data":[[17.5,1],[18.5,1],[19.5,2],[20.5,2],[21.5,1]]},{"data":[[17.5,0.1428571428571428],[18.5,0.1428571428571428],[19.5,0.2857142857142857],[20.5,0.2857142857142857],[21.5,0.1428571428571428]]}]},{"series":[{"data":[[11,2],[13,1],[15,7],[17,2],[19,2]]},{"data":[[11,0.07142857142857142],[13,0.03571428571428571],[15,0.25],[17,0.07142857142857142],[19,0.07142857142857142]]}]}]},"dispose":true},"evals":[],"jsHooks":[]}
```
