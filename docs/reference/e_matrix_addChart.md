# Add new chart to matrix chart

Adds an already existing echart to your new matrix chart. Charts with
timelines are not supported.

## Usage

``` r
e_matrix_addChart(
  e,
  chart,
  coord,
  id = "chart1",
  legend = TRUE,
  legend_pos = "bottom",
  legend_space = "0%",
  margin_trbl = c(t = "5%", r = "5%", b = "15%", l = "5%")
)
```

## Arguments

- e:

  An `echarts4r` object as returned by
  [`e_charts`](https://echarts4r.john-coene.com/reference/init.md) or a
  proxy as returned by
  [`echarts4rProxy`](https://echarts4r.john-coene.com/reference/echarts4r-shiny.md).

- chart:

  An existing echart that you want to attach to your new matrix chart

- coord:

  X,Y Coordinate of matrix to place your new chart. Charts can cover
  multiple coordinates such as \`list(c(0,1),0)\`. See [Matrix
  Coordinates](https://echarts.apache.org/en/option.html#matrix.body.data.coord)
  for more information on matrix coordinates.

- id:

  Unique id value that will be added when chart is placed in the matrix.
  Defaults to \`"chart1"\`

- legend:

  Whether chart contains a legend. Defaults to `TRUE`.

- legend_pos:

  Position of the legend. One of "top", "right", "bottom", "left".
  Determines to which side the \`legend_space\` argument applies.

- legend_space:

  Space between legend and plot area. Supports integers(pixels) or
  strings(percent of parent cell).

- margin_trbl:

  Adjusts the size of the outside margin around the plotting area.
  Default is \`c(t = "15 integers(pixels) or strings(percent of parent
  cell) To change only e.g. two sides \`c("r" = 8, "l" = 8)\` could be
  used, other sides will use defaults.

## Examples

``` r
echart <- iris |>
group_by(Species) |>
  e_charts(Sepal.Length) |>
  e_line(Sepal.Width) |>
  e_tooltip(trigger = "axis")

e_matrix_raw(rows = 3, cols = 3, body = list(
itemStyle = list(borderWidth = 0))) |>
e_matrix_addChart(echart, coord = list(
   c(0,2),0), margin_trbl = c("b" = "20%"))

{"x":{"theme":"","tl":false,"draw":true,"renderer":"canvas","events":[],"buttons":[],"settings":{"crosstalk_key":null,"crosstalk_group":null},"opts":{"matrix":{"x":{"data":[null,null,null],"show":false},"y":{"data":[null,null,null],"show":false},"body":{"itemStyle":{"borderWidth":0}}},"yAxis":[{"show":true,"id":"chart1","gridId":"chart1"}],"xAxis":[{"type":"value","id":"chart1","gridId":"chart1"}],"series":[{"data":[{"value":[4.3,3]},{"value":[4.4,2.9]},{"value":[4.4,3]},{"value":[4.4,3.2]},{"value":[4.5,2.3]},{"value":[4.6,3.1]},{"value":[4.6,3.4]},{"value":[4.6,3.6]},{"value":[4.6,3.2]},{"value":[4.7,3.2]},{"value":[4.7,3.2]},{"value":[4.8,3.4]},{"value":[4.8,3]},{"value":[4.8,3.4]},{"value":[4.8,3.1]},{"value":[4.8,3]},{"value":[4.9,3]},{"value":[4.9,3.1]},{"value":[4.9,3.1]},{"value":[4.9,3.6]},{"value":[5,3.6]},{"value":[5,3.4]},{"value":[5,3]},{"value":[5,3.4]},{"value":[5,3.2]},{"value":[5,3.5]},{"value":[5,3.5]},{"value":[5,3.3]},{"value":[5.1,3.5]},{"value":[5.1,3.5]},{"value":[5.1,3.8]},{"value":[5.1,3.7]},{"value":[5.1,3.3]},{"value":[5.1,3.4]},{"value":[5.1,3.8]},{"value":[5.1,3.8]},{"value":[5.2,3.5]},{"value":[5.2,3.4]},{"value":[5.2,4.1]},{"value":[5.3,3.7]},{"value":[5.4,3.9]},{"value":[5.4,3.7]},{"value":[5.4,3.9]},{"value":[5.4,3.4]},{"value":[5.4,3.4]},{"value":[5.5,4.2]},{"value":[5.5,3.5]},{"value":[5.7,4.4]},{"value":[5.7,3.8]},{"value":[5.8,4]}],"yAxisIndex":0,"xAxisIndex":0,"name":"setosa","type":"line","coordinateSystem":"cartesian2d","xAxisId":"chart1","yAxisId":"chart1"},{"data":[{"value":[4.9,2.4]},{"value":[5,2]},{"value":[5,2.3]},{"value":[5.1,2.5]},{"value":[5.2,2.7]},{"value":[5.4,3]},{"value":[5.5,2.3]},{"value":[5.5,2.4]},{"value":[5.5,2.4]},{"value":[5.5,2.5]},{"value":[5.5,2.6]},{"value":[5.6,2.9]},{"value":[5.6,3]},{"value":[5.6,2.5]},{"value":[5.6,3]},{"value":[5.6,2.7]},{"value":[5.7,2.8]},{"value":[5.7,2.6]},{"value":[5.7,3]},{"value":[5.7,2.9]},{"value":[5.7,2.8]},{"value":[5.8,2.7]},{"value":[5.8,2.7]},{"value":[5.8,2.6]},{"value":[5.9,3]},{"value":[5.9,3.2]},{"value":[6,2.2]},{"value":[6,2.9]},{"value":[6,2.7]},{"value":[6,3.4]},{"value":[6.1,2.9]},{"value":[6.1,2.8]},{"value":[6.1,2.8]},{"value":[6.1,3]},{"value":[6.2,2.2]},{"value":[6.2,2.9]},{"value":[6.3,3.3]},{"value":[6.3,2.5]},{"value":[6.3,2.3]},{"value":[6.4,3.2]},{"value":[6.4,2.9]},{"value":[6.5,2.8]},{"value":[6.6,2.9]},{"value":[6.6,3]},{"value":[6.7,3.1]},{"value":[6.7,3]},{"value":[6.7,3.1]},{"value":[6.8,2.8]},{"value":[6.9,3.1]},{"value":[7,3.2]}],"yAxisIndex":0,"xAxisIndex":0,"name":"versicolor","type":"line","coordinateSystem":"cartesian2d","xAxisId":"chart1","yAxisId":"chart1"},{"data":[{"value":[4.9,2.5]},{"value":[5.6,2.8]},{"value":[5.7,2.5]},{"value":[5.8,2.7]},{"value":[5.8,2.8]},{"value":[5.8,2.7]},{"value":[5.9,3]},{"value":[6,2.2]},{"value":[6,3]},{"value":[6.1,3]},{"value":[6.1,2.6]},{"value":[6.2,2.8]},{"value":[6.2,3.4]},{"value":[6.3,3.3]},{"value":[6.3,2.9]},{"value":[6.3,2.7]},{"value":[6.3,2.8]},{"value":[6.3,3.4]},{"value":[6.3,2.5]},{"value":[6.4,2.7]},{"value":[6.4,3.2]},{"value":[6.4,2.8]},{"value":[6.4,2.8]},{"value":[6.4,3.1]},{"value":[6.5,3]},{"value":[6.5,3.2]},{"value":[6.5,3]},{"value":[6.5,3]},{"value":[6.7,2.5]},{"value":[6.7,3.3]},{"value":[6.7,3.1]},{"value":[6.7,3.3]},{"value":[6.7,3]},{"value":[6.8,3]},{"value":[6.8,3.2]},{"value":[6.9,3.2]},{"value":[6.9,3.1]},{"value":[6.9,3.1]},{"value":[7.1,3]},{"value":[7.2,3.6]},{"value":[7.2,3.2]},{"value":[7.2,3]},{"value":[7.3,2.9]},{"value":[7.4,2.8]},{"value":[7.6,3]},{"value":[7.7,3.8]},{"value":[7.7,2.6]},{"value":[7.7,2.8]},{"value":[7.7,3]},{"value":[7.9,3.8]}],"yAxisIndex":0,"xAxisIndex":0,"name":"virginica","type":"line","coordinateSystem":"cartesian2d","xAxisId":"chart1","yAxisId":"chart1"}],"grid":[{"id":"chart1","coordinateSystem":"matrix","coord":[[0,2],0],"left":"5","top":"5","right":"5","bottom":"20%"}],"legend":[{"data":["setosa","versicolor","virginica"],"coordinateSystem":"matrix","coord":[[0,2],0],"bottom":"0%"}]},"dispose":true},"evals":[],"jsHooks":[]}
```
