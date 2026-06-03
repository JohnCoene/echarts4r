# Line

Add line serie.

## Usage

``` r
e_line(
  e,
  serie,
  bind,
  name = NULL,
  legend = TRUE,
  y_index = 0,
  x_index = 0,
  coord_system = "cartesian2d",
  ...
)

e_line_(
  e,
  serie,
  bind = NULL,
  name = NULL,
  legend = TRUE,
  y_index = 0,
  x_index = 0,
  coord_system = "cartesian2d",
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

- bind:

  Binding between datasets, namely for use of
  [`e_brush`](https://echarts4r.john-coene.com/reference/e_brush.md).

- name:

  name of the serie.

- legend:

  Whether to add serie to legend.

- x_index, y_index:

  Indexes of x and y axis.

- coord_system:

  Coordinate system to plot against.

- ...:

  Any other option to pass, check See Also section.

## See also

[Additional
arguments](https://echarts.apache.org/en/option.html#series-line)

## Examples

``` r
iris |>
  group_by(Species) |>
  e_charts(Sepal.Length) |>
  e_line(Sepal.Width) |>
  e_tooltip(trigger = "axis")

{"x":{"theme":"","tl":false,"draw":true,"renderer":"canvas","events":[],"buttons":[],"settings":{"crosstalk_key":null,"crosstalk_group":null},"opts":{"yAxis":[{"show":true}],"xAxis":[{"type":"value"}],"legend":{"data":["setosa","versicolor","virginica"]},"series":[{"data":[{"value":[4.3,3]},{"value":[4.4,2.9]},{"value":[4.4,3]},{"value":[4.4,3.2]},{"value":[4.5,2.3]},{"value":[4.6,3.1]},{"value":[4.6,3.4]},{"value":[4.6,3.6]},{"value":[4.6,3.2]},{"value":[4.7,3.2]},{"value":[4.7,3.2]},{"value":[4.8,3.4]},{"value":[4.8,3]},{"value":[4.8,3.4]},{"value":[4.8,3.1]},{"value":[4.8,3]},{"value":[4.9,3]},{"value":[4.9,3.1]},{"value":[4.9,3.1]},{"value":[4.9,3.6]},{"value":[5,3.6]},{"value":[5,3.4]},{"value":[5,3]},{"value":[5,3.4]},{"value":[5,3.2]},{"value":[5,3.5]},{"value":[5,3.5]},{"value":[5,3.3]},{"value":[5.1,3.5]},{"value":[5.1,3.5]},{"value":[5.1,3.8]},{"value":[5.1,3.7]},{"value":[5.1,3.3]},{"value":[5.1,3.4]},{"value":[5.1,3.8]},{"value":[5.1,3.8]},{"value":[5.2,3.5]},{"value":[5.2,3.4]},{"value":[5.2,4.1]},{"value":[5.3,3.7]},{"value":[5.4,3.9]},{"value":[5.4,3.7]},{"value":[5.4,3.9]},{"value":[5.4,3.4]},{"value":[5.4,3.4]},{"value":[5.5,4.2]},{"value":[5.5,3.5]},{"value":[5.7,4.4]},{"value":[5.7,3.8]},{"value":[5.8,4]}],"yAxisIndex":0,"xAxisIndex":0,"name":"setosa","type":"line","coordinateSystem":"cartesian2d"},{"data":[{"value":[4.9,2.4]},{"value":[5,2]},{"value":[5,2.3]},{"value":[5.1,2.5]},{"value":[5.2,2.7]},{"value":[5.4,3]},{"value":[5.5,2.3]},{"value":[5.5,2.4]},{"value":[5.5,2.4]},{"value":[5.5,2.5]},{"value":[5.5,2.6]},{"value":[5.6,2.9]},{"value":[5.6,3]},{"value":[5.6,2.5]},{"value":[5.6,3]},{"value":[5.6,2.7]},{"value":[5.7,2.8]},{"value":[5.7,2.6]},{"value":[5.7,3]},{"value":[5.7,2.9]},{"value":[5.7,2.8]},{"value":[5.8,2.7]},{"value":[5.8,2.7]},{"value":[5.8,2.6]},{"value":[5.9,3]},{"value":[5.9,3.2]},{"value":[6,2.2]},{"value":[6,2.9]},{"value":[6,2.7]},{"value":[6,3.4]},{"value":[6.1,2.9]},{"value":[6.1,2.8]},{"value":[6.1,2.8]},{"value":[6.1,3]},{"value":[6.2,2.2]},{"value":[6.2,2.9]},{"value":[6.3,3.3]},{"value":[6.3,2.5]},{"value":[6.3,2.3]},{"value":[6.4,3.2]},{"value":[6.4,2.9]},{"value":[6.5,2.8]},{"value":[6.6,2.9]},{"value":[6.6,3]},{"value":[6.7,3.1]},{"value":[6.7,3]},{"value":[6.7,3.1]},{"value":[6.8,2.8]},{"value":[6.9,3.1]},{"value":[7,3.2]}],"yAxisIndex":0,"xAxisIndex":0,"name":"versicolor","type":"line","coordinateSystem":"cartesian2d"},{"data":[{"value":[4.9,2.5]},{"value":[5.6,2.8]},{"value":[5.7,2.5]},{"value":[5.8,2.7]},{"value":[5.8,2.8]},{"value":[5.8,2.7]},{"value":[5.9,3]},{"value":[6,2.2]},{"value":[6,3]},{"value":[6.1,3]},{"value":[6.1,2.6]},{"value":[6.2,2.8]},{"value":[6.2,3.4]},{"value":[6.3,3.3]},{"value":[6.3,2.9]},{"value":[6.3,2.7]},{"value":[6.3,2.8]},{"value":[6.3,3.4]},{"value":[6.3,2.5]},{"value":[6.4,2.7]},{"value":[6.4,3.2]},{"value":[6.4,2.8]},{"value":[6.4,2.8]},{"value":[6.4,3.1]},{"value":[6.5,3]},{"value":[6.5,3.2]},{"value":[6.5,3]},{"value":[6.5,3]},{"value":[6.7,2.5]},{"value":[6.7,3.3]},{"value":[6.7,3.1]},{"value":[6.7,3.3]},{"value":[6.7,3]},{"value":[6.8,3]},{"value":[6.8,3.2]},{"value":[6.9,3.2]},{"value":[6.9,3.1]},{"value":[6.9,3.1]},{"value":[7.1,3]},{"value":[7.2,3.6]},{"value":[7.2,3.2]},{"value":[7.2,3]},{"value":[7.3,2.9]},{"value":[7.4,2.8]},{"value":[7.6,3]},{"value":[7.7,3.8]},{"value":[7.7,2.6]},{"value":[7.7,2.8]},{"value":[7.7,3]},{"value":[7.9,3.8]}],"yAxisIndex":0,"xAxisIndex":0,"name":"virginica","type":"line","coordinateSystem":"cartesian2d"}],"tooltip":{"trigger":"axis"}},"dispose":true},"evals":[],"jsHooks":[]}
# timeline
iris |>
  group_by(Species) |>
  e_charts(Sepal.Length, timeline = TRUE) |>
  e_line(Sepal.Width) |>
  e_tooltip(trigger = "axis")

{"x":{"theme":"","tl":true,"draw":true,"renderer":"canvas","events":[],"buttons":[],"settings":{"crosstalk_key":null,"crosstalk_group":null},"opts":{"baseOption":{"yAxis":[{"show":true}],"timeline":{"data":["setosa","versicolor","virginica"],"axisType":"category"},"xAxis":[{"type":"value"}],"legend":{"data":["Sepal.Width"]},"series":[{"name":"Sepal.Width","type":"line","selectedMode":"single","yAxisIndex":0,"xAxisIndex":0,"coordinateSystem":"cartesian2d"}],"tooltip":{"trigger":"axis"}},"options":[{"series":[{"data":[{"value":[4.3,3]},{"value":[4.4,2.9]},{"value":[4.4,3]},{"value":[4.4,3.2]},{"value":[4.5,2.3]},{"value":[4.6,3.1]},{"value":[4.6,3.4]},{"value":[4.6,3.6]},{"value":[4.6,3.2]},{"value":[4.7,3.2]},{"value":[4.7,3.2]},{"value":[4.8,3.4]},{"value":[4.8,3]},{"value":[4.8,3.4]},{"value":[4.8,3.1]},{"value":[4.8,3]},{"value":[4.9,3]},{"value":[4.9,3.1]},{"value":[4.9,3.1]},{"value":[4.9,3.6]},{"value":[5,3.6]},{"value":[5,3.4]},{"value":[5,3]},{"value":[5,3.4]},{"value":[5,3.2]},{"value":[5,3.5]},{"value":[5,3.5]},{"value":[5,3.3]},{"value":[5.1,3.5]},{"value":[5.1,3.5]},{"value":[5.1,3.8]},{"value":[5.1,3.7]},{"value":[5.1,3.3]},{"value":[5.1,3.4]},{"value":[5.1,3.8]},{"value":[5.1,3.8]},{"value":[5.2,3.5]},{"value":[5.2,3.4]},{"value":[5.2,4.1]},{"value":[5.3,3.7]},{"value":[5.4,3.9]},{"value":[5.4,3.7]},{"value":[5.4,3.9]},{"value":[5.4,3.4]},{"value":[5.4,3.4]},{"value":[5.5,4.2]},{"value":[5.5,3.5]},{"value":[5.7,4.4]},{"value":[5.7,3.8]},{"value":[5.8,4]}]}]},{"series":[{"data":[{"value":[4.9,2.4]},{"value":[5,2]},{"value":[5,2.3]},{"value":[5.1,2.5]},{"value":[5.2,2.7]},{"value":[5.4,3]},{"value":[5.5,2.3]},{"value":[5.5,2.4]},{"value":[5.5,2.4]},{"value":[5.5,2.5]},{"value":[5.5,2.6]},{"value":[5.6,2.9]},{"value":[5.6,3]},{"value":[5.6,2.5]},{"value":[5.6,3]},{"value":[5.6,2.7]},{"value":[5.7,2.8]},{"value":[5.7,2.6]},{"value":[5.7,3]},{"value":[5.7,2.9]},{"value":[5.7,2.8]},{"value":[5.8,2.7]},{"value":[5.8,2.7]},{"value":[5.8,2.6]},{"value":[5.9,3]},{"value":[5.9,3.2]},{"value":[6,2.2]},{"value":[6,2.9]},{"value":[6,2.7]},{"value":[6,3.4]},{"value":[6.1,2.9]},{"value":[6.1,2.8]},{"value":[6.1,2.8]},{"value":[6.1,3]},{"value":[6.2,2.2]},{"value":[6.2,2.9]},{"value":[6.3,3.3]},{"value":[6.3,2.5]},{"value":[6.3,2.3]},{"value":[6.4,3.2]},{"value":[6.4,2.9]},{"value":[6.5,2.8]},{"value":[6.6,2.9]},{"value":[6.6,3]},{"value":[6.7,3.1]},{"value":[6.7,3]},{"value":[6.7,3.1]},{"value":[6.8,2.8]},{"value":[6.9,3.1]},{"value":[7,3.2]}]}]},{"series":[{"data":[{"value":[4.9,2.5]},{"value":[5.6,2.8]},{"value":[5.7,2.5]},{"value":[5.8,2.7]},{"value":[5.8,2.8]},{"value":[5.8,2.7]},{"value":[5.9,3]},{"value":[6,2.2]},{"value":[6,3]},{"value":[6.1,3]},{"value":[6.1,2.6]},{"value":[6.2,2.8]},{"value":[6.2,3.4]},{"value":[6.3,3.3]},{"value":[6.3,2.9]},{"value":[6.3,2.7]},{"value":[6.3,2.8]},{"value":[6.3,3.4]},{"value":[6.3,2.5]},{"value":[6.4,2.7]},{"value":[6.4,3.2]},{"value":[6.4,2.8]},{"value":[6.4,2.8]},{"value":[6.4,3.1]},{"value":[6.5,3]},{"value":[6.5,3.2]},{"value":[6.5,3]},{"value":[6.5,3]},{"value":[6.7,2.5]},{"value":[6.7,3.3]},{"value":[6.7,3.1]},{"value":[6.7,3.3]},{"value":[6.7,3]},{"value":[6.8,3]},{"value":[6.8,3.2]},{"value":[6.9,3.2]},{"value":[6.9,3.1]},{"value":[6.9,3.1]},{"value":[7.1,3]},{"value":[7.2,3.6]},{"value":[7.2,3.2]},{"value":[7.2,3]},{"value":[7.3,2.9]},{"value":[7.4,2.8]},{"value":[7.6,3]},{"value":[7.7,3.8]},{"value":[7.7,2.6]},{"value":[7.7,2.8]},{"value":[7.7,3]},{"value":[7.9,3.8]}]}]}],"legend":{"data":[null,null,null]}},"dispose":true},"evals":[],"jsHooks":[]}
```
