# Error bar

Add error bars.

## Usage

``` r
e_error_bar(
  e,
  lower,
  upper,
  name = NULL,
  legend = FALSE,
  y_index = 0,
  x_index = 0,
  coord_system = "cartesian2d",
  ...
)

e_error_bar_(
  e,
  lower,
  upper,
  name = NULL,
  legend = FALSE,
  y_index = 0,
  x_index = 0,
  coord_system = "cartesian2d",
  itemStyle = list(borderWidth = 1.5),
  renderer = "renderErrorBar2",
  ...
)
```

## Arguments

- e:

  An `echarts4r` object as returned by
  [`e_charts`](https://echarts4r.john-coene.com/reference/init.md) or a
  proxy as returned by
  [`echarts4rProxy`](https://echarts4r.john-coene.com/reference/echarts4r-shiny.md).

- lower, upper:

  Lower and upper error bands.

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

- itemStyle:

  mostly used for borderWidth, default 1.5

- renderer:

  mame of render function from renderers.js

## Examples

``` r
df <- data.frame(
  x = factor(c(1, 2)),
  y = c(1, 5),
  upper = c(1.1, 5.3),
  lower = c(0.8, 4.6)
)

df |>
  e_charts(x) |>
  e_bar(y) |>
  e_error_bar(lower, upper)

{"x":{"theme":"","tl":false,"draw":true,"renderer":"canvas","events":[],"buttons":[],"settings":{"crosstalk_key":null,"crosstalk_group":null},"opts":{"yAxis":[{"show":true}],"xAxis":[{"data":["1","2"],"type":"category","boundaryGap":true}],"legend":{"data":["y"]},"series":[{"data":[{"value":["1","1"]},{"value":["2","5"]}],"name":"y","type":"bar","yAxisIndex":0,"xAxisIndex":0,"coordinateSystem":"cartesian2d"},{"data":[{"value":["1","0.8","1.1"]},{"value":["2","4.6","5.3"]}],"name":"y","type":"custom","yAxisIndex":0,"xAxisIndex":0,"coordinateSystem":"cartesian2d","itemStyle":{"borderWidth":1.5},"renderItem":"sessionStorage.setItem('ErrorBar.oss','[\"\",\"\",\"1\"]'); renderErrorBar2","encode":{"x":0,"y":[1,2]},"z":3,"color":"black"}]},"dispose":true},"evals":["opts.series.1.renderItem"],"jsHooks":[]}
# timeline
df <- data.frame(
  x = factor(c(1, 1, 2, 2)),
  y = c(1, 5, 3, 4),
  step = factor(c(1, 2, 1, 2)),
  upper = c(1.1, 5.3, 3.3, 4.2),
  lower = c(0.8, 4.6, 2.4, 3.6)
)

df |>
  group_by(step) |>
  e_charts(x, timeline = TRUE) |>
  e_bar(y) |>
  e_error_bar(lower, upper)

{"x":{"theme":"","tl":true,"draw":true,"renderer":"canvas","events":[],"buttons":[],"settings":{"crosstalk_key":null,"crosstalk_group":null},"opts":{"baseOption":{"yAxis":[{"show":true}],"timeline":{"data":["1","2"],"axisType":"category"},"xAxis":[{"data":["1","2"],"type":"category","boundaryGap":true}],"legend":{"data":["y"]},"series":[{"name":"y","type":"bar","yAxisIndex":0,"xAxisIndex":0,"coordinateSystem":"cartesian2d"},{"type":"custom","yAxisIndex":0,"xAxisIndex":0,"coordinateSystem":"cartesian2d","itemStyle":{"borderWidth":1.5},"renderItem":"sessionStorage.setItem('ErrorBar.oss','[\"\",\"\",\"1\"]'); renderErrorBar2","encode":{"x":0,"y":[1,2]},"name":"y","z":3,"color":"black"}]},"options":[{"series":[{"data":[{"value":["1","1"]},{"value":["2","3"]}]},{"data":[{"value":["1","0.8","1.1"]},{"value":["2","2.4","3.3"]}]}]},{"series":[{"data":[{"value":["1","5"]},{"value":["2","4"]}]},{"data":[{"value":["1","4.6","5.3"]},{"value":["2","3.6","4.2"]}]}]}],"legend":{"data":[null,null]}},"dispose":true},"evals":["opts.baseOption.series.1.renderItem"],"jsHooks":[]}
```
