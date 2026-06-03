# Area

Add area serie. Note that this is NOT an unique series type. Rather,
this function is a shorthand for using \`e_bar()\` with \`areaStyle =
list()\` enabled.

## Usage

``` r
e_area(
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

e_area_(
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
CO2 |>
  group_by(Plant) |>
  e_charts(conc) |>
  e_area(uptake) |>
  e_tooltip(trigger = "axis")

{"x":{"theme":"","tl":false,"draw":true,"renderer":"canvas","events":[],"buttons":[],"settings":{"crosstalk_key":null,"crosstalk_group":null},"opts":{"yAxis":[{"show":true}],"xAxis":[{"type":"value"}],"legend":{"data":["Qn1","Qn2","Qn3","Qc1","Qc3","Qc2","Mn3","Mn2","Mn1","Mc2","Mc3","Mc1"]},"series":[{"data":[{"value":[95,16]},{"value":[175,30.4]},{"value":[250,34.8]},{"value":[350,37.2]},{"value":[500,35.3]},{"value":[675,39.2]},{"value":[1000,39.7]}],"yAxisIndex":0,"xAxisIndex":0,"name":"Qn1","type":"line","coordinateSystem":"cartesian2d","areaStyle":[]},{"data":[{"value":[95,13.6]},{"value":[175,27.3]},{"value":[250,37.1]},{"value":[350,41.8]},{"value":[500,40.6]},{"value":[675,41.4]},{"value":[1000,44.3]}],"yAxisIndex":0,"xAxisIndex":0,"name":"Qn2","type":"line","coordinateSystem":"cartesian2d","areaStyle":[]},{"data":[{"value":[95,16.2]},{"value":[175,32.4]},{"value":[250,40.3]},{"value":[350,42.1]},{"value":[500,42.9]},{"value":[675,43.9]},{"value":[1000,45.5]}],"yAxisIndex":0,"xAxisIndex":0,"name":"Qn3","type":"line","coordinateSystem":"cartesian2d","areaStyle":[]},{"data":[{"value":[95,14.2]},{"value":[175,24.1]},{"value":[250,30.3]},{"value":[350,34.6]},{"value":[500,32.5]},{"value":[675,35.4]},{"value":[1000,38.7]}],"yAxisIndex":0,"xAxisIndex":0,"name":"Qc1","type":"line","coordinateSystem":"cartesian2d","areaStyle":[]},{"data":[{"value":[95,15.1]},{"value":[175,21]},{"value":[250,38.1]},{"value":[350,34]},{"value":[500,38.9]},{"value":[675,39.6]},{"value":[1000,41.4]}],"yAxisIndex":0,"xAxisIndex":0,"name":"Qc3","type":"line","coordinateSystem":"cartesian2d","areaStyle":[]},{"data":[{"value":[95,9.300000000000001]},{"value":[175,27.3]},{"value":[250,35]},{"value":[350,38.8]},{"value":[500,38.6]},{"value":[675,37.5]},{"value":[1000,42.4]}],"yAxisIndex":0,"xAxisIndex":0,"name":"Qc2","type":"line","coordinateSystem":"cartesian2d","areaStyle":[]},{"data":[{"value":[95,11.3]},{"value":[175,19.4]},{"value":[250,25.8]},{"value":[350,27.9]},{"value":[500,28.5]},{"value":[675,28.1]},{"value":[1000,27.8]}],"yAxisIndex":0,"xAxisIndex":0,"name":"Mn3","type":"line","coordinateSystem":"cartesian2d","areaStyle":[]},{"data":[{"value":[95,12]},{"value":[175,22]},{"value":[250,30.6]},{"value":[350,31.8]},{"value":[500,32.4]},{"value":[675,31.1]},{"value":[1000,31.5]}],"yAxisIndex":0,"xAxisIndex":0,"name":"Mn2","type":"line","coordinateSystem":"cartesian2d","areaStyle":[]},{"data":[{"value":[95,10.6]},{"value":[175,19.2]},{"value":[250,26.2]},{"value":[350,30]},{"value":[500,30.9]},{"value":[675,32.4]},{"value":[1000,35.5]}],"yAxisIndex":0,"xAxisIndex":0,"name":"Mn1","type":"line","coordinateSystem":"cartesian2d","areaStyle":[]},{"data":[{"value":[95,7.7]},{"value":[175,11.4]},{"value":[250,12.3]},{"value":[350,13]},{"value":[500,12.5]},{"value":[675,13.7]},{"value":[1000,14.4]}],"yAxisIndex":0,"xAxisIndex":0,"name":"Mc2","type":"line","coordinateSystem":"cartesian2d","areaStyle":[]},{"data":[{"value":[95,10.6]},{"value":[175,18]},{"value":[250,17.9]},{"value":[350,17.9]},{"value":[500,17.9]},{"value":[675,18.9]},{"value":[1000,19.9]}],"yAxisIndex":0,"xAxisIndex":0,"name":"Mc3","type":"line","coordinateSystem":"cartesian2d","areaStyle":[]},{"data":[{"value":[95,10.5]},{"value":[175,14.9]},{"value":[250,18.1]},{"value":[350,18.9]},{"value":[500,19.5]},{"value":[675,22.2]},{"value":[1000,21.9]}],"yAxisIndex":0,"xAxisIndex":0,"name":"Mc1","type":"line","coordinateSystem":"cartesian2d","areaStyle":[]}],"tooltip":{"trigger":"axis"}},"dispose":true},"evals":[],"jsHooks":[]}
# timeline
iris |>
  group_by(Species) |>
  e_charts(Sepal.Length, timeline = TRUE) |>
  e_area(Sepal.Width) |>
  e_tooltip(trigger = "axis")

{"x":{"theme":"","tl":true,"draw":true,"renderer":"canvas","events":[],"buttons":[],"settings":{"crosstalk_key":null,"crosstalk_group":null},"opts":{"baseOption":{"yAxis":[{"show":true}],"timeline":{"data":["setosa","versicolor","virginica"],"axisType":"category"},"xAxis":[{"type":"value"}],"legend":{"data":["Sepal.Width"]},"series":[{"name":"Sepal.Width","type":"line","yAxisIndex":0,"xAxisIndex":0,"coordinateSystem":"cartesian2d","areaStyle":[]}],"tooltip":{"trigger":"axis"}},"options":[{"series":[{"data":[{"value":[4.3,3]},{"value":[4.4,2.9]},{"value":[4.4,3]},{"value":[4.4,3.2]},{"value":[4.5,2.3]},{"value":[4.6,3.1]},{"value":[4.6,3.4]},{"value":[4.6,3.6]},{"value":[4.6,3.2]},{"value":[4.7,3.2]},{"value":[4.7,3.2]},{"value":[4.8,3.4]},{"value":[4.8,3]},{"value":[4.8,3.4]},{"value":[4.8,3.1]},{"value":[4.8,3]},{"value":[4.9,3]},{"value":[4.9,3.1]},{"value":[4.9,3.1]},{"value":[4.9,3.6]},{"value":[5,3.6]},{"value":[5,3.4]},{"value":[5,3]},{"value":[5,3.4]},{"value":[5,3.2]},{"value":[5,3.5]},{"value":[5,3.5]},{"value":[5,3.3]},{"value":[5.1,3.5]},{"value":[5.1,3.5]},{"value":[5.1,3.8]},{"value":[5.1,3.7]},{"value":[5.1,3.3]},{"value":[5.1,3.4]},{"value":[5.1,3.8]},{"value":[5.1,3.8]},{"value":[5.2,3.5]},{"value":[5.2,3.4]},{"value":[5.2,4.1]},{"value":[5.3,3.7]},{"value":[5.4,3.9]},{"value":[5.4,3.7]},{"value":[5.4,3.9]},{"value":[5.4,3.4]},{"value":[5.4,3.4]},{"value":[5.5,4.2]},{"value":[5.5,3.5]},{"value":[5.7,4.4]},{"value":[5.7,3.8]},{"value":[5.8,4]}],"yAxisIndex":0,"xAxisIndex":0}]},{"series":[{"data":[{"value":[4.9,2.4]},{"value":[5,2]},{"value":[5,2.3]},{"value":[5.1,2.5]},{"value":[5.2,2.7]},{"value":[5.4,3]},{"value":[5.5,2.3]},{"value":[5.5,2.4]},{"value":[5.5,2.4]},{"value":[5.5,2.5]},{"value":[5.5,2.6]},{"value":[5.6,2.9]},{"value":[5.6,3]},{"value":[5.6,2.5]},{"value":[5.6,3]},{"value":[5.6,2.7]},{"value":[5.7,2.8]},{"value":[5.7,2.6]},{"value":[5.7,3]},{"value":[5.7,2.9]},{"value":[5.7,2.8]},{"value":[5.8,2.7]},{"value":[5.8,2.7]},{"value":[5.8,2.6]},{"value":[5.9,3]},{"value":[5.9,3.2]},{"value":[6,2.2]},{"value":[6,2.9]},{"value":[6,2.7]},{"value":[6,3.4]},{"value":[6.1,2.9]},{"value":[6.1,2.8]},{"value":[6.1,2.8]},{"value":[6.1,3]},{"value":[6.2,2.2]},{"value":[6.2,2.9]},{"value":[6.3,3.3]},{"value":[6.3,2.5]},{"value":[6.3,2.3]},{"value":[6.4,3.2]},{"value":[6.4,2.9]},{"value":[6.5,2.8]},{"value":[6.6,2.9]},{"value":[6.6,3]},{"value":[6.7,3.1]},{"value":[6.7,3]},{"value":[6.7,3.1]},{"value":[6.8,2.8]},{"value":[6.9,3.1]},{"value":[7,3.2]}],"yAxisIndex":0,"xAxisIndex":0}]},{"series":[{"data":[{"value":[4.9,2.5]},{"value":[5.6,2.8]},{"value":[5.7,2.5]},{"value":[5.8,2.7]},{"value":[5.8,2.8]},{"value":[5.8,2.7]},{"value":[5.9,3]},{"value":[6,2.2]},{"value":[6,3]},{"value":[6.1,3]},{"value":[6.1,2.6]},{"value":[6.2,2.8]},{"value":[6.2,3.4]},{"value":[6.3,3.3]},{"value":[6.3,2.9]},{"value":[6.3,2.7]},{"value":[6.3,2.8]},{"value":[6.3,3.4]},{"value":[6.3,2.5]},{"value":[6.4,2.7]},{"value":[6.4,3.2]},{"value":[6.4,2.8]},{"value":[6.4,2.8]},{"value":[6.4,3.1]},{"value":[6.5,3]},{"value":[6.5,3.2]},{"value":[6.5,3]},{"value":[6.5,3]},{"value":[6.7,2.5]},{"value":[6.7,3.3]},{"value":[6.7,3.1]},{"value":[6.7,3.3]},{"value":[6.7,3]},{"value":[6.8,3]},{"value":[6.8,3.2]},{"value":[6.9,3.2]},{"value":[6.9,3.1]},{"value":[6.9,3.1]},{"value":[7.1,3]},{"value":[7.2,3.6]},{"value":[7.2,3.2]},{"value":[7.2,3]},{"value":[7.3,2.9]},{"value":[7.4,2.8]},{"value":[7.6,3]},{"value":[7.7,3.8]},{"value":[7.7,2.6]},{"value":[7.7,2.8]},{"value":[7.7,3]},{"value":[7.9,3.8]}],"yAxisIndex":0,"xAxisIndex":0}]}]},"dispose":true},"evals":[],"jsHooks":[]}
```
