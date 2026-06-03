# Highlight & Downplay

Highlight series

## Usage

``` r
e_highlight(e, series_index = NULL, series_name = NULL, btn = NULL)

e_downplay(e, series_index = NULL, series_name = NULL, btn = NULL)
```

## Arguments

- e:

  An `echarts4r` object as returned by
  [`e_charts`](https://echarts4r.john-coene.com/reference/init.md) or a
  proxy as returned by
  [`echarts4rProxy`](https://echarts4r.john-coene.com/reference/echarts4r-shiny.md).

- series_index, series_name:

  Index or name of serie to highlight or `list` or `vector` of series.

- btn:

  A [`e_button`](https://echarts4r.john-coene.com/reference/e_button.md)
  id.

## Examples

``` r
iris |>
  group_by(Species) |>
  e_charts(Sepal.Length) |>
  e_line(Sepal.Width) |>
  e_line(Petal.Length) |>
  e_highlight(series_name = "setosa") # highlight group

{"x":{"theme":"","tl":false,"draw":true,"renderer":"canvas","events":[{"data":{"type":"highlight","seriesName":["setosa"]}}],"buttons":[],"settings":{"crosstalk_key":null,"crosstalk_group":null},"opts":{"yAxis":[{"show":true}],"xAxis":[{"type":"value"}],"legend":{"data":["setosa","versicolor","virginica","setosa","versicolor","virginica"]},"series":[{"data":[{"value":[4.3,3]},{"value":[4.4,2.9]},{"value":[4.4,3]},{"value":[4.4,3.2]},{"value":[4.5,2.3]},{"value":[4.6,3.1]},{"value":[4.6,3.4]},{"value":[4.6,3.6]},{"value":[4.6,3.2]},{"value":[4.7,3.2]},{"value":[4.7,3.2]},{"value":[4.8,3.4]},{"value":[4.8,3]},{"value":[4.8,3.4]},{"value":[4.8,3.1]},{"value":[4.8,3]},{"value":[4.9,3]},{"value":[4.9,3.1]},{"value":[4.9,3.1]},{"value":[4.9,3.6]},{"value":[5,3.6]},{"value":[5,3.4]},{"value":[5,3]},{"value":[5,3.4]},{"value":[5,3.2]},{"value":[5,3.5]},{"value":[5,3.5]},{"value":[5,3.3]},{"value":[5.1,3.5]},{"value":[5.1,3.5]},{"value":[5.1,3.8]},{"value":[5.1,3.7]},{"value":[5.1,3.3]},{"value":[5.1,3.4]},{"value":[5.1,3.8]},{"value":[5.1,3.8]},{"value":[5.2,3.5]},{"value":[5.2,3.4]},{"value":[5.2,4.1]},{"value":[5.3,3.7]},{"value":[5.4,3.9]},{"value":[5.4,3.7]},{"value":[5.4,3.9]},{"value":[5.4,3.4]},{"value":[5.4,3.4]},{"value":[5.5,4.2]},{"value":[5.5,3.5]},{"value":[5.7,4.4]},{"value":[5.7,3.8]},{"value":[5.8,4]}],"yAxisIndex":0,"xAxisIndex":0,"name":"setosa","type":"line","coordinateSystem":"cartesian2d"},{"data":[{"value":[4.9,2.4]},{"value":[5,2]},{"value":[5,2.3]},{"value":[5.1,2.5]},{"value":[5.2,2.7]},{"value":[5.4,3]},{"value":[5.5,2.3]},{"value":[5.5,2.4]},{"value":[5.5,2.4]},{"value":[5.5,2.5]},{"value":[5.5,2.6]},{"value":[5.6,2.9]},{"value":[5.6,3]},{"value":[5.6,2.5]},{"value":[5.6,3]},{"value":[5.6,2.7]},{"value":[5.7,2.8]},{"value":[5.7,2.6]},{"value":[5.7,3]},{"value":[5.7,2.9]},{"value":[5.7,2.8]},{"value":[5.8,2.7]},{"value":[5.8,2.7]},{"value":[5.8,2.6]},{"value":[5.9,3]},{"value":[5.9,3.2]},{"value":[6,2.2]},{"value":[6,2.9]},{"value":[6,2.7]},{"value":[6,3.4]},{"value":[6.1,2.9]},{"value":[6.1,2.8]},{"value":[6.1,2.8]},{"value":[6.1,3]},{"value":[6.2,2.2]},{"value":[6.2,2.9]},{"value":[6.3,3.3]},{"value":[6.3,2.5]},{"value":[6.3,2.3]},{"value":[6.4,3.2]},{"value":[6.4,2.9]},{"value":[6.5,2.8]},{"value":[6.6,2.9]},{"value":[6.6,3]},{"value":[6.7,3.1]},{"value":[6.7,3]},{"value":[6.7,3.1]},{"value":[6.8,2.8]},{"value":[6.9,3.1]},{"value":[7,3.2]}],"yAxisIndex":0,"xAxisIndex":0,"name":"versicolor","type":"line","coordinateSystem":"cartesian2d"},{"data":[{"value":[4.9,2.5]},{"value":[5.6,2.8]},{"value":[5.7,2.5]},{"value":[5.8,2.7]},{"value":[5.8,2.8]},{"value":[5.8,2.7]},{"value":[5.9,3]},{"value":[6,2.2]},{"value":[6,3]},{"value":[6.1,3]},{"value":[6.1,2.6]},{"value":[6.2,2.8]},{"value":[6.2,3.4]},{"value":[6.3,3.3]},{"value":[6.3,2.9]},{"value":[6.3,2.7]},{"value":[6.3,2.8]},{"value":[6.3,3.4]},{"value":[6.3,2.5]},{"value":[6.4,2.7]},{"value":[6.4,3.2]},{"value":[6.4,2.8]},{"value":[6.4,2.8]},{"value":[6.4,3.1]},{"value":[6.5,3]},{"value":[6.5,3.2]},{"value":[6.5,3]},{"value":[6.5,3]},{"value":[6.7,2.5]},{"value":[6.7,3.3]},{"value":[6.7,3.1]},{"value":[6.7,3.3]},{"value":[6.7,3]},{"value":[6.8,3]},{"value":[6.8,3.2]},{"value":[6.9,3.2]},{"value":[6.9,3.1]},{"value":[6.9,3.1]},{"value":[7.1,3]},{"value":[7.2,3.6]},{"value":[7.2,3.2]},{"value":[7.2,3]},{"value":[7.3,2.9]},{"value":[7.4,2.8]},{"value":[7.6,3]},{"value":[7.7,3.8]},{"value":[7.7,2.6]},{"value":[7.7,2.8]},{"value":[7.7,3]},{"value":[7.9,3.8]}],"yAxisIndex":0,"xAxisIndex":0,"name":"virginica","type":"line","coordinateSystem":"cartesian2d"},{"data":[{"value":[4.3,1.1]},{"value":[4.4,1.4]},{"value":[4.4,1.3]},{"value":[4.4,1.3]},{"value":[4.5,1.3]},{"value":[4.6,1.5]},{"value":[4.6,1.4]},{"value":[4.6,1]},{"value":[4.6,1.4]},{"value":[4.7,1.3]},{"value":[4.7,1.6]},{"value":[4.8,1.6]},{"value":[4.8,1.4]},{"value":[4.8,1.9]},{"value":[4.8,1.6]},{"value":[4.8,1.4]},{"value":[4.9,1.4]},{"value":[4.9,1.5]},{"value":[4.9,1.5]},{"value":[4.9,1.4]},{"value":[5,1.4]},{"value":[5,1.5]},{"value":[5,1.6]},{"value":[5,1.6]},{"value":[5,1.2]},{"value":[5,1.3]},{"value":[5,1.6]},{"value":[5,1.4]},{"value":[5.1,1.4]},{"value":[5.1,1.4]},{"value":[5.1,1.5]},{"value":[5.1,1.5]},{"value":[5.1,1.7]},{"value":[5.1,1.5]},{"value":[5.1,1.9]},{"value":[5.1,1.6]},{"value":[5.2,1.5]},{"value":[5.2,1.4]},{"value":[5.2,1.5]},{"value":[5.3,1.5]},{"value":[5.4,1.7]},{"value":[5.4,1.5]},{"value":[5.4,1.3]},{"value":[5.4,1.7]},{"value":[5.4,1.5]},{"value":[5.5,1.4]},{"value":[5.5,1.3]},{"value":[5.7,1.5]},{"value":[5.7,1.7]},{"value":[5.8,1.2]}],"yAxisIndex":0,"xAxisIndex":0,"name":"setosa","type":"line","coordinateSystem":"cartesian2d"},{"data":[{"value":[4.9,3.3]},{"value":[5,3.5]},{"value":[5,3.3]},{"value":[5.1,3]},{"value":[5.2,3.9]},{"value":[5.4,4.5]},{"value":[5.5,4]},{"value":[5.5,3.8]},{"value":[5.5,3.7]},{"value":[5.5,4]},{"value":[5.5,4.4]},{"value":[5.6,3.6]},{"value":[5.6,4.5]},{"value":[5.6,3.9]},{"value":[5.6,4.1]},{"value":[5.6,4.2]},{"value":[5.7,4.5]},{"value":[5.7,3.5]},{"value":[5.7,4.2]},{"value":[5.7,4.2]},{"value":[5.7,4.1]},{"value":[5.8,4.1]},{"value":[5.8,3.9]},{"value":[5.8,4]},{"value":[5.9,4.2]},{"value":[5.9,4.8]},{"value":[6,4]},{"value":[6,4.5]},{"value":[6,5.1]},{"value":[6,4.5]},{"value":[6.1,4.7]},{"value":[6.1,4]},{"value":[6.1,4.7]},{"value":[6.1,4.6]},{"value":[6.2,4.5]},{"value":[6.2,4.3]},{"value":[6.3,4.7]},{"value":[6.3,4.9]},{"value":[6.3,4.4]},{"value":[6.4,4.5]},{"value":[6.4,4.3]},{"value":[6.5,4.6]},{"value":[6.6,4.6]},{"value":[6.6,4.4]},{"value":[6.7,4.4]},{"value":[6.7,5]},{"value":[6.7,4.7]},{"value":[6.8,4.8]},{"value":[6.9,4.9]},{"value":[7,4.7]}],"yAxisIndex":0,"xAxisIndex":0,"name":"versicolor","type":"line","coordinateSystem":"cartesian2d"},{"data":[{"value":[4.9,4.5]},{"value":[5.6,4.9]},{"value":[5.7,5]},{"value":[5.8,5.1]},{"value":[5.8,5.1]},{"value":[5.8,5.1]},{"value":[5.9,5.1]},{"value":[6,5]},{"value":[6,4.8]},{"value":[6.1,4.9]},{"value":[6.1,5.6]},{"value":[6.2,4.8]},{"value":[6.2,5.4]},{"value":[6.3,6]},{"value":[6.3,5.6]},{"value":[6.3,4.9]},{"value":[6.3,5.1]},{"value":[6.3,5.6]},{"value":[6.3,5]},{"value":[6.4,5.3]},{"value":[6.4,5.3]},{"value":[6.4,5.6]},{"value":[6.4,5.6]},{"value":[6.4,5.5]},{"value":[6.5,5.8]},{"value":[6.5,5.1]},{"value":[6.5,5.5]},{"value":[6.5,5.2]},{"value":[6.7,5.8]},{"value":[6.7,5.7]},{"value":[6.7,5.6]},{"value":[6.7,5.7]},{"value":[6.7,5.2]},{"value":[6.8,5.5]},{"value":[6.8,5.9]},{"value":[6.9,5.7]},{"value":[6.9,5.4]},{"value":[6.9,5.1]},{"value":[7.1,5.9]},{"value":[7.2,6.1]},{"value":[7.2,6]},{"value":[7.2,5.8]},{"value":[7.3,6.3]},{"value":[7.4,6.1]},{"value":[7.6,6.6]},{"value":[7.7,6.7]},{"value":[7.7,6.9]},{"value":[7.7,6.7]},{"value":[7.7,6.1]},{"value":[7.9,6.4]}],"yAxisIndex":0,"xAxisIndex":0,"name":"virginica","type":"line","coordinateSystem":"cartesian2d"}]},"dispose":true},"evals":[],"jsHooks":[]}
```
