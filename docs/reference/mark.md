# Mark point

Mark points and lines.

## Usage

``` r
e_mark_point(
  e,
  serie = NULL,
  data = NULL,
  ...,
  title = NULL,
  title_position = NULL
)

e_mark_line(
  e,
  serie = NULL,
  data = NULL,
  ...,
  title = NULL,
  title_position = NULL
)

e_mark_area(
  e,
  serie = NULL,
  data = NULL,
  ...,
  title = NULL,
  title_position = NULL
)
```

## Arguments

- e:

  An `echarts4r` object as returned by
  [`e_charts`](https://echarts4r.john-coene.com/reference/init.md) or a
  proxy as returned by
  [`echarts4rProxy`](https://echarts4r.john-coene.com/reference/echarts4r-shiny.md).

- serie:

  Serie or vector of series to mark on, defaults to all series.

- data:

  Placement of point, line or area.

- ...:

  Any other option to pass, check See Also section.

- title:

  A convenience argument to easily set label, see details.

- title_position:

  Position of title.

## Details

To set a label you need to either use the `title` argument or pass a
list specifying the label formatter.
`label = list(formatter = "label")`. The former is more convenient but
more limited, e.g.: you cannot specify the placement of the label. When
the `e_mark` series function is used with `e_timeline` at the same time,
if the number of marks provided does not match the series, the mark
information will follow the setting of the previous frame.

## See also

[Additional point
arguments](https://echarts.apache.org/en/option.html#series-line.markPoint),
[Additional line
arguments](https://echarts.apache.org/en/option.html#series-line.markLine)

## Examples

``` r
max <- list(
  name = "Max",
  type = "max"
)

min <- list(
  name = "Min",
  type = "min"
)

avg <- list(
  type = "average",
  name = "AVG"
)

mtcars |>
  e_charts(mpg) |>
  e_line(wt) |>
  e_line(drat) |>
  e_line(cyl) |>
  e_mark_point("wt", data = max) |>
  e_mark_point(c("cyl", "drat"), data = min) |>
  e_mark_line(data = avg) |> # applies to all
  e_mark_area(
    serie = "wt",
    data = list(
      list(xAxis = "min", yAxis = "min"),
      list(xAxis = "max", yAxis = "max")
    )
  )

{"x":{"theme":"","tl":false,"draw":true,"renderer":"canvas","events":[],"buttons":[],"settings":{"crosstalk_key":null,"crosstalk_group":null},"opts":{"yAxis":[{"show":true}],"xAxis":[{"type":"value"}],"legend":{"data":["wt","drat","cyl"]},"series":[{"data":[{"value":[10.4,5.25]},{"value":[10.4,5.424]},{"value":[13.3,3.84]},{"value":[14.3,3.57]},{"value":[14.7,5.345]},{"value":[15,3.57]},{"value":[15.2,3.78]},{"value":[15.2,3.435]},{"value":[15.5,3.52]},{"value":[15.8,3.17]},{"value":[16.4,4.07]},{"value":[17.3,3.73]},{"value":[17.8,3.44]},{"value":[18.1,3.46]},{"value":[18.7,3.44]},{"value":[19.2,3.44]},{"value":[19.2,3.845]},{"value":[19.7,2.77]},{"value":[21,2.62]},{"value":[21,2.875]},{"value":[21.4,3.215]},{"value":[21.4,2.78]},{"value":[21.5,2.465]},{"value":[22.8,2.32]},{"value":[22.8,3.15]},{"value":[24.4,3.19]},{"value":[26,2.14]},{"value":[27.3,1.935]},{"value":[30.4,1.615]},{"value":[30.4,1.513]},{"value":[32.4,2.2]},{"value":[33.9,1.835]}],"yAxisIndex":0,"xAxisIndex":0,"name":"wt","type":"line","coordinateSystem":"cartesian2d","markPoint":{"data":[{"name":"Max","type":"max"}]},"markLine":{"data":[{"type":"average","name":"AVG"}]},"markArea":{"data":[[{"xAxis":"min","yAxis":"min"},{"xAxis":"max","yAxis":"max"}]]}},{"data":[{"value":[10.4,2.93]},{"value":[10.4,3]},{"value":[13.3,3.73]},{"value":[14.3,3.21]},{"value":[14.7,3.23]},{"value":[15,3.54]},{"value":[15.2,3.07]},{"value":[15.2,3.15]},{"value":[15.5,2.76]},{"value":[15.8,4.22]},{"value":[16.4,3.07]},{"value":[17.3,3.07]},{"value":[17.8,3.92]},{"value":[18.1,2.76]},{"value":[18.7,3.15]},{"value":[19.2,3.92]},{"value":[19.2,3.08]},{"value":[19.7,3.62]},{"value":[21,3.9]},{"value":[21,3.9]},{"value":[21.4,3.08]},{"value":[21.4,4.11]},{"value":[21.5,3.7]},{"value":[22.8,3.85]},{"value":[22.8,3.92]},{"value":[24.4,3.69]},{"value":[26,4.43]},{"value":[27.3,4.08]},{"value":[30.4,4.93]},{"value":[30.4,3.77]},{"value":[32.4,4.08]},{"value":[33.9,4.22]}],"yAxisIndex":0,"xAxisIndex":0,"name":"drat","type":"line","coordinateSystem":"cartesian2d","markPoint":{"data":[{"name":"Min","type":"min"}]},"markLine":{"data":[{"type":"average","name":"AVG"}]}},{"data":[{"value":[10.4,8]},{"value":[10.4,8]},{"value":[13.3,8]},{"value":[14.3,8]},{"value":[14.7,8]},{"value":[15,8]},{"value":[15.2,8]},{"value":[15.2,8]},{"value":[15.5,8]},{"value":[15.8,8]},{"value":[16.4,8]},{"value":[17.3,8]},{"value":[17.8,6]},{"value":[18.1,6]},{"value":[18.7,8]},{"value":[19.2,6]},{"value":[19.2,8]},{"value":[19.7,6]},{"value":[21,6]},{"value":[21,6]},{"value":[21.4,6]},{"value":[21.4,4]},{"value":[21.5,4]},{"value":[22.8,4]},{"value":[22.8,4]},{"value":[24.4,4]},{"value":[26,4]},{"value":[27.3,4]},{"value":[30.4,4]},{"value":[30.4,4]},{"value":[32.4,4]},{"value":[33.9,4]}],"yAxisIndex":0,"xAxisIndex":0,"name":"cyl","type":"line","coordinateSystem":"cartesian2d","markPoint":{"data":[{"name":"Min","type":"min"}]},"markLine":{"data":[{"type":"average","name":"AVG"}]}}]},"dispose":true},"evals":[],"jsHooks":[]}
# Serie options, since the mark of "virginica" is not set, the mark setting
# of the previous frame is used
iris |>
  group_by(Species) |>
  e_charts(Sepal.Length, timeline = TRUE) |>
  e_line(Sepal.Width) |>
  e_timeline_serie(
    title = list(
      list(text = "setosa"),
      list(text = "versicolor"),
      list(text = "virginica")
    )
  ) |>
  e_mark_area(
    serie = "setosa",
    data = list(
      list(xAxis = 4, yAxis = 2),
      list(xAxis = 6, yAxis = 4.5)
    ),
    itemStyle = list(color = "lightgreen")
  ) |>
  e_mark_area(
    serie = "versicolor",
    data = list(
      list(xAxis = 4.5),
      list(xAxis = 7)
    ),
    itemStyle = list(color = "lightblue")
  )

{"x":{"theme":"","tl":true,"draw":true,"renderer":"canvas","events":[],"buttons":[],"settings":{"crosstalk_key":null,"crosstalk_group":null},"opts":{"baseOption":{"yAxis":[{"show":true}],"timeline":{"data":["setosa","versicolor","virginica"],"axisType":"category"},"xAxis":[{"type":"value"}],"legend":{"data":["Sepal.Width"]},"series":[{"name":"Sepal.Width","type":"line","selectedMode":"single","yAxisIndex":0,"xAxisIndex":0,"coordinateSystem":"cartesian2d"}]},"options":[{"series":[{"data":[{"value":[4.3,3]},{"value":[4.4,2.9]},{"value":[4.4,3]},{"value":[4.4,3.2]},{"value":[4.5,2.3]},{"value":[4.6,3.1]},{"value":[4.6,3.4]},{"value":[4.6,3.6]},{"value":[4.6,3.2]},{"value":[4.7,3.2]},{"value":[4.7,3.2]},{"value":[4.8,3.4]},{"value":[4.8,3]},{"value":[4.8,3.4]},{"value":[4.8,3.1]},{"value":[4.8,3]},{"value":[4.9,3]},{"value":[4.9,3.1]},{"value":[4.9,3.1]},{"value":[4.9,3.6]},{"value":[5,3.6]},{"value":[5,3.4]},{"value":[5,3]},{"value":[5,3.4]},{"value":[5,3.2]},{"value":[5,3.5]},{"value":[5,3.5]},{"value":[5,3.3]},{"value":[5.1,3.5]},{"value":[5.1,3.5]},{"value":[5.1,3.8]},{"value":[5.1,3.7]},{"value":[5.1,3.3]},{"value":[5.1,3.4]},{"value":[5.1,3.8]},{"value":[5.1,3.8]},{"value":[5.2,3.5]},{"value":[5.2,3.4]},{"value":[5.2,4.1]},{"value":[5.3,3.7]},{"value":[5.4,3.9]},{"value":[5.4,3.7]},{"value":[5.4,3.9]},{"value":[5.4,3.4]},{"value":[5.4,3.4]},{"value":[5.5,4.2]},{"value":[5.5,3.5]},{"value":[5.7,4.4]},{"value":[5.7,3.8]},{"value":[5.8,4]}],"markArea":{"itemStyle":{"color":"lightgreen"},"data":[[{"xAxis":4,"yAxis":2},{"xAxis":6,"yAxis":4.5}]]}}],"title":[{"text":"setosa"}]},{"series":[{"data":[{"value":[4.9,2.4]},{"value":[5,2]},{"value":[5,2.3]},{"value":[5.1,2.5]},{"value":[5.2,2.7]},{"value":[5.4,3]},{"value":[5.5,2.3]},{"value":[5.5,2.4]},{"value":[5.5,2.4]},{"value":[5.5,2.5]},{"value":[5.5,2.6]},{"value":[5.6,2.9]},{"value":[5.6,3]},{"value":[5.6,2.5]},{"value":[5.6,3]},{"value":[5.6,2.7]},{"value":[5.7,2.8]},{"value":[5.7,2.6]},{"value":[5.7,3]},{"value":[5.7,2.9]},{"value":[5.7,2.8]},{"value":[5.8,2.7]},{"value":[5.8,2.7]},{"value":[5.8,2.6]},{"value":[5.9,3]},{"value":[5.9,3.2]},{"value":[6,2.2]},{"value":[6,2.9]},{"value":[6,2.7]},{"value":[6,3.4]},{"value":[6.1,2.9]},{"value":[6.1,2.8]},{"value":[6.1,2.8]},{"value":[6.1,3]},{"value":[6.2,2.2]},{"value":[6.2,2.9]},{"value":[6.3,3.3]},{"value":[6.3,2.5]},{"value":[6.3,2.3]},{"value":[6.4,3.2]},{"value":[6.4,2.9]},{"value":[6.5,2.8]},{"value":[6.6,2.9]},{"value":[6.6,3]},{"value":[6.7,3.1]},{"value":[6.7,3]},{"value":[6.7,3.1]},{"value":[6.8,2.8]},{"value":[6.9,3.1]},{"value":[7,3.2]}],"markArea":{"itemStyle":{"color":"lightblue"},"data":[[{"xAxis":4.5},{"xAxis":7}]]}}],"title":[{"text":"versicolor"}]},{"series":[{"data":[{"value":[4.9,2.5]},{"value":[5.6,2.8]},{"value":[5.7,2.5]},{"value":[5.8,2.7]},{"value":[5.8,2.8]},{"value":[5.8,2.7]},{"value":[5.9,3]},{"value":[6,2.2]},{"value":[6,3]},{"value":[6.1,3]},{"value":[6.1,2.6]},{"value":[6.2,2.8]},{"value":[6.2,3.4]},{"value":[6.3,3.3]},{"value":[6.3,2.9]},{"value":[6.3,2.7]},{"value":[6.3,2.8]},{"value":[6.3,3.4]},{"value":[6.3,2.5]},{"value":[6.4,2.7]},{"value":[6.4,3.2]},{"value":[6.4,2.8]},{"value":[6.4,2.8]},{"value":[6.4,3.1]},{"value":[6.5,3]},{"value":[6.5,3.2]},{"value":[6.5,3]},{"value":[6.5,3]},{"value":[6.7,2.5]},{"value":[6.7,3.3]},{"value":[6.7,3.1]},{"value":[6.7,3.3]},{"value":[6.7,3]},{"value":[6.8,3]},{"value":[6.8,3.2]},{"value":[6.9,3.2]},{"value":[6.9,3.1]},{"value":[6.9,3.1]},{"value":[7.1,3]},{"value":[7.2,3.6]},{"value":[7.2,3.2]},{"value":[7.2,3]},{"value":[7.3,2.9]},{"value":[7.4,2.8]},{"value":[7.6,3]},{"value":[7.7,3.8]},{"value":[7.7,2.6]},{"value":[7.7,2.8]},{"value":[7.7,3]},{"value":[7.9,3.8]}]}],"title":[{"text":"virginica"}]}],"legend":{"data":[null,null,null]}},"dispose":true},"evals":[],"jsHooks":[]}
```
