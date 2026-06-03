# Format labels

Format labels

## Usage

``` r
e_labels(e, show = TRUE, position = "top", ...)
```

## Arguments

- e:

  An `echarts4r` object as returned by
  [`e_charts`](https://echarts4r.john-coene.com/reference/init.md) or a
  proxy as returned by
  [`echarts4rProxy`](https://echarts4r.john-coene.com/reference/echarts4r-shiny.md).

- show:

  Set to `TRUE` to show the labels.

- position:

  Position of labels, see [official
  documentation](https://echarts.apache.org/en/option.html#series-line.label.position)
  for the full list of options.

- ...:

  Any other options see
  [documentation](https://echarts.apache.org/en/option.html#series-line.label)
  for other options.

## Examples

``` r
mtcars |>
  e_chart(wt) |>
  e_scatter(qsec, cyl) |>
  e_labels(fontSize = 9)

{"x":{"theme":"","tl":false,"draw":true,"renderer":"canvas","events":[],"buttons":[],"settings":{"crosstalk_key":null,"crosstalk_group":null},"opts":{"yAxis":[{"show":true}],"xAxis":[{"type":"value"}],"legend":{"data":["qsec"]},"series":[{"data":[{"value":[1.513,16.9,4,1]},{"value":[1.615,18.52,4,1]},{"value":[1.835,19.9,4,1]},{"value":[1.935,18.9,4,1]},{"value":[2.14,16.7,4,1]},{"value":[2.2,19.47,4,1]},{"value":[2.32,18.61,4,1]},{"value":[2.465,20.01,4,1]},{"value":[2.62,16.46,6,10.5]},{"value":[2.77,15.5,6,10.5]},{"value":[2.78,18.6,4,1]},{"value":[2.875,17.02,6,10.5]},{"value":[3.15,22.9,4,1]},{"value":[3.17,14.5,8,20]},{"value":[3.19,20,4,1]},{"value":[3.215,19.44,6,10.5]},{"value":[3.435,17.3,8,20]},{"value":[3.44,17.02,8,20]},{"value":[3.44,18.3,6,10.5]},{"value":[3.44,18.9,6,10.5]},{"value":[3.46,20.22,6,10.5]},{"value":[3.52,16.87,8,20]},{"value":[3.57,15.84,8,20]},{"value":[3.57,14.6,8,20]},{"value":[3.73,17.6,8,20]},{"value":[3.78,18,8,20]},{"value":[3.84,15.41,8,20]},{"value":[3.845,17.05,8,20]},{"value":[4.07,17.4,8,20]},{"value":[5.25,17.98,8,20]},{"value":[5.345,17.42,8,20]},{"value":[5.424,17.82,8,20]}],"name":"qsec","type":"scatter","symbol":null,"coordinateSystem":"cartesian2d","yAxisIndex":0,"xAxisIndex":0,"symbolSize":"function(data){ return data[3];}","label":{"show":true,"position":"top","fontSize":9}}]},"dispose":true},"evals":["opts.series.0.symbolSize"],"jsHooks":[]}
mtcars |>
  group_by(cyl) |>
  e_chart(wt) |>
  e_scatter(qsec, mpg) |>
  e_labels(fontSize = 9)

{"x":{"theme":"","tl":false,"draw":true,"renderer":"canvas","events":[],"buttons":[],"settings":{"crosstalk_key":null,"crosstalk_group":null},"opts":{"yAxis":[{"show":true}],"xAxis":[{"type":"value"}],"legend":{"data":["4","6","8"]},"series":[{"data":[{"value":[1.513,16.9,30.4,14.68]},{"value":[1.615,18.52,30.4,14.68]},{"value":[1.835,19.9,33.9,20]},{"value":[1.935,18.9,27.3,9.968000000000004]},{"value":[2.14,16.7,26,7.992000000000002]},{"value":[2.2,19.47,32.4,17.72]},{"value":[2.32,18.61,22.8,3.128000000000003]},{"value":[2.465,20.01,21.5,1.152000000000002]},{"value":[2.78,18.6,21.4,1]},{"value":[3.15,22.9,22.8,3.128000000000003]},{"value":[3.19,20,24.4,5.56]}],"name":"4","type":"scatter","symbol":null,"coordinateSystem":"cartesian2d","yAxisIndex":0,"xAxisIndex":0,"symbolSize":"function(data){ return data[3];}","label":{"show":true,"position":"top","fontSize":9}},{"data":[{"value":[2.62,16.46,21,17.88888888888889]},{"value":[2.77,15.5,19.7,11.02777777777778]},{"value":[2.875,17.02,21,17.88888888888889]},{"value":[3.215,19.44,21.4,20]},{"value":[3.44,18.3,19.2,8.388888888888886]},{"value":[3.44,18.9,17.8,1]},{"value":[3.46,20.22,18.1,2.583333333333338]}],"name":"6","type":"scatter","symbol":null,"coordinateSystem":"cartesian2d","yAxisIndex":0,"xAxisIndex":0,"symbolSize":"function(data){ return data[3];}","label":{"show":true,"position":"top","fontSize":9}},{"data":[{"value":[3.17,14.5,15.8,12.65909090909091]},{"value":[3.435,17.3,15.2,11.36363636363636]},{"value":[3.44,17.02,18.7,18.92045454545454]},{"value":[3.52,16.87,15.5,12.01136363636364]},{"value":[3.57,15.84,14.3,9.420454545454547]},{"value":[3.57,14.6,15,10.93181818181818]},{"value":[3.73,17.6,17.3,15.89772727272727]},{"value":[3.78,18,15.2,11.36363636363636]},{"value":[3.84,15.41,13.3,7.261363636363638]},{"value":[3.845,17.05,19.2,20]},{"value":[4.07,17.4,16.4,13.95454545454545]},{"value":[5.25,17.98,10.4,1]},{"value":[5.345,17.42,14.7,10.28409090909091]},{"value":[5.424,17.82,10.4,1]}],"name":"8","type":"scatter","symbol":null,"coordinateSystem":"cartesian2d","yAxisIndex":0,"xAxisIndex":0,"symbolSize":"function(data){ return data[3];}","label":{"show":true,"position":"top","fontSize":9}}]},"dispose":true},"evals":["opts.series.0.symbolSize","opts.series.1.symbolSize","opts.series.2.symbolSize"],"jsHooks":[]}
# timeline
mtcars |>
  group_by(cyl) |>
  e_chart(wt, timeline = TRUE) |>
  e_scatter(qsec, mpg) |>
  e_labels(fontSize = 9)

{"x":{"theme":"","tl":true,"draw":true,"renderer":"canvas","events":[],"buttons":[],"settings":{"crosstalk_key":null,"crosstalk_group":null},"opts":{"baseOption":{"yAxis":[{"show":true}],"timeline":{"data":["4","6","8"],"axisType":"category"},"xAxis":[{"type":"value"}],"legend":{"data":["qsec"]},"series":[{"coordinateSystem":"cartesian2d","yAxisIndex":0,"xAxisIndex":0,"symbolSize":"function(data){ return data[3];}","name":"qsec","type":"scatter","symbol":null,"label":{"show":true,"position":"top","fontSize":9}}]},"options":[{"series":[{"data":[{"value":[1.513,16.9,30.4,14.68]},{"value":[1.615,18.52,30.4,14.68]},{"value":[1.835,19.9,33.9,20]},{"value":[1.935,18.9,27.3,9.968000000000004]},{"value":[2.14,16.7,26,7.992000000000002]},{"value":[2.2,19.47,32.4,17.72]},{"value":[2.32,18.61,22.8,3.128000000000003]},{"value":[2.465,20.01,21.5,1.152000000000002]},{"value":[2.78,18.6,21.4,1]},{"value":[3.15,22.9,22.8,3.128000000000003]},{"value":[3.19,20,24.4,5.56]}]}]},{"series":[{"data":[{"value":[2.62,16.46,21,17.88888888888889]},{"value":[2.77,15.5,19.7,11.02777777777778]},{"value":[2.875,17.02,21,17.88888888888889]},{"value":[3.215,19.44,21.4,20]},{"value":[3.44,18.3,19.2,8.388888888888886]},{"value":[3.44,18.9,17.8,1]},{"value":[3.46,20.22,18.1,2.583333333333338]}]}]},{"series":[{"data":[{"value":[3.17,14.5,15.8,12.65909090909091]},{"value":[3.435,17.3,15.2,11.36363636363636]},{"value":[3.44,17.02,18.7,18.92045454545454]},{"value":[3.52,16.87,15.5,12.01136363636364]},{"value":[3.57,15.84,14.3,9.420454545454547]},{"value":[3.57,14.6,15,10.93181818181818]},{"value":[3.73,17.6,17.3,15.89772727272727]},{"value":[3.78,18,15.2,11.36363636363636]},{"value":[3.84,15.41,13.3,7.261363636363638]},{"value":[3.845,17.05,19.2,20]},{"value":[4.07,17.4,16.4,13.95454545454545]},{"value":[5.25,17.98,10.4,1]},{"value":[5.345,17.42,14.7,10.28409090909091]},{"value":[5.424,17.82,10.4,1]}]}]}]},"dispose":true},"evals":["opts.baseOption.series.0.symbolSize"],"jsHooks":[]}
```
