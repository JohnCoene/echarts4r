# Formatters

Simple formatters as helpers.

## Usage

``` r
e_format_axis(e, axis = "y", suffix = NULL, prefix = NULL, ...)

e_format_x_axis(e, suffix = NULL, prefix = NULL, ...)

e_format_y_axis(e, suffix = NULL, prefix = NULL, ...)
```

## Arguments

- e:

  An `echarts4r` object as returned by
  [`e_charts`](https://echarts4r.john-coene.com/reference/init.md) or a
  proxy as returned by
  [`echarts4rProxy`](https://echarts4r.john-coene.com/reference/echarts4r-shiny.md).

- axis:

  Axis to apply formatter to.

- suffix, prefix:

  Suffix and prefix of label.

- ...:

  Any other arguments to pass to
  [`e_axis`](https://echarts4r.john-coene.com/reference/axis.md).

## Examples

``` r
# Y = %
df <- data.frame(
  x = 1:10,
  y = round(
    runif(10, 1, 100),
    2
  )
)

df |>
  e_charts(x) |>
  e_line(y) |>
  e_format_y_axis(suffix = "%") |>
  e_format_x_axis(prefix = "A")

{"x":{"theme":"","tl":false,"draw":true,"renderer":"canvas","events":[],"buttons":[],"settings":{"crosstalk_key":null,"crosstalk_group":null},"opts":{"yAxis":[{"show":true,"axisLabel":{"formatter":" {value} %"}}],"xAxis":[{"type":"value","axisLabel":{"formatter":"A {value} "}}],"legend":{"data":["y"]},"series":[{"data":[{"value":[1,66.95999999999999]},{"value":[2,92.15000000000001]},{"value":[3,77.61]},{"value":[4,73.41]},{"value":[5,60.29]},{"value":[6,42.74]},{"value":[7,17.05]},{"value":[8,75.40000000000001]},{"value":[9,75.90000000000001]},{"value":[10,57.44]}],"yAxisIndex":0,"xAxisIndex":0,"name":"y","type":"line","coordinateSystem":"cartesian2d"}]},"dispose":true},"evals":[],"jsHooks":[]}
```
