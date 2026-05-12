# Color

Customise chart and background colors.

## Usage

``` r
e_color(e, color = NULL, background = NULL, append = TRUE)

# S3 method for class 'echarts4r'
e_color(e, color = NULL, background = NULL, append = TRUE)

# S3 method for class 'echarts4rProxy'
e_color(e, color = NULL, background = NULL, append = TRUE)
```

## Arguments

- e:

  An `echarts4r` object as returned by
  [`e_charts`](https://echarts4r.john-coene.com/reference/init.md) or a
  proxy as returned by
  [`echarts4rProxy`](https://echarts4r.john-coene.com/reference/echarts4r-shiny.md).

- color:

  Vector of colors.

- background:

  Background color.

- append:

  Only applicable to \`echarts4rProxy\`. Whether to append the \`color\`
  to the existing array (vector) or colors or to replace it.

## See also

[`e_theme`](https://echarts4r.john-coene.com/reference/theme.md),
[Official color
documentation](https://echarts.apache.org/en/option.html#color),
[Official background
documentation](https://echarts.apache.org/en/option.html#backgroundColor)

## Examples

``` r
mtcars |>
  e_charts(drat) |>
  e_line(mpg) |>
  e_area(qsec) |>
  e_color(
    c("red", "blue"),
    "#d3d3d3"
  )

{"x":{"theme":"","tl":false,"draw":true,"renderer":"canvas","events":[],"buttons":[],"settings":{"crosstalk_key":null,"crosstalk_group":null},"opts":{"yAxis":[{"show":true}],"xAxis":[{"type":"value"}],"legend":{"data":["mpg","qsec"]},"series":[{"data":[{"value":[2.76,18.1]},{"value":[2.76,15.5]},{"value":[2.93,10.4]},{"value":[3,10.4]},{"value":[3.07,16.4]},{"value":[3.07,17.3]},{"value":[3.07,15.2]},{"value":[3.08,21.4]},{"value":[3.08,19.2]},{"value":[3.15,18.7]},{"value":[3.15,15.2]},{"value":[3.21,14.3]},{"value":[3.23,14.7]},{"value":[3.54,15]},{"value":[3.62,19.7]},{"value":[3.69,24.4]},{"value":[3.7,21.5]},{"value":[3.73,13.3]},{"value":[3.77,30.4]},{"value":[3.85,22.8]},{"value":[3.9,21]},{"value":[3.9,21]},{"value":[3.92,22.8]},{"value":[3.92,19.2]},{"value":[3.92,17.8]},{"value":[4.08,32.4]},{"value":[4.08,27.3]},{"value":[4.11,21.4]},{"value":[4.22,33.9]},{"value":[4.22,15.8]},{"value":[4.43,26]},{"value":[4.93,30.4]}],"yAxisIndex":0,"xAxisIndex":0,"name":"mpg","type":"line","coordinateSystem":"cartesian2d"},{"data":[{"value":[2.76,20.22]},{"value":[2.76,16.87]},{"value":[2.93,17.98]},{"value":[3,17.82]},{"value":[3.07,17.4]},{"value":[3.07,17.6]},{"value":[3.07,18]},{"value":[3.08,19.44]},{"value":[3.08,17.05]},{"value":[3.15,17.02]},{"value":[3.15,17.3]},{"value":[3.21,15.84]},{"value":[3.23,17.42]},{"value":[3.54,14.6]},{"value":[3.62,15.5]},{"value":[3.69,20]},{"value":[3.7,20.01]},{"value":[3.73,15.41]},{"value":[3.77,16.9]},{"value":[3.85,18.61]},{"value":[3.9,16.46]},{"value":[3.9,17.02]},{"value":[3.92,22.9]},{"value":[3.92,18.3]},{"value":[3.92,18.9]},{"value":[4.08,19.47]},{"value":[4.08,18.9]},{"value":[4.11,18.6]},{"value":[4.22,19.9]},{"value":[4.22,14.5]},{"value":[4.43,16.7]},{"value":[4.93,18.52]}],"yAxisIndex":0,"xAxisIndex":0,"name":"qsec","type":"line","coordinateSystem":"cartesian2d","areaStyle":[]}],"color":["red","blue"],"backgroundColor":"#d3d3d3"},"dispose":true},"evals":[],"jsHooks":[]}
```
