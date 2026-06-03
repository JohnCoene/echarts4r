# Stagger Axis Labels

Stagger axis labels.

## Usage

``` r
e_axis_stagger(e)
```

## Arguments

- e:

  An `echarts4r` object as returned by
  [`e_charts`](https://echarts4r.john-coene.com/reference/init.md) or a
  proxy as returned by
  [`echarts4rProxy`](https://echarts4r.john-coene.com/reference/echarts4r-shiny.md).

## Examples

``` r
df <- data.frame(
  x = c("a very long label", "Another long label"),
  y = 1:2
)

df |>
  e_charts(x, width = 150) |>
  e_bar(y) |>
  e_axis_stagger()

{"x":{"theme":"","tl":false,"draw":true,"renderer":"canvas","events":[],"buttons":[],"settings":{"crosstalk_key":null,"crosstalk_group":null},"opts":{"yAxis":[{"show":true}],"xAxis":[{"data":["a very long label","Another long label"],"type":"category","boundaryGap":true,"axisLabel":{"formatter":"function(value, index){\n    if(index % 2){\n      return('\\n' + value)\n    }\n\n    return(value)\n  }"}}],"legend":{"data":["y"]},"series":[{"data":[{"value":["a very long label","1"]},{"value":["Another long label","2"]}],"name":"y","type":"bar","yAxisIndex":0,"xAxisIndex":0,"coordinateSystem":"cartesian2d"}]},"dispose":true},"evals":["opts.xAxis.0.axisLabel.formatter"],"jsHooks":[]}
```
