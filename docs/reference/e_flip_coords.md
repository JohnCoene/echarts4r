# Flip coordinates

Flip cartesian 2D coordinates.

## Usage

``` r
e_flip_coords(e)
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
  x = LETTERS[1:5],
  y = runif(5, 1, 5),
  z = runif(5, 3, 10)
)

df |>
  e_charts(x) |>
  e_bar(y) |>
  e_line(z) -> plot

plot # normal

{"x":{"theme":"","tl":false,"draw":true,"renderer":"canvas","events":[],"buttons":[],"settings":{"crosstalk_key":null,"crosstalk_group":null},"opts":{"yAxis":[{"show":true}],"xAxis":[{"data":["A","B","C","D","E"],"type":"category","boundaryGap":true}],"legend":{"data":["y","z"]},"series":[{"data":[{"value":["A","3.579811"]},{"value":["B","1.388159"]},{"value":["C","1.042827"]},{"value":["D","3.335171"]},{"value":["E","1.984933"]}],"name":"y","type":"bar","yAxisIndex":0,"xAxisIndex":0,"coordinateSystem":"cartesian2d"},{"data":[{"value":["A","9.235357"]},{"value":["B","8.565122"]},{"value":["C","3.135261"]},{"value":["D","8.243479"]},{"value":["E","9.019232"]}],"yAxisIndex":0,"xAxisIndex":0,"name":"z","type":"line","coordinateSystem":"cartesian2d"}]},"dispose":true},"evals":[],"jsHooks":[]}e_flip_coords(plot) # flip

{"x":{"theme":"","tl":false,"draw":true,"renderer":"canvas","events":[],"buttons":[],"settings":{"crosstalk_key":null,"crosstalk_group":null},"opts":{"xAxis":[{"show":true}],"yAxis":[{"data":["A","B","C","D","E"],"type":"category","boundaryGap":true}],"legend":{"data":["y","z"]},"series":[{"data":[{"value":["3.579811","A"]},{"value":["1.388159","B"]},{"value":["1.042827","C"]},{"value":["3.335171","D"]},{"value":["1.984933","E"]}],"name":"y","type":"bar","yAxisIndex":0,"xAxisIndex":0,"coordinateSystem":"cartesian2d"},{"data":[{"value":["9.235357","A"]},{"value":["8.565122","B"]},{"value":["3.135261","C"]},{"value":["8.243479","D"]},{"value":["9.019232","E"]}],"yAxisIndex":0,"xAxisIndex":0,"name":"z","type":"line","coordinateSystem":"cartesian2d"}]},"dispose":true},"evals":[],"jsHooks":[]}
```
