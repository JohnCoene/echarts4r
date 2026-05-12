# Radius axis

Customise radius axis.

## Usage

``` r
e_radius_axis(e, serie, show = TRUE, ...)

e_radius_axis_(e, serie = NULL, show = TRUE, ...)
```

## Arguments

- e:

  An `echarts4r` object as returned by
  [`e_charts`](https://echarts4r.john-coene.com/reference/init.md) or a
  proxy as returned by
  [`echarts4rProxy`](https://echarts4r.john-coene.com/reference/echarts4r-shiny.md).

- serie:

  Serie to use as axis labels.

- show:

  Whether to display the axis.

- ...:

  Any other option to pass, check See Also section.

## See also

[Additional
arguments](https://echarts.apache.org/en/option.html#radiusAxis)

## Examples

``` r
df <- data.frame(x = LETTERS[1:10], y = seq(1, 20, by = 2))

df |>
  e_charts(x) |>
  e_polar() |>
  e_angle_axis() |>
  e_radius_axis(x) |>
  e_bar(y, coord_system = "polar")

{"x":{"theme":"","tl":false,"draw":true,"renderer":"canvas","events":[],"buttons":[],"settings":{"crosstalk_key":null,"crosstalk_group":null},"opts":{"polar":{"show":true},"angleAxis":{"show":true},"radiusAxis":{"show":true,"data":["A","B","C","D","E","F","G","H","I","J"]},"legend":{"data":["y"]},"series":[{"data":[1,3,5,7,9,11,13,15,17,19],"name":"y","type":"bar","yAxisIndex":0,"xAxisIndex":0,"coordinateSystem":"polar"}]},"dispose":true},"evals":[],"jsHooks":[]}
```
