# Liquid fill

Draw liquid fill.

## Usage

``` r
e_liquid(e, serie, color, rm_x = TRUE, rm_y = TRUE, ...)

e_liquid_(e, serie, color = NULL, rm_x = TRUE, rm_y = TRUE, ...)
```

## Arguments

- e:

  An `echarts4r` object as returned by
  [`e_charts`](https://echarts4r.john-coene.com/reference/init.md) or a
  proxy as returned by
  [`echarts4rProxy`](https://echarts4r.john-coene.com/reference/echarts4r-shiny.md).

- serie:

  Column name of serie to plot.

- color:

  Column of color to plot.

- rm_x, rm_y:

  Whether to remove x and y axis, defaults to `TRUE`.

- ...:

  Any other option to pass, check See Also section.

## See also

[official documentation](https://github.com/ecomfe/echarts-liquidfill)

## Examples

``` r
df <- data.frame(val = c(0.6, 0.5, 0.4))

df |>
  e_charts() |>
  e_liquid(val) |>
  e_theme("dark")

{"x":{"theme":"dark","tl":false,"draw":true,"renderer":"canvas","events":[],"buttons":[],"settings":{"crosstalk_key":null,"crosstalk_group":null},"opts":{"series":[{"type":"liquidFill","data":[0.6,0.5,0.4]}]},"dispose":true},"evals":[],"jsHooks":[]}
```
