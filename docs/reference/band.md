# Confidence bands

Add confidence bands

## Usage

``` r
e_band(
  e,
  min,
  max,
  stack = "confidence-band",
  symbol = c("none", "none"),
  areaStyle = list(list(color = "rgba(0,0,0,0)"), list()),
  legend = list(FALSE, FALSE),
  ...
)

e_band_(
  e,
  min,
  max,
  stack = "confidence-band",
  symbol = c("none", "none"),
  areaStyle = list(list(color = "rgba(0,0,0,0)"), list()),
  legend = list(FALSE, FALSE),
  ...
)
```

## Arguments

- e:

  An `echarts4r` object as returned by
  [`e_charts`](https://echarts4r.john-coene.com/reference/init.md) or a
  proxy as returned by
  [`echarts4rProxy`](https://echarts4r.john-coene.com/reference/echarts4r-shiny.md).

- min, max:

  series.

- stack:

  Name of stack.

- symbol:

  Whether to show symbols on lower and upper band lines.

- areaStyle:

  The style of lower and upper bands, i.e.: color.

- legend:

  Whether to show `min` and `max` in legend.

- ...:

  All options must be of vectors or lists of length 2 where the first
  argument is for the lower bound and the second for the upper bound,
  see examples.

## Examples

``` r
df <- data.frame(
  x = 1:10,
  y = runif(10, 5, 10)
) |>
  dplyr::mutate(
    lwr = y - runif(10, 1, 3),
    upr = y + runif(10, 2, 4)
  )

df |>
  e_charts(x) |>
  e_line(y) |>
  e_band(lwr, upr)

{"x":{"theme":"","tl":false,"draw":true,"renderer":"canvas","events":[],"buttons":[],"settings":{"crosstalk_key":null,"crosstalk_group":null},"opts":{"yAxis":[{"show":true}],"xAxis":[{"type":"category"}],"legend":{"data":["y"]},"series":[{"data":[{"value":[1,6.587695921771228]},{"value":[2,5.911610504845157]},{"value":[3,9.805303844623268]},{"value":[4,8.980617700144649]},{"value":[5,9.893048591911793]},{"value":[6,5.474413692718372]},{"value":[7,8.327678302302957]},{"value":[8,9.287530876463279]},{"value":[9,6.427669920958579]},{"value":[10,7.741117889527231]}],"yAxisIndex":0,"xAxisIndex":0,"name":"y","type":"line","coordinateSystem":"cartesian2d"},{"data":[{"value":[1,5.183322138618678]},{"value":[2,3.026798709062859]},{"value":[3,7.49574301019311]},{"value":[4,6.093156149610877]},{"value":[5,8.752711775712669]},{"value":[6,4.330466856947169]},{"value":[7,6.63516099890694]},{"value":[8,6.616999725578353]},{"value":[9,4.531659244094044]},{"value":[10,5.006159068085253]}],"yAxisIndex":0,"xAxisIndex":0,"name":"lwr","type":"line","coordinateSystem":"cartesian2d","lineStyle":{"normal":{"opacity":0}},"symbol":"none","areaStyle":{"color":"rgba(0,0,0,0)"},"stack":"confidence-band"},{"data":[{"value":[1,3.449928707443178]},{"value":[2,6.875961432699114]},{"value":[3,4.628844581078738]},{"value":[4,4.964196152985096]},{"value":[5,4.465934642124921]},{"value":[6,3.14515307219699]},{"value":[7,4.642393993679434]},{"value":[8,5.911690924316645]},{"value":[9,4.832143064122647]},{"value":[10,5.5966154364869]}],"yAxisIndex":0,"xAxisIndex":0,"name":"upr","type":"line","coordinateSystem":"cartesian2d","lineStyle":{"normal":{"opacity":0}},"symbol":"none","areaStyle":[],"stack":"confidence-band"}]},"dispose":true},"evals":[],"jsHooks":[]}
```
