# Radar axis

Radar axis setup and options.

## Usage

``` r
e_radar_opts(e, index = 0, ...)
```

## Arguments

- e:

  An `echarts4r` object as returned by
  [`e_charts`](https://echarts4r.john-coene.com/reference/init.md) or a
  proxy as returned by
  [`echarts4rProxy`](https://echarts4r.john-coene.com/reference/echarts4r-shiny.md).

- index:

  Index of axis to customise.

- ...:

  Any other option to pass, check See Also section.

## Examples

``` r
df <- data.frame(
  x = LETTERS[1:5],
  y = runif(5, 1, 5),
  z = runif(5, 3, 7)
)

df |>
  e_charts(x) |>
  e_radar(y, max = 7) |>
  e_radar(z) |>
  e_radar_opts(center = c("25%", "25%")) |>
  e_tooltip(trigger = "item")

{"x":{"theme":"","tl":false,"draw":true,"renderer":"canvas","events":[],"buttons":[],"settings":{"crosstalk_key":null,"crosstalk_group":null},"opts":{"radar":[{"indicator":[{"name":"A","max":"7"},{"name":"B","max":"7"},{"name":"C","max":"7"},{"name":"D","max":"7"},{"name":"E","max":"7"}],"center":["25%","25%"]}],"series":[{"type":"radar","data":[{"value":[1.122476710937917,1.236693467013538,2.889348425902426,3.805653868243098,1.433709479868412],"name":"y"},{"value":[3.441468123346567,4.219672588631511,4.492245843634009,5.402136127464473,4.478003036230803],"name":"z"}],"radarIndex":0}],"legend":{"data":["y","z"]},"tooltip":{"trigger":"item"}},"dispose":true},"evals":[],"jsHooks":[]}
```
