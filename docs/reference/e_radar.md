# Radar

Add a radar chart

## Usage

``` r
e_radar(
  e,
  serie,
  max = 100,
  name = NULL,
  legend = TRUE,
  rm_x = TRUE,
  rm_y = TRUE,
  ...,
  radar = list()
)

e_radar_(
  e,
  serie,
  max = 100,
  name = NULL,
  legend = TRUE,
  rm_x = TRUE,
  rm_y = TRUE,
  ...,
  radar = list()
)
```

## Arguments

- e:

  An `echarts4r` object as returned by
  [`e_charts`](https://echarts4r.john-coene.com/reference/init.md) or a
  proxy as returned by
  [`echarts4rProxy`](https://echarts4r.john-coene.com/reference/echarts4r-shiny.md).

- serie:

  Column name of serie to plot.

- max:

  Maximum value.

- name:

  name of the serie.

- legend:

  Whether to add serie to legend.

- rm_x, rm_y:

  Whether to remove x and y axis, defaults to `TRUE`.

- ...:

  Any other option to pass, check See Also section.

- radar:

  A `list` of options to pass to the `radar` rather than the serie, see
  [official
  documentation](https://echarts.apache.org/en/option.html#radar)
  alternatively, use the
  [`e_radar_opts`](https://echarts4r.john-coene.com/reference/e_radar_opts.md).

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
  e_tooltip(trigger = "item")

{"x":{"theme":"","tl":false,"draw":true,"renderer":"canvas","events":[],"buttons":[],"settings":{"crosstalk_key":null,"crosstalk_group":null},"opts":{"radar":[{"indicator":[{"name":"A","max":"7"},{"name":"B","max":"7"},{"name":"C","max":"7"},{"name":"D","max":"7"},{"name":"E","max":"7"}]}],"series":[{"type":"radar","data":[{"value":[2.439802867360413,1.771020610816777,4.545185283757746,3.879675946198404,3.098696202971041],"name":"y"},{"value":[4.282921931706369,4.73408012278378,3.301008227281272,6.516692508943379,3.505958292633295],"name":"z"}],"radarIndex":0}],"legend":{"data":["y","z"]},"tooltip":{"trigger":"item"}},"dispose":true},"evals":[],"jsHooks":[]}
```
