# Gauge

Plot a gauge.

## Usage

``` r
e_gauge(e, value, name = NULL, rm_x = TRUE, rm_y = TRUE, ...)

e_gauge_(e, value, name = NULL, rm_x = TRUE, rm_y = TRUE, ...)
```

## Arguments

- e:

  An `echarts4r` object as returned by
  [`e_charts`](https://echarts4r.john-coene.com/reference/init.md) or a
  proxy as returned by
  [`echarts4rProxy`](https://echarts4r.john-coene.com/reference/echarts4r-shiny.md).

- value:

  Value to gauge.

- name:

  Text on gauge.

- rm_x, rm_y:

  Whether to remove x and y axis, defaults to `TRUE`.

- ...:

  Any other option to pass, check See Also section.

## See also

[Additional
arguments](https://echarts.apache.org/en/option.html#series-gauge)

## Examples

``` r
e_charts() |>
  e_gauge(57, "PERCENT")

{"x":{"theme":"","tl":false,"draw":true,"renderer":"canvas","events":[],"buttons":[],"settings":{"crosstalk_key":null,"crosstalk_group":null},"opts":{"series":[{"data":[{"value":57,"name":"PERCENT"}],"type":"gauge"}]},"dispose":true},"evals":[],"jsHooks":[]}
# timeline
data.frame(time = 2015:2017) |>
  group_by(time) |>
  e_charts(timeline = TRUE) |>
  e_gauge(
    c(57, 23, 65),
    c("percent", "percentage", "cases")
  )

{"x":{"theme":"","tl":true,"draw":true,"renderer":"canvas","events":[],"buttons":[],"settings":{"crosstalk_key":null,"crosstalk_group":null},"opts":{"baseOption":{"timeline":{"data":["2015","2016","2017"],"axisType":"category"},"series":[{"type":"gauge"}]},"options":[{"series":[{"data":[{"value":57,"name":"percent"}]}]},{"series":[{"data":[{"value":23,"name":"percentage"}]}]},{"series":[{"data":[{"value":65,"name":"cases"}]}]}]},"dispose":true},"evals":[],"jsHooks":[]}
```
