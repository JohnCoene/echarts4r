# Candlestick

Add a candlestick chart.

## Usage

``` r
e_candle(
  e,
  opening,
  closing,
  low,
  high,
  bind,
  name = "candle",
  legend = TRUE,
  ...
)

e_candle_(
  e,
  opening,
  closing,
  low,
  high,
  bind = NULL,
  name = "candle",
  legend = TRUE,
  ...
)
```

## Arguments

- e:

  An `echarts4r` object as returned by
  [`e_charts`](https://echarts4r.john-coene.com/reference/init.md) or a
  proxy as returned by
  [`echarts4rProxy`](https://echarts4r.john-coene.com/reference/echarts4r-shiny.md).

- opening, closing, low, high:

  Stock prices.

- bind:

  Binding between datasets, namely for use of
  [`e_brush`](https://echarts4r.john-coene.com/reference/e_brush.md).

- name:

  name of the serie.

- legend:

  Whether to add serie to legend.

- ...:

  Any other option to pass, check See Also section.

## See also

[Additional
arguments](https://echarts.apache.org/en/option.html#series-candlestick)

## Examples

``` r
date <- c(
  "2017-01-01",
  "2017-01-02",
  "2017-01-03",
  "2017-01-04",
  "2017-03-05",
  "2017-01-06",
  "2017-01-07"
)

stock <- data.frame(
  date = date,
  opening = c(200.60, 200.22, 198.43, 199.05, 203.54, 203.40, 208.34),
  closing = c(200.72, 198.85, 199.05, 203.73, 204.08, 208.11, 211.88),
  low = c(197.82, 198.07, 197.90, 198.10, 202.00, 201.50, 207.60),
  high = c(203.32, 200.67, 200.00, 203.95, 204.90, 208.44, 213.17)
)

stock |>
  e_charts(date) |>
  e_candle(opening, closing, low, high) |>
  e_y_axis(min = 190, max = 220)

{"x":{"theme":"","tl":false,"draw":true,"renderer":"canvas","events":[],"buttons":[],"settings":{"crosstalk_key":null,"crosstalk_group":null},"opts":{"yAxis":[{"show":true,"min":190,"max":220}],"xAxis":[{"data":["2017-01-01","2017-01-02","2017-01-03","2017-01-04","2017-03-05","2017-01-06","2017-01-07"],"type":"category","boundaryGap":true}],"legend":{"data":["candle"]},"series":[{"data":[{"value":[200.6,200.72,197.82,203.32]},{"value":[200.22,198.85,198.07,200.67]},{"value":[198.43,199.05,197.9,200]},{"value":[199.05,203.73,198.1,203.95]},{"value":[203.54,204.08,202,204.9]},{"value":[203.4,208.11,201.5,208.44]},{"value":[208.34,211.88,207.6,213.17]}],"name":"candle","type":"candlestick"}]},"dispose":true},"evals":[],"jsHooks":[]}
```
