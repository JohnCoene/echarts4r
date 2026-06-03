# Zoom

Zoom on a region.

## Usage

``` r
e_zoom(e, ..., btn = NULL)
```

## Arguments

- e:

  An `echarts4r` object as returned by
  [`e_charts`](https://echarts4r.john-coene.com/reference/init.md) or a
  proxy as returned by
  [`echarts4rProxy`](https://echarts4r.john-coene.com/reference/echarts4r-shiny.md).

- ...:

  Any options, see [official
  documentation](https://echarts.apache.org/en/api.html#action.dataZoom.dataZoom)

- btn:

  A [`e_button`](https://echarts4r.john-coene.com/reference/e_button.md)
  id.

## Examples

``` r
cars |>
  e_charts(dist) |>
  e_scatter(speed) |>
  e_datazoom() |>
  e_zoom(
    dataZoomIndex = 0,
    start = 20,
    end = 40,
    btn = "BUTTON"
  ) |>
  e_button("BUTTON", "Zoom in")
Zoom in

{"x":{"theme":"","tl":false,"draw":true,"renderer":"canvas","events":[],"buttons":{"BUTTON":[{"data":{"type":"dataZoom","dataZoomIndex":0,"start":20,"end":40}}]},"settings":{"crosstalk_key":null,"crosstalk_group":null},"opts":{"yAxis":[{"show":true}],"xAxis":[{"type":"value"}],"legend":{"data":["speed"]},"series":[{"data":[{"value":[2,4]},{"value":[4,7]},{"value":[10,4]},{"value":[10,9]},{"value":[14,12]},{"value":[16,8]},{"value":[17,11]},{"value":[18,10]},{"value":[20,12]},{"value":[20,15]},{"value":[22,7]},{"value":[24,12]},{"value":[26,10]},{"value":[26,13]},{"value":[26,14]},{"value":[26,15]},{"value":[28,11]},{"value":[28,12]},{"value":[32,16]},{"value":[32,17]},{"value":[32,20]},{"value":[34,10]},{"value":[34,13]},{"value":[34,13]},{"value":[36,14]},{"value":[36,19]},{"value":[40,16]},{"value":[40,17]},{"value":[42,18]},{"value":[46,13]},{"value":[46,19]},{"value":[48,20]},{"value":[50,17]},{"value":[52,20]},{"value":[54,15]},{"value":[54,23]},{"value":[56,18]},{"value":[56,20]},{"value":[60,14]},{"value":[64,20]},{"value":[66,22]},{"value":[68,19]},{"value":[70,24]},{"value":[76,18]},{"value":[80,14]},{"value":[84,18]},{"value":[85,25]},{"value":[92,24]},{"value":[93,24]},{"value":[120,24]}],"name":"speed","type":"scatter","symbol":null,"coordinateSystem":"cartesian2d","yAxisIndex":0,"xAxisIndex":0,"symbolSize":3}],"dataZoom":[[]],"toolbox":{"feature":{"dataZoom":[]}}},"dispose":true},"evals":[],"jsHooks":[]}
```
