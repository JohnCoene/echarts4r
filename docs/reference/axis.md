# Axis

Customise axis.

## Usage

``` r
e_axis(
  e,
  serie,
  axis = c("x", "y", "z"),
  index = 0,
  formatter = NULL,
  margin = 0,
  ...
)

e_axis_(
  e,
  serie = NULL,
  axis = c("x", "y", "z"),
  index = 0,
  formatter = NULL,
  margin = 0,
  ...
)

e_x_axis_(e, serie = NULL, index = 0, formatter = NULL, margin = 0, ...)

e_y_axis_(e, serie = NULL, index = 0, formatter = NULL, margin = 0, ...)

e_z_axis_(e, serie = NULL, index = 0, margin = 0, ...)

e_x_axis(e, serie, index = 0, formatter = NULL, margin = 0, ...)

e_y_axis(e, serie, index = 0, formatter = NULL, margin = 0, ...)

e_z_axis(e, serie, index = 0, margin = 0, ...)

e_rm_axis(e, axis = c("x", "y", "z"))

e_axis_formatter(
  style = c("decimal", "percent", "currency"),
  digits = 0,
  locale = NULL,
  currency = "USD"
)
```

## Arguments

- e:

  An `echarts4r` object as returned by
  [`e_charts`](https://echarts4r.john-coene.com/reference/init.md) or a
  proxy as returned by
  [`echarts4rProxy`](https://echarts4r.john-coene.com/reference/echarts4r-shiny.md).

- serie:

  Column name of serie to range the axis. If used the range of the serie
  is used as, `min` an `max`.

- axis:

  Axis to customise.

- index:

  Index of axis to customise.

- formatter:

  An axis formatter as returned by `e_axis_formatter`.

- margin:

  Margin to apply to `serie`: \\min = serie - margin\\ and \\max =
  serie + margin\\

- ...:

  Any other option to pass, check See Also section.

- style:

  Formatter style, one of `decimal`, `percent`, or `currency`.

- digits:

  Number of decimals.

- locale:

  Locale, if `NULL` then it is inferred from `Sys.getlocale`.

- currency:

  Currency to to display.

## Details

The `e_axis_formatter` may not work in RStudio, open the plot in your
browser. It will display just fine in Rmarkdown and Shiny.

## Functions

- `e_axis` to customise axis

- `e_rm_axis` to remove axis

## See also

[Additional x
arguments](https://echarts.apache.org/en/option.html#xAxis), [Additional
y arguments](https://echarts.apache.org/en/option.html#yAxis)

## Examples

``` r
# range axis based on serie
cars |>
  e_charts(speed) |>
  e_line(dist) |>
  e_x_axis(speed) |>
  e_y_axis(dist)

{"x":{"theme":"","tl":false,"draw":true,"renderer":"canvas","events":[],"buttons":[],"settings":{"crosstalk_key":null,"crosstalk_group":null},"opts":{"yAxis":[{"show":true,"min":2,"max":120}],"xAxis":[{"type":"value","min":4,"max":25}],"legend":{"data":["dist"]},"series":[{"data":[{"value":[4,2]},{"value":[4,10]},{"value":[7,4]},{"value":[7,22]},{"value":[8,16]},{"value":[9,10]},{"value":[10,18]},{"value":[10,26]},{"value":[10,34]},{"value":[11,17]},{"value":[11,28]},{"value":[12,14]},{"value":[12,20]},{"value":[12,24]},{"value":[12,28]},{"value":[13,26]},{"value":[13,34]},{"value":[13,34]},{"value":[13,46]},{"value":[14,26]},{"value":[14,36]},{"value":[14,60]},{"value":[14,80]},{"value":[15,20]},{"value":[15,26]},{"value":[15,54]},{"value":[16,32]},{"value":[16,40]},{"value":[17,32]},{"value":[17,40]},{"value":[17,50]},{"value":[18,42]},{"value":[18,56]},{"value":[18,76]},{"value":[18,84]},{"value":[19,36]},{"value":[19,46]},{"value":[19,68]},{"value":[20,32]},{"value":[20,48]},{"value":[20,52]},{"value":[20,56]},{"value":[20,64]},{"value":[22,66]},{"value":[23,54]},{"value":[24,70]},{"value":[24,92]},{"value":[24,93]},{"value":[24,120]},{"value":[25,85]}],"yAxisIndex":0,"xAxisIndex":0,"name":"dist","type":"line","coordinateSystem":"cartesian2d"}]},"dispose":true},"evals":[],"jsHooks":[]}
# use formatter
cars |>
  dplyr::mutate(
    speed = speed / 25
  ) |>
  e_charts(speed) |>
  e_scatter(dist) |>
  e_y_axis(
    formatter = e_axis_formatter("currency")
  ) |>
  e_x_axis(
    formatter = e_axis_formatter("percent", digits = 0)
  )

{"x":{"theme":"","tl":false,"draw":true,"renderer":"canvas","events":[],"buttons":[],"settings":{"crosstalk_key":null,"crosstalk_group":null},"opts":{"yAxis":[{"show":true,"axisLabel":{"formatter":"function(value, index) {\n        var fmt = new Intl.NumberFormat('en', {\"style\":\"currency\",\"minimumFractionDigits\":0,\"maximumFractionDigits\":0,\"currency\":\"USD\"});\n        return fmt.format(value);\n    }"}}],"xAxis":[{"type":"value","axisLabel":{"formatter":"function(value, index) {\n        var fmt = new Intl.NumberFormat('en', {\"style\":\"percent\",\"minimumFractionDigits\":0,\"maximumFractionDigits\":0,\"currency\":\"USD\"});\n        return fmt.format(value);\n    }"}}],"legend":{"data":["dist"]},"series":[{"data":[{"value":[0.16,2]},{"value":[0.16,10]},{"value":[0.28,4]},{"value":[0.28,22]},{"value":[0.32,16]},{"value":[0.36,10]},{"value":[0.4,18]},{"value":[0.4,26]},{"value":[0.4,34]},{"value":[0.44,17]},{"value":[0.44,28]},{"value":[0.48,14]},{"value":[0.48,20]},{"value":[0.48,24]},{"value":[0.48,28]},{"value":[0.52,26]},{"value":[0.52,34]},{"value":[0.52,34]},{"value":[0.52,46]},{"value":[0.5600000000000001,26]},{"value":[0.5600000000000001,36]},{"value":[0.5600000000000001,60]},{"value":[0.5600000000000001,80]},{"value":[0.6,20]},{"value":[0.6,26]},{"value":[0.6,54]},{"value":[0.64,32]},{"value":[0.64,40]},{"value":[0.68,32]},{"value":[0.68,40]},{"value":[0.68,50]},{"value":[0.72,42]},{"value":[0.72,56]},{"value":[0.72,76]},{"value":[0.72,84]},{"value":[0.76,36]},{"value":[0.76,46]},{"value":[0.76,68]},{"value":[0.8,32]},{"value":[0.8,48]},{"value":[0.8,52]},{"value":[0.8,56]},{"value":[0.8,64]},{"value":[0.88,66]},{"value":[0.92,54]},{"value":[0.96,70]},{"value":[0.96,92]},{"value":[0.96,93]},{"value":[0.96,120]},{"value":[1,85]}],"name":"dist","type":"scatter","symbol":null,"coordinateSystem":"cartesian2d","yAxisIndex":0,"xAxisIndex":0,"symbolSize":3}]},"dispose":true},"evals":["opts.yAxis.0.axisLabel.formatter","opts.xAxis.0.axisLabel.formatter"],"jsHooks":[]}
# plot all labels & rotate
USArrests |>
  head(10) |>
  tibble::rownames_to_column(var = "State") |>
  e_charts(State) |>
  e_area(Murder) |>
  e_x_axis(axisLabel = list(interval = 0, rotate = 45)) # rotate

{"x":{"theme":"","tl":false,"draw":true,"renderer":"canvas","events":[],"buttons":[],"settings":{"crosstalk_key":null,"crosstalk_group":null},"opts":{"yAxis":[{"show":true}],"xAxis":[{"data":["Alabama","Alaska","Arizona","Arkansas","California","Colorado","Connecticut","Delaware","Florida","Georgia"],"type":"category","boundaryGap":true,"axisLabel":{"interval":0,"rotate":45}}],"legend":{"data":["Murder"]},"series":[{"data":[{"value":["Alabama","13.2"]},{"value":["Alaska","10.0"]},{"value":["Arizona"," 8.1"]},{"value":["Arkansas"," 8.8"]},{"value":["California"," 9.0"]},{"value":["Colorado"," 7.9"]},{"value":["Connecticut"," 3.3"]},{"value":["Delaware"," 5.9"]},{"value":["Florida","15.4"]},{"value":["Georgia","17.4"]}],"yAxisIndex":0,"xAxisIndex":0,"name":"Murder","type":"line","coordinateSystem":"cartesian2d","areaStyle":[]}]},"dispose":true},"evals":[],"jsHooks":[]}
```
