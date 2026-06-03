# Tooltip

Customise tooltip

## Usage

``` r
e_tooltip(e, trigger = c("item", "axis"), formatter = NULL, ...)

e_tooltip_item_formatter(
  style = c("decimal", "percent", "currency"),
  digits = 0,
  locale = NULL,
  currency = "USD"
)

e_tooltip_choro_formatter(
  style = c("decimal", "percent", "currency"),
  digits = 0,
  locale = NULL,
  currency = "USD"
)

e_tooltip_pie_formatter(
  style = c("decimal", "percent", "currency"),
  digits = 0,
  locale = NULL,
  currency = "USD",
  ...
)

e_tooltip_pointer_formatter(
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

- trigger:

  What triggers the tooltip, one of `item` or `axis`.

- formatter:

  Item and Pointer formatter as returned by `e_tooltip_item_formatter`,
  `e_tooltip_pointer_formatter`, `e_tooltip_pie_formatter`.

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

## Formatters

- `e_tooltip_pie_formatter`: special helper for
  [`e_pie`](https://echarts4r.john-coene.com/reference/e_pie.md).

- `e_tooltip_item_formatter`: general helper, this is passed to the
  [tooltip
  `formatter`](https://echarts.apache.org/en/option.html#tooltip.formatter).

- `e_tooltip_pointer_formatter`: helper for pointer, this is passed to
  the [`label` parameter under
  `axisPointer`](https://echarts.apache.org/en/option.html#tooltip.axisPointer.label).

## See also

[Additional
arguments](https://echarts.apache.org/en/option.html#tooltip)

## Examples

``` r
# basic
USArrests |>
  e_charts(Assault) |>
  e_scatter(Murder) |>
  e_tooltip()

{"x":{"theme":"","tl":false,"draw":true,"renderer":"canvas","events":[],"buttons":[],"settings":{"crosstalk_key":null,"crosstalk_group":null},"opts":{"yAxis":[{"show":true}],"xAxis":[{"type":"value"}],"legend":{"data":["Murder"]},"series":[{"data":[{"value":[45,0.8]},{"value":[46,5.3]},{"value":[48,2.2]},{"value":[53,2.6]},{"value":[56,2.2]},{"value":[57,2.1]},{"value":[72,2.7]},{"value":[81,5.7]},{"value":[83,2.1]},{"value":[86,3.8]},{"value":[102,4.3]},{"value":[106,6.3]},{"value":[109,9.699999999999999]},{"value":[109,6]},{"value":[110,3.3]},{"value":[113,7.2]},{"value":[115,6]},{"value":[120,2.6]},{"value":[120,7.3]},{"value":[120,3.2]},{"value":[145,4]},{"value":[149,4.4]},{"value":[151,6.6]},{"value":[156,8.5]},{"value":[159,7.4]},{"value":[159,4.9]},{"value":[161,6.8]},{"value":[174,3.4]},{"value":[178,9]},{"value":[188,13.2]},{"value":[190,8.800000000000001]},{"value":[201,12.7]},{"value":[204,7.9]},{"value":[211,17.4]},{"value":[236,13.2]},{"value":[238,5.9]},{"value":[249,10.4]},{"value":[249,15.4]},{"value":[252,12.2]},{"value":[254,11.1]},{"value":[255,12.1]},{"value":[259,16.1]},{"value":[263,10]},{"value":[276,9]},{"value":[279,14.4]},{"value":[285,11.4]},{"value":[294,8.1]},{"value":[300,11.3]},{"value":[335,15.4]},{"value":[337,13]}],"name":"Murder","type":"scatter","symbol":null,"coordinateSystem":"cartesian2d","yAxisIndex":0,"xAxisIndex":0,"symbolSize":3}],"tooltip":{"trigger":"item"}},"dispose":true},"evals":[],"jsHooks":[]}
# formatter
cars |>
  dplyr::mutate(
    dist = dist / 120
  ) |>
  e_charts(speed) |>
  e_scatter(dist, symbol_size = 5) |>
  e_tooltip(
    formatter = e_tooltip_item_formatter("percent")
  )

{"x":{"theme":"","tl":false,"draw":true,"renderer":"canvas","events":[],"buttons":[],"settings":{"crosstalk_key":null,"crosstalk_group":null},"opts":{"yAxis":[{"show":true}],"xAxis":[{"type":"value"}],"legend":{"data":["dist"]},"series":[{"data":[{"value":[4,0.01666666666666667]},{"value":[4,0.08333333333333333]},{"value":[7,0.03333333333333333]},{"value":[7,0.1833333333333333]},{"value":[8,0.1333333333333333]},{"value":[9,0.08333333333333333]},{"value":[10,0.15]},{"value":[10,0.2166666666666667]},{"value":[10,0.2833333333333333]},{"value":[11,0.1416666666666667]},{"value":[11,0.2333333333333333]},{"value":[12,0.1166666666666667]},{"value":[12,0.1666666666666667]},{"value":[12,0.2]},{"value":[12,0.2333333333333333]},{"value":[13,0.2166666666666667]},{"value":[13,0.2833333333333333]},{"value":[13,0.2833333333333333]},{"value":[13,0.3833333333333334]},{"value":[14,0.2166666666666667]},{"value":[14,0.3]},{"value":[14,0.5]},{"value":[14,0.6666666666666666]},{"value":[15,0.1666666666666667]},{"value":[15,0.2166666666666667]},{"value":[15,0.45]},{"value":[16,0.2666666666666667]},{"value":[16,0.3333333333333333]},{"value":[17,0.2666666666666667]},{"value":[17,0.3333333333333333]},{"value":[17,0.4166666666666667]},{"value":[18,0.35]},{"value":[18,0.4666666666666667]},{"value":[18,0.6333333333333333]},{"value":[18,0.7]},{"value":[19,0.3]},{"value":[19,0.3833333333333334]},{"value":[19,0.5666666666666667]},{"value":[20,0.2666666666666667]},{"value":[20,0.4]},{"value":[20,0.4333333333333333]},{"value":[20,0.4666666666666667]},{"value":[20,0.5333333333333333]},{"value":[22,0.55]},{"value":[23,0.45]},{"value":[24,0.5833333333333334]},{"value":[24,0.7666666666666667]},{"value":[24,0.775]},{"value":[24,1]},{"value":[25,0.7083333333333334]}],"name":"dist","type":"scatter","symbol":null,"coordinateSystem":"cartesian2d","yAxisIndex":0,"xAxisIndex":0,"symbolSize":5}],"tooltip":{"trigger":"item","formatter":"function(params, ticket, callback) {\n        var fmt = new Intl.NumberFormat('en', {\"style\":\"percent\",\"minimumFractionDigits\":0,\"maximumFractionDigits\":0,\"currency\":\"USD\"});\n        var idx = 0;\n        if (params.name == params.value[0]) {\n            idx = 1;\n        }\n        return params.marker + ' ' +\n               params.seriesName + ': ' + fmt.format(parseFloat(params.value[idx]));\n    }"}},"dispose":true},"evals":["opts.tooltip.formatter"],"jsHooks":[]}
# axis pointer
cars |>
  e_charts(speed) |>
  e_scatter(dist, symbol_size = 5) |>
  e_tooltip(
    formatter = e_tooltip_pointer_formatter("currency"),
    axisPointer = list(
      type = "cross"
    )
  )

{"x":{"theme":"","tl":false,"draw":true,"renderer":"canvas","events":[],"buttons":[],"settings":{"crosstalk_key":null,"crosstalk_group":null},"opts":{"yAxis":[{"show":true}],"xAxis":[{"type":"value"}],"legend":{"data":["dist"]},"series":[{"data":[{"value":[4,2]},{"value":[4,10]},{"value":[7,4]},{"value":[7,22]},{"value":[8,16]},{"value":[9,10]},{"value":[10,18]},{"value":[10,26]},{"value":[10,34]},{"value":[11,17]},{"value":[11,28]},{"value":[12,14]},{"value":[12,20]},{"value":[12,24]},{"value":[12,28]},{"value":[13,26]},{"value":[13,34]},{"value":[13,34]},{"value":[13,46]},{"value":[14,26]},{"value":[14,36]},{"value":[14,60]},{"value":[14,80]},{"value":[15,20]},{"value":[15,26]},{"value":[15,54]},{"value":[16,32]},{"value":[16,40]},{"value":[17,32]},{"value":[17,40]},{"value":[17,50]},{"value":[18,42]},{"value":[18,56]},{"value":[18,76]},{"value":[18,84]},{"value":[19,36]},{"value":[19,46]},{"value":[19,68]},{"value":[20,32]},{"value":[20,48]},{"value":[20,52]},{"value":[20,56]},{"value":[20,64]},{"value":[22,66]},{"value":[23,54]},{"value":[24,70]},{"value":[24,92]},{"value":[24,93]},{"value":[24,120]},{"value":[25,85]}],"name":"dist","type":"scatter","symbol":null,"coordinateSystem":"cartesian2d","yAxisIndex":0,"xAxisIndex":0,"symbolSize":5}],"tooltip":{"trigger":"item","axisPointer":{"type":"cross","label":"function(params, ticket, callback) {\n        var fmt = new Intl.NumberFormat('en', {\"style\":\"currency\",\"minimumFractionDigits\":0,\"maximumFractionDigits\":0,\"currency\":\"USD\"});\n        var res = params[0].value[0];\n        for (i = 0; i < params.length; i++) {\n            res += '<br />' +\n                   params[i].marker + ' ' +\n                   params[i].seriesName + ': ' +\n                   fmt.format(parseFloat(params[i].value[1]));\n        }\n        return res;\n    }"},"formatter":"function(params, ticket, callback) {\n        var fmt = new Intl.NumberFormat('en', {\"style\":\"currency\",\"minimumFractionDigits\":0,\"maximumFractionDigits\":0,\"currency\":\"USD\"});\n        var res = params[0].value[0];\n        for (i = 0; i < params.length; i++) {\n            res += '<br />' +\n                   params[i].marker + ' ' +\n                   params[i].seriesName + ': ' +\n                   fmt.format(parseFloat(params[i].value[1]));\n        }\n        return res;\n    }"}},"dispose":true},"evals":["opts.tooltip.axisPointer.label","opts.tooltip.formatter"],"jsHooks":[]}
```
