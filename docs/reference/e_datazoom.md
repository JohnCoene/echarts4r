# Data zoom

Add data zoom.

## Usage

``` r
e_datazoom(e, x_index = NULL, y_index = NULL, toolbox = TRUE, ...)
```

## Arguments

- e:

  An `echarts4r` object as returned by
  [`e_charts`](https://echarts4r.john-coene.com/reference/init.md) or a
  proxy as returned by
  [`echarts4rProxy`](https://echarts4r.john-coene.com/reference/echarts4r-shiny.md).

- x_index, y_index:

  Indexes of x and y axis.

- toolbox:

  Whether to add the toolbox,
  [`e_toolbox_feature`](https://echarts4r.john-coene.com/reference/toolbox.md),
  (`e_toolbox_feature(e, "dataZoom")`).

- ...:

  Any other option to pass, check See Also section.

## See also

[Additional
arguments](https://echarts.apache.org/en/option.html#dataZoom)

## Examples

``` r
USArrests |>
  e_charts(UrbanPop) |>
  e_line(Assault) |>
  e_area(Murder, y_index = 1, x_index = 1) |>
  e_y_axis(gridIndex = 1) |>
  e_x_axis(gridIndex = 1) |>
  e_grid(height = "35%") |>
  e_grid(height = "35%", top = "50%") |>
  e_toolbox_feature("dataZoom", title = list(zoom = "zoom", back = "back")) |>
  e_datazoom(x_index = c(0, 1))

{"x":{"theme":"","tl":false,"draw":true,"renderer":"canvas","events":[],"buttons":[],"settings":{"crosstalk_key":null,"crosstalk_group":null},"opts":{"yAxis":[{"show":true,"gridIndex":1},{"type":"value"}],"xAxis":[{"type":"value","gridIndex":1},{"type":"value"}],"legend":{"data":["Assault","Murder"]},"series":[{"data":[{"value":[32,48]},{"value":[39,81]},{"value":[44,259]},{"value":[44,45]},{"value":[45,337]},{"value":[45,86]},{"value":[48,263]},{"value":[48,279]},{"value":[50,190]},{"value":[51,83]},{"value":[52,109]},{"value":[53,109]},{"value":[54,120]},{"value":[56,57]},{"value":[57,56]},{"value":[58,236]},{"value":[59,188]},{"value":[60,211]},{"value":[60,161]},{"value":[62,102]},{"value":[63,156]},{"value":[65,113]},{"value":[66,115]},{"value":[66,249]},{"value":[66,72]},{"value":[66,53]},{"value":[67,300]},{"value":[67,159]},{"value":[68,151]},{"value":[70,178]},{"value":[70,285]},{"value":[72,238]},{"value":[72,106]},{"value":[73,145]},{"value":[74,255]},{"value":[75,120]},{"value":[77,110]},{"value":[78,204]},{"value":[80,294]},{"value":[80,335]},{"value":[80,201]},{"value":[80,120]},{"value":[81,252]},{"value":[83,46]},{"value":[83,249]},{"value":[85,149]},{"value":[86,254]},{"value":[87,174]},{"value":[89,159]},{"value":[91,276]}],"yAxisIndex":0,"xAxisIndex":0,"name":"Assault","type":"line","coordinateSystem":"cartesian2d"},{"data":[{"value":[32,2.2]},{"value":[39,5.7]},{"value":[44,16.1]},{"value":[44,0.8]},{"value":[45,13]},{"value":[45,3.8]},{"value":[48,10]},{"value":[48,14.4]},{"value":[50,8.800000000000001]},{"value":[51,2.1]},{"value":[52,9.699999999999999]},{"value":[53,6]},{"value":[54,2.6]},{"value":[56,2.1]},{"value":[57,2.2]},{"value":[58,13.2]},{"value":[59,13.2]},{"value":[60,17.4]},{"value":[60,6.8]},{"value":[62,4.3]},{"value":[63,8.5]},{"value":[65,7.2]},{"value":[66,6]},{"value":[66,15.4]},{"value":[66,2.7]},{"value":[66,2.6]},{"value":[67,11.3]},{"value":[67,4.9]},{"value":[68,6.6]},{"value":[70,9]},{"value":[70,11.4]},{"value":[72,5.9]},{"value":[72,6.3]},{"value":[73,4]},{"value":[74,12.1]},{"value":[75,7.3]},{"value":[77,3.3]},{"value":[78,7.9]},{"value":[80,8.1]},{"value":[80,15.4]},{"value":[80,12.7]},{"value":[80,3.2]},{"value":[81,12.2]},{"value":[83,5.3]},{"value":[83,10.4]},{"value":[85,4.4]},{"value":[86,11.1]},{"value":[87,3.4]},{"value":[89,7.4]},{"value":[91,9]}],"yAxisIndex":1,"xAxisIndex":1,"name":"Murder","type":"line","coordinateSystem":"cartesian2d","areaStyle":[]}],"grid":[{"height":"35%"},{"height":"35%","top":"50%"}],"toolbox":{"feature":{"dataZoom":{"title":{"zoom":"zoom","back":"back"}}}},"dataZoom":[{"xAxisIndex":[0,1]}]},"dispose":true},"evals":[],"jsHooks":[]}
```
