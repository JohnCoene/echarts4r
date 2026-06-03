# Insert binded data

Utility function to attach an existing column(s) from your data to an
existing series

## Usage

``` r
e_insert_data(e, .serie, .data)
```

## Arguments

- e:

  An `echarts4r` object as returned by
  [`e_charts`](https://echarts4r.john-coene.com/reference/init.md) or a
  proxy as returned by
  [`echarts4rProxy`](https://echarts4r.john-coene.com/reference/echarts4r-shiny.md).

- .serie:

  Serie's index to add the data.

- .data:

  Column names for the new data.

## Details

This inserts new data into a series's values which allows for use in
mapping things like e_visual_map or tooltip parameters. This function
also works when using timelines.

## Examples

``` r
mtcars |>
 e_charts(mpg) |>
 e_scatter(wt, scale = e_scale) |>
 e_insert_data(.serie = 1, .data = "qsec") |>
 e_visual_map(qsec, scale = e_scale, dimension = 2) |> 
 e_tooltip(trigger = "item", 
           formatter = htmlwidgets::JS("function(params) {
                                       return 'mpg: ' + params.value[0] + 
                                       '<br />wt: ' + params.value[1] +
                                       '<br />qsec: ' + params.value[2];
                                       }")
 ) 

{"x":{"theme":"","tl":false,"draw":true,"renderer":"canvas","events":[],"buttons":[],"settings":{"crosstalk_key":null,"crosstalk_group":null},"opts":{"yAxis":[{"show":true}],"xAxis":[{"type":"value"}],"legend":{"data":["wt"]},"series":[{"data":[{"value":[10.4,5.25,17.98]},{"value":[10.4,5.424,17.82]},{"value":[13.3,3.84,15.41]},{"value":[14.3,3.57,15.84]},{"value":[14.7,5.345,17.42]},{"value":[15,3.57,14.6]},{"value":[15.2,3.78,18]},{"value":[15.2,3.435,17.3]},{"value":[15.5,3.52,16.87]},{"value":[15.8,3.17,14.5]},{"value":[16.4,4.07,17.4]},{"value":[17.3,3.73,17.6]},{"value":[17.8,3.44,18.9]},{"value":[18.1,3.46,20.22]},{"value":[18.7,3.44,17.02]},{"value":[19.2,3.44,18.3]},{"value":[19.2,3.845,17.05]},{"value":[19.7,2.77,15.5]},{"value":[21,2.62,16.46]},{"value":[21,2.875,17.02]},{"value":[21.4,3.215,19.44]},{"value":[21.4,2.78,18.6]},{"value":[21.5,2.465,20.01]},{"value":[22.8,2.32,18.61]},{"value":[22.8,3.15,22.9]},{"value":[24.4,3.19,20]},{"value":[26,2.14,16.7]},{"value":[27.3,1.935,18.9]},{"value":[30.4,1.615,18.52]},{"value":[30.4,1.513,16.9]},{"value":[32.4,2.2,19.47]},{"value":[33.9,1.835,19.9]}],"name":"wt","type":"scatter","symbol":null,"coordinateSystem":"cartesian2d","yAxisIndex":0,"xAxisIndex":0,"symbolSize":3}],"visualMap":[{"dimension":2,"calculable":true,"type":"continuous","min":1,"max":20}],"tooltip":{"trigger":"item","formatter":"function(params) {\n                                       return 'mpg: ' + params.value[0] + \n                                       '<br />wt: ' + params.value[1] +\n                                       '<br />qsec: ' + params.value[2];\n                                       }"}},"dispose":true},"evals":["opts.tooltip.formatter"],"jsHooks":[]}
```
