# Toolbox

Add toolbox interface.

## Usage

``` r
e_toolbox_feature(e, feature, ...)

e_toolbox(e, ...)
```

## Arguments

- e:

  An `echarts4r` object as returned by
  [`e_charts`](https://echarts4r.john-coene.com/reference/init.md) or a
  proxy as returned by
  [`echarts4rProxy`](https://echarts4r.john-coene.com/reference/echarts4r-shiny.md).

- feature:

  Feature to add, defaults to all.

- ...:

  Any other option to pass, check See Also section.

## Details

Valid `feature`:

- `saveAsImage`

- `brush`

- `restore`

- `dataView`

- `dataZoom`

- `magicType`

## See also

[Additional
arguments](https://echarts.apache.org/en/option.html#toolbox)

## Examples

``` r
USArrests |>
  e_charts(UrbanPop) |>
  e_line(Assault) |>
  e_area(Murder, y_index = 1, x_index = 1) |>
  e_datazoom(x_index = 0)

{"x":{"theme":"","tl":false,"draw":true,"renderer":"canvas","events":[],"buttons":[],"settings":{"crosstalk_key":null,"crosstalk_group":null},"opts":{"yAxis":[{"show":true},{"type":"value"}],"xAxis":[{"type":"value"},{"type":"value"}],"legend":{"data":["Assault","Murder"]},"series":[{"data":[{"value":[32,48]},{"value":[39,81]},{"value":[44,259]},{"value":[44,45]},{"value":[45,337]},{"value":[45,86]},{"value":[48,263]},{"value":[48,279]},{"value":[50,190]},{"value":[51,83]},{"value":[52,109]},{"value":[53,109]},{"value":[54,120]},{"value":[56,57]},{"value":[57,56]},{"value":[58,236]},{"value":[59,188]},{"value":[60,211]},{"value":[60,161]},{"value":[62,102]},{"value":[63,156]},{"value":[65,113]},{"value":[66,115]},{"value":[66,249]},{"value":[66,72]},{"value":[66,53]},{"value":[67,300]},{"value":[67,159]},{"value":[68,151]},{"value":[70,178]},{"value":[70,285]},{"value":[72,238]},{"value":[72,106]},{"value":[73,145]},{"value":[74,255]},{"value":[75,120]},{"value":[77,110]},{"value":[78,204]},{"value":[80,294]},{"value":[80,335]},{"value":[80,201]},{"value":[80,120]},{"value":[81,252]},{"value":[83,46]},{"value":[83,249]},{"value":[85,149]},{"value":[86,254]},{"value":[87,174]},{"value":[89,159]},{"value":[91,276]}],"yAxisIndex":0,"xAxisIndex":0,"name":"Assault","type":"line","coordinateSystem":"cartesian2d"},{"data":[{"value":[32,2.2]},{"value":[39,5.7]},{"value":[44,16.1]},{"value":[44,0.8]},{"value":[45,13]},{"value":[45,3.8]},{"value":[48,10]},{"value":[48,14.4]},{"value":[50,8.800000000000001]},{"value":[51,2.1]},{"value":[52,9.699999999999999]},{"value":[53,6]},{"value":[54,2.6]},{"value":[56,2.1]},{"value":[57,2.2]},{"value":[58,13.2]},{"value":[59,13.2]},{"value":[60,17.4]},{"value":[60,6.8]},{"value":[62,4.3]},{"value":[63,8.5]},{"value":[65,7.2]},{"value":[66,6]},{"value":[66,15.4]},{"value":[66,2.7]},{"value":[66,2.6]},{"value":[67,11.3]},{"value":[67,4.9]},{"value":[68,6.6]},{"value":[70,9]},{"value":[70,11.4]},{"value":[72,5.9]},{"value":[72,6.3]},{"value":[73,4]},{"value":[74,12.1]},{"value":[75,7.3]},{"value":[77,3.3]},{"value":[78,7.9]},{"value":[80,8.1]},{"value":[80,15.4]},{"value":[80,12.7]},{"value":[80,3.2]},{"value":[81,12.2]},{"value":[83,5.3]},{"value":[83,10.4]},{"value":[85,4.4]},{"value":[86,11.1]},{"value":[87,3.4]},{"value":[89,7.4]},{"value":[91,9]}],"yAxisIndex":1,"xAxisIndex":1,"name":"Murder","type":"line","coordinateSystem":"cartesian2d","areaStyle":[]}],"dataZoom":[{"xAxisIndex":0}],"toolbox":{"feature":{"dataZoom":[]}}},"dispose":true},"evals":[],"jsHooks":[]}
mtcars |>
  tibble::rownames_to_column("model") |>
  e_charts(model) |>
  e_line(qsec) |>
  e_toolbox() |>
  e_toolbox_feature(
    feature = "magicType",
    type = list("line", "bar")
  )

{"x":{"theme":"","tl":false,"draw":true,"renderer":"canvas","events":[],"buttons":[],"settings":{"crosstalk_key":null,"crosstalk_group":null},"opts":{"yAxis":[{"show":true}],"xAxis":[{"data":["Mazda RX4","Mazda RX4 Wag","Datsun 710","Hornet 4 Drive","Hornet Sportabout","Valiant","Duster 360","Merc 240D","Merc 230","Merc 280","Merc 280C","Merc 450SE","Merc 450SL","Merc 450SLC","Cadillac Fleetwood","Lincoln Continental","Chrysler Imperial","Fiat 128","Honda Civic","Toyota Corolla","Toyota Corona","Dodge Challenger","AMC Javelin","Camaro Z28","Pontiac Firebird","Fiat X1-9","Porsche 914-2","Lotus Europa","Ford Pantera L","Ferrari Dino","Maserati Bora","Volvo 142E"],"type":"category","boundaryGap":true}],"legend":{"data":["qsec"]},"series":[{"data":[{"value":["Mazda RX4","16.46"]},{"value":["Mazda RX4 Wag","17.02"]},{"value":["Datsun 710","18.61"]},{"value":["Hornet 4 Drive","19.44"]},{"value":["Hornet Sportabout","17.02"]},{"value":["Valiant","20.22"]},{"value":["Duster 360","15.84"]},{"value":["Merc 240D","20.00"]},{"value":["Merc 230","22.90"]},{"value":["Merc 280","18.30"]},{"value":["Merc 280C","18.90"]},{"value":["Merc 450SE","17.40"]},{"value":["Merc 450SL","17.60"]},{"value":["Merc 450SLC","18.00"]},{"value":["Cadillac Fleetwood","17.98"]},{"value":["Lincoln Continental","17.82"]},{"value":["Chrysler Imperial","17.42"]},{"value":["Fiat 128","19.47"]},{"value":["Honda Civic","18.52"]},{"value":["Toyota Corolla","19.90"]},{"value":["Toyota Corona","20.01"]},{"value":["Dodge Challenger","16.87"]},{"value":["AMC Javelin","17.30"]},{"value":["Camaro Z28","15.41"]},{"value":["Pontiac Firebird","17.05"]},{"value":["Fiat X1-9","18.90"]},{"value":["Porsche 914-2","16.70"]},{"value":["Lotus Europa","16.90"]},{"value":["Ford Pantera L","14.50"]},{"value":["Ferrari Dino","15.50"]},{"value":["Maserati Bora","14.60"]},{"value":["Volvo 142E","18.60"]}],"yAxisIndex":0,"xAxisIndex":0,"name":"qsec","type":"line","coordinateSystem":"cartesian2d"}],"toolbox":{"feature":{"magicType":{"type":["line","bar"]}}}},"dispose":true},"evals":[],"jsHooks":[]}
```
