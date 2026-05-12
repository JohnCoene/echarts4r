# Bar and Line chart

Add bar serie.

## Usage

``` r
e_bar(
  e,
  serie,
  bind,
  name = NULL,
  legend = TRUE,
  y_index = 0,
  x_index = 0,
  coord_system = "cartesian2d",
  ...
)

e_bar_(
  e,
  serie,
  bind = NULL,
  name = NULL,
  legend = TRUE,
  y_index = 0,
  x_index = 0,
  coord_system = "cartesian2d",
  ...
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

- bind:

  Binding between datasets, namely for use of
  [`e_brush`](https://echarts4r.john-coene.com/reference/e_brush.md).

- name:

  name of the serie.

- legend:

  Whether to add serie to legend.

- x_index, y_index:

  Indexes of x and y axis.

- coord_system:

  Coordinate system to plot against.

- ...:

  Any other option to pass, check See Also section.

## Note

The bar serie expects the data on the x axis to be `categorical` in R
this means a `factor` or `character`. If the data on the x axis is
numeric everything should work well in most cases but strange behaviour
may be observed.

## See also

[Additional
arguments](https://echarts.apache.org/en/option.html#series-bar)

## Examples

``` r
library(dplyr)
#> 
#> Attaching package: 'dplyr'
#> The following objects are masked from 'package:stats':
#> 
#>     filter, lag
#> The following objects are masked from 'package:base':
#> 
#>     intersect, setdiff, setequal, union

mtcars |>
  tibble::rownames_to_column("model") |>
  mutate(total = mpg + qsec) |>
  arrange(desc(total)) |>
  e_charts(model) |>
  e_bar(mpg, stack = "grp") |>
  e_bar(qsec, stack = "grp")

{"x":{"theme":"","tl":false,"draw":true,"renderer":"canvas","events":[],"buttons":[],"settings":{"crosstalk_key":null,"crosstalk_group":null},"opts":{"yAxis":[{"show":true}],"xAxis":[{"data":["Toyota Corolla","Fiat 128","Honda Civic","Lotus Europa","Fiat X1-9","Merc 230","Merc 240D","Porsche 914-2","Toyota Corona","Datsun 710","Hornet 4 Drive","Volvo 142E","Valiant","Mazda RX4 Wag","Merc 280","Mazda RX4","Merc 280C","Pontiac Firebird","Hornet Sportabout","Ferrari Dino","Merc 450SL","Merc 450SE","Merc 450SLC","AMC Javelin","Dodge Challenger","Chrysler Imperial","Ford Pantera L","Duster 360","Maserati Bora","Camaro Z28","Cadillac Fleetwood","Lincoln Continental"],"type":"category","boundaryGap":true}],"legend":{"data":["mpg","qsec"]},"series":[{"data":[{"value":["Toyota Corolla","33.9"]},{"value":["Fiat 128","32.4"]},{"value":["Honda Civic","30.4"]},{"value":["Lotus Europa","30.4"]},{"value":["Fiat X1-9","27.3"]},{"value":["Merc 230","22.8"]},{"value":["Merc 240D","24.4"]},{"value":["Porsche 914-2","26.0"]},{"value":["Toyota Corona","21.5"]},{"value":["Datsun 710","22.8"]},{"value":["Hornet 4 Drive","21.4"]},{"value":["Volvo 142E","21.4"]},{"value":["Valiant","18.1"]},{"value":["Mazda RX4 Wag","21.0"]},{"value":["Merc 280","19.2"]},{"value":["Mazda RX4","21.0"]},{"value":["Merc 280C","17.8"]},{"value":["Pontiac Firebird","19.2"]},{"value":["Hornet Sportabout","18.7"]},{"value":["Ferrari Dino","19.7"]},{"value":["Merc 450SL","17.3"]},{"value":["Merc 450SE","16.4"]},{"value":["Merc 450SLC","15.2"]},{"value":["AMC Javelin","15.2"]},{"value":["Dodge Challenger","15.5"]},{"value":["Chrysler Imperial","14.7"]},{"value":["Ford Pantera L","15.8"]},{"value":["Duster 360","14.3"]},{"value":["Maserati Bora","15.0"]},{"value":["Camaro Z28","13.3"]},{"value":["Cadillac Fleetwood","10.4"]},{"value":["Lincoln Continental","10.4"]}],"name":"mpg","type":"bar","yAxisIndex":0,"xAxisIndex":0,"coordinateSystem":"cartesian2d","stack":"grp"},{"data":[{"value":["Toyota Corolla","19.90"]},{"value":["Fiat 128","19.47"]},{"value":["Honda Civic","18.52"]},{"value":["Lotus Europa","16.90"]},{"value":["Fiat X1-9","18.90"]},{"value":["Merc 230","22.90"]},{"value":["Merc 240D","20.00"]},{"value":["Porsche 914-2","16.70"]},{"value":["Toyota Corona","20.01"]},{"value":["Datsun 710","18.61"]},{"value":["Hornet 4 Drive","19.44"]},{"value":["Volvo 142E","18.60"]},{"value":["Valiant","20.22"]},{"value":["Mazda RX4 Wag","17.02"]},{"value":["Merc 280","18.30"]},{"value":["Mazda RX4","16.46"]},{"value":["Merc 280C","18.90"]},{"value":["Pontiac Firebird","17.05"]},{"value":["Hornet Sportabout","17.02"]},{"value":["Ferrari Dino","15.50"]},{"value":["Merc 450SL","17.60"]},{"value":["Merc 450SE","17.40"]},{"value":["Merc 450SLC","18.00"]},{"value":["AMC Javelin","17.30"]},{"value":["Dodge Challenger","16.87"]},{"value":["Chrysler Imperial","17.42"]},{"value":["Ford Pantera L","14.50"]},{"value":["Duster 360","15.84"]},{"value":["Maserati Bora","14.60"]},{"value":["Camaro Z28","15.41"]},{"value":["Cadillac Fleetwood","17.98"]},{"value":["Lincoln Continental","17.82"]}],"name":"qsec","type":"bar","yAxisIndex":0,"xAxisIndex":0,"coordinateSystem":"cartesian2d","stack":"grp"}]},"dispose":true},"evals":[],"jsHooks":[]}
```
