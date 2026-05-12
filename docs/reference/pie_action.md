# Select & Unselect Pie

Actions related to
[`e_pie`](https://echarts4r.john-coene.com/reference/e_pie.md).

## Usage

``` r
e_pie_select(e, ..., btn = NULL)

e_pie_unselect(e, ..., btn = NULL)
```

## Arguments

- e:

  An `echarts4r` object as returned by
  [`e_charts`](https://echarts4r.john-coene.com/reference/init.md) or a
  proxy as returned by
  [`echarts4rProxy`](https://echarts4r.john-coene.com/reference/echarts4r-shiny.md).

- ...:

  Any options, see [official
  documentation](https://echarts.apache.org/en/api.html#action.pie)

- btn:

  A [`e_button`](https://echarts4r.john-coene.com/reference/e_button.md)
  id.

## Examples

``` r
mtcars |>
  head() |>
  tibble::rownames_to_column("model") |>
  e_charts(model) |>
  e_pie(carb) |>
  e_pie_select(dataIndex = 0)

{"x":{"theme":"","tl":false,"draw":true,"renderer":"canvas","events":[{"data":{"type":"pieSelect","dataIndex":0}}],"buttons":[],"settings":{"crosstalk_key":null,"crosstalk_group":null},"opts":{"legend":{"data":["Mazda RX4","Mazda RX4 Wag","Datsun 710","Hornet 4 Drive","Hornet Sportabout","Valiant"]},"series":[{"name":"carb","type":"pie","data":[{"value":4,"name":"Mazda RX4"},{"value":4,"name":"Mazda RX4 Wag"},{"value":1,"name":"Datsun 710"},{"value":1,"name":"Hornet 4 Drive"},{"value":2,"name":"Hornet Sportabout"},{"value":1,"name":"Valiant"}]}]},"dispose":true},"evals":[],"jsHooks":[]}
```
