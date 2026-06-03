# Map Actions

Map-related actions.

## Usage

``` r
e_map_select(e, ..., btn = NULL)

e_map_unselect(e, ..., btn = NULL)

e_map_toggle_select(e, ..., btn = NULL)
```

## Arguments

- e:

  An `echarts4r` object as returned by
  [`e_charts`](https://echarts4r.john-coene.com/reference/init.md) or a
  proxy as returned by
  [`echarts4rProxy`](https://echarts4r.john-coene.com/reference/echarts4r-shiny.md).

- ...:

  Any options, see [official
  documentation](https://echarts.apache.org/en/api.html#action.map)

- btn:

  A [`e_button`](https://echarts4r.john-coene.com/reference/e_button.md)
  id.

## See also

[`e_map_register`](https://echarts4r.john-coene.com/reference/e_map_register.md)

## Examples

``` r
choropleth <- data.frame(
  countries = c(
    "France",
    "Brazil",
    "China",
    "Russia",
    "Canada",
    "India",
    "United States",
    "Argentina",
    "Australia"
  ),
  values = round(runif(9, 10, 25))
)

choropleth |>
  e_charts(countries) |>
  e_map(values) |>
  e_visual_map(min = 10, max = 25) |>
  e_map_toggle_select(name = "China", btn = "btn") |>
  e_button("btn", "Select China")
Select China

{"x":{"theme":"","tl":false,"draw":true,"renderer":"canvas","events":[],"buttons":{"btn":[{"id":"btn","data":{"type":"mapToggleSelect","name":"China"}}]},"settings":{"crosstalk_key":null,"crosstalk_group":null},"opts":{"series":[{"type":"map","map":"world","name":"values","data":[{"value":24,"name":"France"},{"value":13,"name":"Brazil"},{"value":10,"name":"China"},{"value":23,"name":"Russia"},{"value":15,"name":"Canada"},{"value":10,"name":"India"},{"value":17,"name":"United States"},{"value":11,"name":"Argentina"},{"value":17,"name":"Australia"}]}],"visualMap":[{"min":10,"max":25,"calculable":true,"type":"continuous"}]},"dispose":true},"evals":[],"jsHooks":[]}
```
