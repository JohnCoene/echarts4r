# Geo 3D

Initialise geo 3D.

## Usage

``` r
e_geo_3d(e, serie, color, type = "world", rm_x = TRUE, rm_y = TRUE, ...)

e_geo_3d_(
  e,
  serie = NULL,
  color = NULL,
  type = "world",
  rm_x = TRUE,
  rm_y = TRUE,
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

- color:

  Color.

- type:

  Map type.

- rm_x, rm_y:

  Whether to remove x and y axis, defaults to `TRUE`.

- ...:

  Any other option to pass, check See Also section.

## See also

[`e_country_names`](https://echarts4r.john-coene.com/reference/e_country_names.md),
[Additional
arguments](https://echarts.apache.org/en/option-gl.html#geo3D)

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
  height = runif(9, 1, 5),
  color = c(
    "#F7FBFF",
    "#DEEBF7",
    "#C6DBEF",
    "#9ECAE1",
    "#6BAED6",
    "#4292C6",
    "#2171B5",
    "#08519C",
    "#08306B"
  )
)

choropleth |>
  e_charts(countries) |>
  e_geo_3d(height, color)

{"x":{"theme":"","tl":false,"draw":true,"renderer":"canvas","events":[],"buttons":[],"settings":{"crosstalk_key":null,"crosstalk_group":null},"opts":{"geo3D":{"map":"world","regions":[{"name":"France","height":"4.098825","itemStyle":{"color":"#F7FBFF"}},{"name":"Brazil","height":"1.321764","itemStyle":{"color":"#DEEBF7"}},{"name":"China","height":"4.855115","itemStyle":{"color":"#C6DBEF"}},{"name":"Russia","height":"3.393752","itemStyle":{"color":"#9ECAE1"}},{"name":"Canada","height":"1.588801","itemStyle":{"color":"#6BAED6"}},{"name":"India","height":"4.320120","itemStyle":{"color":"#4292C6"}},{"name":"United States","height":"2.227112","itemStyle":{"color":"#2171B5"}},{"name":"Argentina","height":"1.629966","itemStyle":{"color":"#08519C"}},{"name":"Australia","height":"4.759758","itemStyle":{"color":"#08306B"}}]}},"dispose":true},"evals":[],"jsHooks":[]}
```
