# Themes

Add a custom theme or apply a pre-built one.

## Usage

``` r
e_theme(
  e,
  name = c("auritus", "azul", "bee-inspired", "blue", "caravan", "carp", "chalk", "cool",
    "dark", "dark-blue", "dark-bold", "dark-digerati", "dark-fresh-cut", "dark-mushroom",
    "eduardo", "essos", "forest", "fresh-cut", "fruit", "gray", "green", "halloween",
    "helianthus", "infographic", "inspired", "jazz", "london", "macarons", "macarons2",
    "mint", "purple-passion", "rainbow", "red", "red-velvet", "roma", "royal", "sakura",
    "shine", "tech-blue", "v5", "vintage", "walden", "wef", "weforum", "westeros",
    "wonderland")
)

e_theme_custom(e, theme, name = "custom")

e_theme_register(theme, name = "custom")
```

## Arguments

- e:

  An `echarts4r` object as returned by
  [`e_charts`](https://echarts4r.john-coene.com/reference/init.md) or a
  proxy as returned by
  [`echarts4rProxy`](https://echarts4r.john-coene.com/reference/echarts4r-shiny.md).

- name:

  Name of theme.

- theme:

  Theme, A json string or a see below.

## Details

The function `e_theme_register` can be used to register the theme
globally in R markdown or shiny (UI). This is useful because 1) the
`e_theme_custom` registers the theme every time and is more
computationally expensive.

## Functions

- `e_theme` - Use a default theme by name.

- `e_theme_custom` - Use a custom theme.

- `e_theme_register` - Register a theme globally in shiny or R markdown.

## See also

[create your own
theme](https://echarts.apache.org/en/theme-builder.html).

## Examples

``` r
mtcars |>
  e_charts(mpg) |>
  e_line(disp) |>
  e_area(hp) |>
  e_x_axis(min = 10) -> p

p |> e_theme("chalk")

{"x":{"theme":"chalk","tl":false,"draw":true,"renderer":"canvas","events":[],"buttons":[],"settings":{"crosstalk_key":null,"crosstalk_group":null},"opts":{"yAxis":[{"show":true}],"xAxis":[{"type":"value","min":10}],"legend":{"data":["disp","hp"]},"series":[{"data":[{"value":[10.4,472]},{"value":[10.4,460]},{"value":[13.3,350]},{"value":[14.3,360]},{"value":[14.7,440]},{"value":[15,301]},{"value":[15.2,275.8]},{"value":[15.2,304]},{"value":[15.5,318]},{"value":[15.8,351]},{"value":[16.4,275.8]},{"value":[17.3,275.8]},{"value":[17.8,167.6]},{"value":[18.1,225]},{"value":[18.7,360]},{"value":[19.2,167.6]},{"value":[19.2,400]},{"value":[19.7,145]},{"value":[21,160]},{"value":[21,160]},{"value":[21.4,258]},{"value":[21.4,121]},{"value":[21.5,120.1]},{"value":[22.8,108]},{"value":[22.8,140.8]},{"value":[24.4,146.7]},{"value":[26,120.3]},{"value":[27.3,79]},{"value":[30.4,75.7]},{"value":[30.4,95.09999999999999]},{"value":[32.4,78.7]},{"value":[33.9,71.09999999999999]}],"yAxisIndex":0,"xAxisIndex":0,"name":"disp","type":"line","coordinateSystem":"cartesian2d"},{"data":[{"value":[10.4,205]},{"value":[10.4,215]},{"value":[13.3,245]},{"value":[14.3,245]},{"value":[14.7,230]},{"value":[15,335]},{"value":[15.2,180]},{"value":[15.2,150]},{"value":[15.5,150]},{"value":[15.8,264]},{"value":[16.4,180]},{"value":[17.3,180]},{"value":[17.8,123]},{"value":[18.1,105]},{"value":[18.7,175]},{"value":[19.2,123]},{"value":[19.2,175]},{"value":[19.7,175]},{"value":[21,110]},{"value":[21,110]},{"value":[21.4,110]},{"value":[21.4,109]},{"value":[21.5,97]},{"value":[22.8,93]},{"value":[22.8,95]},{"value":[24.4,62]},{"value":[26,91]},{"value":[27.3,66]},{"value":[30.4,52]},{"value":[30.4,113]},{"value":[32.4,66]},{"value":[33.9,65]}],"yAxisIndex":0,"xAxisIndex":0,"name":"hp","type":"line","coordinateSystem":"cartesian2d","areaStyle":[]}]},"dispose":true},"evals":[],"jsHooks":[]}p |> e_theme_custom('{"color":["#ff715e","#ffaf51"]}')

{"x":{"theme":"custom","tl":false,"draw":true,"renderer":"canvas","events":[],"buttons":[],"settings":{"crosstalk_key":null,"crosstalk_group":null},"opts":{"yAxis":[{"show":true}],"xAxis":[{"type":"value","min":10}],"legend":{"data":["disp","hp"]},"series":[{"data":[{"value":[10.4,472]},{"value":[10.4,460]},{"value":[13.3,350]},{"value":[14.3,360]},{"value":[14.7,440]},{"value":[15,301]},{"value":[15.2,275.8]},{"value":[15.2,304]},{"value":[15.5,318]},{"value":[15.8,351]},{"value":[16.4,275.8]},{"value":[17.3,275.8]},{"value":[17.8,167.6]},{"value":[18.1,225]},{"value":[18.7,360]},{"value":[19.2,167.6]},{"value":[19.2,400]},{"value":[19.7,145]},{"value":[21,160]},{"value":[21,160]},{"value":[21.4,258]},{"value":[21.4,121]},{"value":[21.5,120.1]},{"value":[22.8,108]},{"value":[22.8,140.8]},{"value":[24.4,146.7]},{"value":[26,120.3]},{"value":[27.3,79]},{"value":[30.4,75.7]},{"value":[30.4,95.09999999999999]},{"value":[32.4,78.7]},{"value":[33.9,71.09999999999999]}],"yAxisIndex":0,"xAxisIndex":0,"name":"disp","type":"line","coordinateSystem":"cartesian2d"},{"data":[{"value":[10.4,205]},{"value":[10.4,215]},{"value":[13.3,245]},{"value":[14.3,245]},{"value":[14.7,230]},{"value":[15,335]},{"value":[15.2,180]},{"value":[15.2,150]},{"value":[15.5,150]},{"value":[15.8,264]},{"value":[16.4,180]},{"value":[17.3,180]},{"value":[17.8,123]},{"value":[18.1,105]},{"value":[18.7,175]},{"value":[19.2,123]},{"value":[19.2,175]},{"value":[19.7,175]},{"value":[21,110]},{"value":[21,110]},{"value":[21.4,110]},{"value":[21.4,109]},{"value":[21.5,97]},{"value":[22.8,93]},{"value":[22.8,95]},{"value":[24.4,62]},{"value":[26,91]},{"value":[27.3,66]},{"value":[30.4,52]},{"value":[30.4,113]},{"value":[32.4,66]},{"value":[33.9,65]}],"yAxisIndex":0,"xAxisIndex":0,"name":"hp","type":"line","coordinateSystem":"cartesian2d","areaStyle":[]}]},"dispose":true,"theme2":true,"customTheme":"{\"color\":[\"#ff715e\",\"#ffaf51\"]}","theme_name":"custom"},"evals":[],"jsHooks":[]}
```
