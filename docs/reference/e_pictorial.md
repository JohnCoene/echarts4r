# Pictorial

Pictorial bar chart is a type of bar chart that customized glyph (like
images, SVG PathData) can be used instead of rectangular bar.

## Usage

``` r
e_pictorial(
  e,
  serie,
  symbol,
  bind,
  name = NULL,
  legend = TRUE,
  y_index = 0,
  x_index = 0,
  ...
)

e_pictorial_(
  e,
  serie,
  symbol,
  bind = NULL,
  name = NULL,
  legend = TRUE,
  y_index = 0,
  x_index = 0,
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

- symbol:

  Symbol to plot.

- bind:

  Binding between datasets, namely for use of
  [`e_brush`](https://echarts4r.john-coene.com/reference/e_brush.md).

- name:

  name of the serie.

- legend:

  Whether to add serie to legend.

- x_index, y_index:

  Indexes of x and y axis.

- ...:

  Any other option to pass, check See Also section.

## Symbols

- Built-in:

  `circle`, `rect`, `roundRect`, `triangle`, `diamond`, `pin`, `arrow`.

- SVG Path:

  Path data for SVG graphics.

- Images:

  Path to image, don't forget to precede it with `image://`, see
  examples.

## See also

[Additional
arguments](https://echarts.apache.org/en/option.html#series-pictorialBar)

## Examples

``` r
# built-in symbols
y <- rnorm(10, 10, 2)
df <- data.frame(
  x = 1:10,
  y = y,
  z = y - rnorm(10, 5, 1)
)

df |>
  e_charts(x) |>
  e_bar(z, barWidth = 10) |>
  e_pictorial(
    y,
    symbol = "rect",
    symbolRepeat = TRUE,
    z = -1,
    symbolSize = c(10, 4)
  ) |>
  e_theme("westeros")

{"x":{"theme":"westeros","tl":false,"draw":true,"renderer":"canvas","events":[],"buttons":[],"settings":{"crosstalk_key":null,"crosstalk_group":null},"opts":{"yAxis":[{"show":true}],"xAxis":[{"type":"value"}],"legend":{"data":["z","y"]},"series":[{"data":[{"value":[1,3.13913730106914]},{"value":[2,5.526583594204411]},{"value":[3,5.166374678886444]},{"value":[4,6.212644157447046]},{"value":[5,4.457765667252596]},{"value":[6,3.177905647013128]},{"value":[7,2.27819846498791]},{"value":[8,10.30428232865397]},{"value":[9,7.870460254311175]},{"value":[10,4.692378155430389]}],"name":"z","type":"bar","yAxisIndex":0,"xAxisIndex":0,"coordinateSystem":"cartesian2d","barWidth":10},{"data":[{"value":[1,7.937013850252923]},{"value":[2,10.82402762236756]},{"value":[3,10.1229170786158]},{"value":[4,9.945613402148638]},{"value":[5,9.739791115353277]},{"value":[6,7.051541769010236]},{"value":[7,7.188374405855072]},{"value":[8,13.98277623885222]},{"value":[9,9.182607401599519]},{"value":[10,8.675097907658966]}],"name":"y","type":"pictorialBar","yAxisIndex":0,"xAxisIndex":0,"symbolRepeat":true,"z":-1,"symbolSize":[10,4],"symbol":"rect"}]},"dispose":true},"evals":[],"jsHooks":[]}
# svg path
path <- "path://M0,10 L10,10 C5.5,10 5.5,5 5,0 C4.5,5 4.5,10 0,10 z"

style <- list(
  normal = list(opacity = 0.5),
  # normal
  emphasis = list(opacity = 1) # on hover
)

df |>
  e_charts(x) |>
  e_pictorial(
    y,
    symbol = path,
    barCategoryGap = "-130%",
    itemStyle = style
  )

{"x":{"theme":"","tl":false,"draw":true,"renderer":"canvas","events":[],"buttons":[],"settings":{"crosstalk_key":null,"crosstalk_group":null},"opts":{"yAxis":[{"show":true}],"xAxis":[{"type":"value"}],"legend":{"data":["y"]},"series":[{"data":[{"value":[1,7.937013850252923]},{"value":[2,10.82402762236756]},{"value":[3,10.1229170786158]},{"value":[4,9.945613402148638]},{"value":[5,9.739791115353277]},{"value":[6,7.051541769010236]},{"value":[7,7.188374405855072]},{"value":[8,13.98277623885222]},{"value":[9,9.182607401599519]},{"value":[10,8.675097907658966]}],"name":"y","type":"pictorialBar","yAxisIndex":0,"xAxisIndex":0,"barCategoryGap":"-130%","itemStyle":{"normal":{"opacity":0.5},"emphasis":{"opacity":1}},"symbol":"path://M0,10 L10,10 C5.5,10 5.5,5 5,0 C4.5,5 4.5,10 0,10 z"}]},"dispose":true},"evals":[],"jsHooks":[]}
# image
# might not work in RStudio viewer
# open in browser
qomo <- paste0(
  "https://ecomfe.github.io/echarts-examples/public/",
  "data/asset/img/hill-Qomolangma.png"
)

kili <- paste0(
  "https://ecomfe.github.io/echarts-examples/public/",
  "data/asset/img/hill-Kilimanjaro.png"
)

data <- data.frame(
  x = c("Qomolangma", "Kilimanjaro"),
  value = c(8844, 5895),
  symbol = c(
    paste0("image://", qomo),
    paste0("image://", kili)
  )
)

data |>
  e_charts(x) |>
  e_pictorial(value, symbol) |>
  e_legend(FALSE)

{"x":{"theme":"","tl":false,"draw":true,"renderer":"canvas","events":[],"buttons":[],"settings":{"crosstalk_key":null,"crosstalk_group":null},"opts":{"yAxis":[{"show":true}],"xAxis":[{"data":["Qomolangma","Kilimanjaro"],"type":"category","boundaryGap":true}],"legend":{"data":["value"],"show":false,"type":"plain"},"series":[{"data":[{"value":["Qomolangma","8844"],"symbol":"image://https://ecomfe.github.io/echarts-examples/public/data/asset/img/hill-Qomolangma.png"},{"value":["Kilimanjaro","5895"],"symbol":"image://https://ecomfe.github.io/echarts-examples/public/data/asset/img/hill-Kilimanjaro.png"}],"name":"value","type":"pictorialBar","yAxisIndex":0,"xAxisIndex":0}]},"dispose":true},"evals":[],"jsHooks":[]}
# timeline
df <- data.frame(
  x = rep(1:5, 2),
  y = runif(10, 1, 10),
  year = c(
    rep(2017, 5),
    rep(2018, 5)
  )
)

df |>
  group_by(year) |>
  e_charts(x, timeline = TRUE) |>
  e_pictorial(
    y,
    symbol = "rect",
    symbolRepeat = TRUE,
    z = -1,
    symbolSize = c(10, 4)
  )

{"x":{"theme":"","tl":true,"draw":true,"renderer":"canvas","events":[],"buttons":[],"settings":{"crosstalk_key":null,"crosstalk_group":null},"opts":{"baseOption":{"yAxis":[{"show":true}],"timeline":{"data":["2017","2018"],"axisType":"category"},"xAxis":[{"type":"value"}],"legend":{"data":["y"]},"series":[{"name":"y","type":"pictorialBar","yAxisIndex":0,"xAxisIndex":0,"symbolRepeat":true,"z":-1,"symbolSize":[10,4]}]},"options":[{"series":[{"data":[{"value":[1,6.677289308514446]},{"value":[2,6.741374559467658]},{"value":[3,5.852175108157098]},{"value":[4,3.711235732072964]},{"value":[5,9.09841323364526]}]}]},{"series":[{"data":[{"value":[1,8.150339548010379]},{"value":[2,7.15040225815028]},{"value":[3,4.860965554602444]},{"value":[4,1.867917549796402]},{"value":[5,5.758024752140045]}]}]}]},"dispose":true},"evals":[],"jsHooks":[]}
```
