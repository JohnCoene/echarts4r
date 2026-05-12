# Add nested data

Utility function to add data where the original JavaScript library
expects nested data.

## Usage

``` r
e_add(e, param, ..., .serie = NULL, .data = NULL)

e_add_nested(e, param, ..., .serie = NULL, .data = NULL)

e_add_unnested(e, param, value, .serie = NULL, .data = NULL)
```

## Arguments

- e:

  An `echarts4r` object as returned by
  [`e_charts`](https://echarts4r.john-coene.com/reference/init.md) or a
  proxy as returned by
  [`echarts4rProxy`](https://echarts4r.john-coene.com/reference/echarts4r-shiny.md).

- param:

  The nested parameter to add data to.

- ...:

  Any other option to pass, check See Also section.

- .serie:

  Serie's index to add the data to, if \`NULL\` then it is added to all.

- .data:

  A dataset to use, if none are specified than the original dataset
  passed to \`e_charts\` is used.

- value:

  The column to map to the parameter.

## Details

For instance,
[`e_funnel`](https://echarts4r.john-coene.com/reference/e_funnel.md)
lets you pass `values` and `labels` (from your initial data.frame) which
corresponds to `name` and `value` in the [original
library](https://echarts.apache.org/en/option.html#series-heatmap.data).
However the latter also takes, `label`, `itemStyle`, and `emphasis` but
being JSON arrays they translate to lists in R and dealing with nested
data.frames is not ideal. `e_add` remedies to that. It allows adding
those nested data points, see the examples below.

## Functions

\- \`e_add_nested\`: Adds nested data, e.g.: \`e_add_nested("itemStyle",
color, fontBold)\` creates \`{itemStyle: {color: 'red', fontBold:
'bold'}}\`. - \`e_add_unnested\`: Adds unnested data, e.g.:
\`e_add_unnested("symbolSize", size)\` creates \`{symbolSize: 4}\`.

## Examples

``` r
# funnel can take nested itemStyle
# https://echarts.apache.org/en/option.html#series-funnel.data
funnel <- data.frame(
  stage = c("View", "Click", "Purchase"),
  value = c(80, 30, 20),
  color = c("blue", "red", "green")
)

funnel |>
  e_charts() |>
  e_funnel(value, stage) |>
  e_add_nested("itemStyle", color)

{"x":{"theme":"","tl":false,"draw":true,"renderer":"canvas","events":[],"buttons":[],"settings":{"crosstalk_key":null,"crosstalk_group":null},"opts":{"legend":{"data":["View","Click","Purchase"]},"series":[{"data":[{"value":80,"name":"View","itemStyle":{"color":"blue"}},{"value":30,"name":"Click","itemStyle":{"color":"red"}},{"value":20,"name":"Purchase","itemStyle":{"color":"green"}}],"name":null,"type":"funnel"}]},"dispose":true},"evals":[],"jsHooks":[]}
# Heatmap can take nested label
# https://echarts.apache.org/en/option.html#series-heatmap.data
v <- LETTERS[1:10]
matrix <- data.frame(
  x = sample(v, 300, replace = TRUE),
  y = sample(v, 300, replace = TRUE),
  z = rnorm(300, 10, 1),
  stringsAsFactors = FALSE
) |>
  dplyr::group_by(x, y) |>
  dplyr::summarise(z = sum(z)) |>
  dplyr::ungroup() |>
  dplyr::mutate(
    show = TRUE,
    fontStyle = round(runif(dplyr::n(), 5, 12))
  )
#> `summarise()` has regrouped the output.
#> ℹ Summaries were computed grouped by x and y.
#> ℹ Output is grouped by x.
#> ℹ Use `summarise(.groups = "drop_last")` to silence this message.
#> ℹ Use `summarise(.by = c(x, y))` for per-operation grouping
#>   (`?dplyr::dplyr_by`) instead.

matrix |>
  e_charts(x) |>
  e_heatmap(y, z) |>
  e_visual_map(z) |>
  e_add_nested(
    "label",
    show,
    fontStyle
  )

{"x":{"theme":"","tl":false,"draw":true,"renderer":"canvas","events":[],"buttons":[],"settings":{"crosstalk_key":null,"crosstalk_group":null},"opts":{"yAxis":[{"data":["A","B","C","D","E","F","G","I","J","H"]}],"xAxis":[{"data":["A","B","C","D","E","F","G","H","I","J"]}],"series":[{"data":[{"value":["A","A","40.353702"],"label":{"show":1,"fontStyle":9}},{"value":["A","B","29.007876"],"label":{"show":1,"fontStyle":10}},{"value":["A","C","82.027596"],"label":{"show":1,"fontStyle":7}},{"value":["A","D","36.674855"],"label":{"show":1,"fontStyle":9}},{"value":["A","E","28.326759"],"label":{"show":1,"fontStyle":8}},{"value":["A","F","21.363583"],"label":{"show":1,"fontStyle":10}},{"value":["A","G","10.405019"],"label":{"show":1,"fontStyle":8}},{"value":["A","I","37.937255"],"label":{"show":1,"fontStyle":8}},{"value":["A","J","19.266084"],"label":{"show":1,"fontStyle":6}},{"value":["B","A","29.166993"],"label":{"show":1,"fontStyle":10}},{"value":["B","B"," 9.343363"],"label":{"show":1,"fontStyle":6}},{"value":["B","C","40.711156"],"label":{"show":1,"fontStyle":5}},{"value":["B","D","18.902946"],"label":{"show":1,"fontStyle":6}},{"value":["B","E","74.636373"],"label":{"show":1,"fontStyle":7}},{"value":["B","G","40.574989"],"label":{"show":1,"fontStyle":6}},{"value":["B","H","48.064287"],"label":{"show":1,"fontStyle":10}},{"value":["B","I","38.774653"],"label":{"show":1,"fontStyle":10}},{"value":["B","J","30.167774"],"label":{"show":1,"fontStyle":5}},{"value":["C","A","10.888667"],"label":{"show":1,"fontStyle":11}},{"value":["C","C","10.575803"],"label":{"show":1,"fontStyle":7}},{"value":["C","D","17.039763"],"label":{"show":1,"fontStyle":8}},{"value":["C","E","26.568690"],"label":{"show":1,"fontStyle":10}},{"value":["C","F","61.427466"],"label":{"show":1,"fontStyle":8}},{"value":["C","G","38.734531"],"label":{"show":1,"fontStyle":11}},{"value":["C","I","31.710766"],"label":{"show":1,"fontStyle":10}},{"value":["C","J","20.207226"],"label":{"show":1,"fontStyle":6}},{"value":["D","A","38.571562"],"label":{"show":1,"fontStyle":8}},{"value":["D","B","24.721724"],"label":{"show":1,"fontStyle":11}},{"value":["D","C","20.451710"],"label":{"show":1,"fontStyle":8}},{"value":["D","D","39.804609"],"label":{"show":1,"fontStyle":5}},{"value":["D","E","41.412879"],"label":{"show":1,"fontStyle":8}},{"value":["D","F","21.155859"],"label":{"show":1,"fontStyle":6}},{"value":["D","G","11.008447"],"label":{"show":1,"fontStyle":9}},{"value":["D","I","24.676565"],"label":{"show":1,"fontStyle":7}},{"value":["D","J","30.636643"],"label":{"show":1,"fontStyle":9}},{"value":["E","A"," 9.490738"],"label":{"show":1,"fontStyle":8}},{"value":["E","B","49.494165"],"label":{"show":1,"fontStyle":9}},{"value":["E","C","37.900894"],"label":{"show":1,"fontStyle":12}},{"value":["E","D","11.022324"],"label":{"show":1,"fontStyle":9}},{"value":["E","E","18.699125"],"label":{"show":1,"fontStyle":11}},{"value":["E","F","40.206044"],"label":{"show":1,"fontStyle":6}},{"value":["E","G","28.371479"],"label":{"show":1,"fontStyle":5}},{"value":["E","H","10.879087"],"label":{"show":1,"fontStyle":7}},{"value":["E","I"," 9.486033"],"label":{"show":1,"fontStyle":6}},{"value":["E","J","20.977117"],"label":{"show":1,"fontStyle":7}},{"value":["F","A","62.250170"],"label":{"show":1,"fontStyle":7}},{"value":["F","B","62.035364"],"label":{"show":1,"fontStyle":12}},{"value":["F","C","50.726236"],"label":{"show":1,"fontStyle":11}},{"value":["F","D","49.582173"],"label":{"show":1,"fontStyle":10}},{"value":["F","E","37.414651"],"label":{"show":1,"fontStyle":10}},{"value":["F","F"," 9.677549"],"label":{"show":1,"fontStyle":8}},{"value":["F","G","30.324623"],"label":{"show":1,"fontStyle":8}},{"value":["F","H","26.347197"],"label":{"show":1,"fontStyle":11}},{"value":["F","I","30.427584"],"label":{"show":1,"fontStyle":12}},{"value":["F","J","20.089231"],"label":{"show":1,"fontStyle":8}},{"value":["G","A","19.758529"],"label":{"show":1,"fontStyle":5}},{"value":["G","B","39.329117"],"label":{"show":1,"fontStyle":7}},{"value":["G","C","22.304832"],"label":{"show":1,"fontStyle":7}},{"value":["G","D","30.089366"],"label":{"show":1,"fontStyle":6}},{"value":["G","E","19.946614"],"label":{"show":1,"fontStyle":8}},{"value":["G","F","10.076510"],"label":{"show":1,"fontStyle":10}},{"value":["G","G","59.603860"],"label":{"show":1,"fontStyle":12}},{"value":["G","H","19.473796"],"label":{"show":1,"fontStyle":9}},{"value":["G","I","20.084887"],"label":{"show":1,"fontStyle":10}},{"value":["G","J","49.482638"],"label":{"show":1,"fontStyle":10}},{"value":["H","A","52.553059"],"label":{"show":1,"fontStyle":8}},{"value":["H","B","41.087716"],"label":{"show":1,"fontStyle":6}},{"value":["H","C","39.632505"],"label":{"show":1,"fontStyle":7}},{"value":["H","D","51.881353"],"label":{"show":1,"fontStyle":7}},{"value":["H","E","48.895799"],"label":{"show":1,"fontStyle":8}},{"value":["H","F","17.095981"],"label":{"show":1,"fontStyle":8}},{"value":["H","G","51.949917"],"label":{"show":1,"fontStyle":9}},{"value":["H","H","10.179072"],"label":{"show":1,"fontStyle":9}},{"value":["H","I","60.030699"],"label":{"show":1,"fontStyle":11}},{"value":["H","J","53.396432"],"label":{"show":1,"fontStyle":7}},{"value":["I","B","22.357883"],"label":{"show":1,"fontStyle":6}},{"value":["I","C","31.215372"],"label":{"show":1,"fontStyle":9}},{"value":["I","D","41.644035"],"label":{"show":1,"fontStyle":7}},{"value":["I","E","31.739382"],"label":{"show":1,"fontStyle":6}},{"value":["I","F","30.335437"],"label":{"show":1,"fontStyle":6}},{"value":["I","G"," 9.915174"],"label":{"show":1,"fontStyle":6}},{"value":["I","H","30.501559"],"label":{"show":1,"fontStyle":9}},{"value":["I","I","41.089435"],"label":{"show":1,"fontStyle":9}},{"value":["I","J","20.496069"],"label":{"show":1,"fontStyle":11}},{"value":["J","A","20.259557"],"label":{"show":1,"fontStyle":11}},{"value":["J","B","32.044330"],"label":{"show":1,"fontStyle":12}},{"value":["J","C","62.915844"],"label":{"show":1,"fontStyle":7}},{"value":["J","D","11.323232"],"label":{"show":1,"fontStyle":7}},{"value":["J","E","69.263264"],"label":{"show":1,"fontStyle":12}},{"value":["J","F","20.479011"],"label":{"show":1,"fontStyle":8}},{"value":["J","G","53.718480"],"label":{"show":1,"fontStyle":8}},{"value":["J","H","11.766164"],"label":{"show":1,"fontStyle":8}},{"value":["J","I","20.478044"],"label":{"show":1,"fontStyle":12}},{"value":["J","J"," 9.944084"],"label":{"show":1,"fontStyle":9}}],"name":null,"type":"heatmap","coordinateSystem":"cartesian2d"}],"visualMap":[{"calculable":true,"type":"continuous","min":9.343362635558078,"max":82.02759572021867}]},"dispose":true},"evals":[],"jsHooks":[]}
```
