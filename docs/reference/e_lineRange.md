# Line range chart

Draw a line range area plot.

## Usage

``` r
e_lineRange(
  e,
  lower,
  upper,
  name = "lineRange",
  legend = TRUE,
  y_index = 0,
  x_index = 0,
  lineStyle = list(opacity = 0.3, color = "#000", width = 1),
  areaStyle = list(opacity = 0.3, color = "#032", width = 1),
  ...
)
```

## Arguments

- e:

  An `echarts4r` object as returned by
  [`e_charts`](https://echarts4r.john-coene.com/reference/init.md) or a
  proxy as returned by
  [`echarts4rProxy`](https://echarts4r.john-coene.com/reference/echarts4r-shiny.md).

- lower, upper:

  series of lower and upper borders of the band

- name:

  name of the serie.

- legend:

  Whether to add serie to legend.

- x_index, y_index:

  Indexes of x and y axis.

- lineStyle:

  properties of the border lines

- areaStyle:

  properties of the area between the lines

- ...:

  additional options

## See also

[official
documentation](https://github.com/apache/echarts-custom-series/tree/main/custom-series/lineRange)

## Examples

``` r
df <- iris |>
  dplyr::group_by(Species) |>
  dplyr::summarise(lower = min(Sepal.Length),
                   upper = max(Sepal.Length))
df |>
  e_chart(Species) |>
  e_lineRange(lower = lower, upper = upper)

{"x":{"theme":"","tl":false,"draw":true,"renderer":"canvas","events":[],"buttons":[],"settings":{"crosstalk_key":null,"crosstalk_group":null},"opts":{"yAxis":[{"show":true}],"xAxis":[{"data":["setosa","versicolor","virginica"],"type":"category","boundaryGap":true}],"legend":{"data":[{"name":"lineRange","icon":"rect","itemStyle":{"color":"#032"}}]},"series":[{"type":"custom","name":"lineRange","renderItem":"lineRange","encode":{"x":0,"y":[1,2],"tooltip":[1,2]},"data":[{"value":["setosa","4.3","5.8"]},{"value":["versicolor","4.9","7.0"]},{"value":["virginica","4.9","7.9"]}],"itemPayload":{"areaStyle":{"opacity":0.3,"color":"#032","width":1},"lineStyle":{"opacity":0.3,"color":"#000","width":1}},"data.1":[{"value":["setosa","4.3","5.8"]},{"value":["versicolor","4.9","7.0"]},{"value":["virginica","4.9","7.9"]}]}]},"dispose":true},"evals":[],"jsHooks":[]}
```
