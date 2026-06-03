# Segmented Doughnut

Draw segmented doughnut.

## Usage

``` r
e_doughnut(
  e,
  numerator = NULL,
  denominator = NULL,
  formatter = "{c}/{b}",
  fontSize = "10em",
  fontColor = "#555",
  center = c("50%", "50%"),
  radius = c("50%", "65%"),
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

- numerator, denominator:

  numeraetor to provide filled segments and denominator for total
  segments.

- formatter:

  javascript string formatter for center text of chart.

- fontSize, fontColor:

  font values for center text of chart.

- center, radius:

  center provides relative position of the center of chart while radius
  provides the radius of your circle for outer segments.

- rm_x, rm_y:

  Whether to remove x and y axis, defaults to `TRUE`.

- ...:

  Any other option to pass, check See Also section.

## See also

[official
documentation](https://github.com/apache/echarts-custom-series/tree/main/custom-series/segmentedDoughnut)

## Examples

``` r

e_chart() |>
 e_doughnut(numerator = 3, denominator = 6)

{"x":{"theme":"","tl":false,"draw":true,"renderer":"canvas","events":[],"buttons":[],"settings":{"crosstalk_key":null,"crosstalk_group":null},"opts":{"series":[{"type":"custom","renderItem":"segmentedDoughnut","coordinateSystem":"none","itemPayload":{"center":["50%","50%"],"radius":["50%","65%"],"segmentCount":6,"label":{"show":true,"formatter":"{c}/{b}","fontSize":"10em","color":"#555"}},"data":[3]}]},"dispose":true},"evals":[],"jsHooks":[]}
```
