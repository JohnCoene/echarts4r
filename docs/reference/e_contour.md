# Contour chart

Draw a contour plot. x and y must each be numbers.

## Usage

``` r
e_contour(
  e,
  serie,
  name = "contour",
  legend = TRUE,
  thresholds = 8,
  bandwidth = 20,
  lineStyle = list(opacity = 0.3, color = "black", width = 1),
  contourOpacity = 0.8,
  contourColors = list("#5470c6", "#91cc75", "#fac858", "#ee6666"),
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

- name:

  name of the serie.

- legend:

  Whether to add serie to legend.

- thresholds:

  contour density

- bandwidth:

  the size of the contours

- lineStyle:

  list of properties of the line

- contourOpacity:

  opacity of the countours

- contourColors:

  colors used for the contours

- ...:

  Any other option to pass, check See Also section.

## See also

[official
documentation](https://github.com/apache/echarts-custom-series/tree/main/custom-series/contour)

## Examples

``` r
mtcars |>
 e_charts(mpg) |>
 e_contour(serie = mpg)

{"x":{"theme":"","tl":false,"draw":true,"renderer":"canvas","events":[],"buttons":[],"settings":{"crosstalk_key":null,"crosstalk_group":null},"opts":{"yAxis":[{"show":true}],"xAxis":[{"type":"value"}],"legend":{"data":["contour"]},"series":[{"type":"custom","renderItem":"contour","encode":{"x":0,"y":1,"tooltip":2},"data":[{"value":[10.4,10.4]},{"value":[10.4,10.4]},{"value":[13.3,13.3]},{"value":[14.3,14.3]},{"value":[14.7,14.7]},{"value":[15,15]},{"value":[15.2,15.2]},{"value":[15.2,15.2]},{"value":[15.5,15.5]},{"value":[15.8,15.8]},{"value":[16.4,16.4]},{"value":[17.3,17.3]},{"value":[17.8,17.8]},{"value":[18.1,18.1]},{"value":[18.7,18.7]},{"value":[19.2,19.2]},{"value":[19.2,19.2]},{"value":[19.7,19.7]},{"value":[21,21]},{"value":[21,21]},{"value":[21.4,21.4]},{"value":[21.4,21.4]},{"value":[21.5,21.5]},{"value":[22.8,22.8]},{"value":[22.8,22.8]},{"value":[24.4,24.4]},{"value":[26,26]},{"value":[27.3,27.3]},{"value":[30.4,30.4]},{"value":[30.4,30.4]},{"value":[32.4,32.4]},{"value":[33.9,33.9]}],"name":"contour","itemPayload":{"thresholds":8,"bandwidth":20,"lineStyle":{"opacity":0.3,"color":"black","width":1},"itemStyle":{"opacity":0.8,"color":["#5470c6","#91cc75","#fac858","#ee6666"]}},"data.1":[{"value":[10.4,10.4]},{"value":[10.4,10.4]},{"value":[13.3,13.3]},{"value":[14.3,14.3]},{"value":[14.7,14.7]},{"value":[15,15]},{"value":[15.2,15.2]},{"value":[15.2,15.2]},{"value":[15.5,15.5]},{"value":[15.8,15.8]},{"value":[16.4,16.4]},{"value":[17.3,17.3]},{"value":[17.8,17.8]},{"value":[18.1,18.1]},{"value":[18.7,18.7]},{"value":[19.2,19.2]},{"value":[19.2,19.2]},{"value":[19.7,19.7]},{"value":[21,21]},{"value":[21,21]},{"value":[21.4,21.4]},{"value":[21.4,21.4]},{"value":[21.5,21.5]},{"value":[22.8,22.8]},{"value":[22.8,22.8]},{"value":[24.4,24.4]},{"value":[26,26]},{"value":[27.3,27.3]},{"value":[30.4,30.4]},{"value":[30.4,30.4]},{"value":[32.4,32.4]},{"value":[33.9,33.9]}]}]},"dispose":true},"evals":[],"jsHooks":[]}
```
