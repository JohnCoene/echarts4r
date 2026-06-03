# Violin chart

Draw a violin chart with scattered dots.

## Usage

``` r
e_violin(
  e,
  name = "violin",
  legend = TRUE,
  y_index = 0,
  x_index = 0,
  symbolSize = 10,
  areaOpacity = 0.5,
  binCount = 100,
  bandWidthScale = 1,
  ...
)
```

## Arguments

- e:

  An `echarts4r` object as returned by
  [`e_charts`](https://echarts4r.john-coene.com/reference/init.md) or a
  proxy as returned by
  [`echarts4rProxy`](https://echarts4r.john-coene.com/reference/echarts4r-shiny.md).

- name:

  name of the serie.

- legend:

  Whether to add serie to legend.

- x_index, y_index:

  Indexes of x and y axis.

- symbolSize:

  overall size of symbol.

- areaOpacity:

  opacity of violin area.

- binCount:

  number of bins for violin plot. More bins will provide a more detailed
  version of the plot,

- bandWidthScale:

  scale for the amplitude of violin area

- ...:

  additional options

## See also

[official
documentation](https://github.com/apache/echarts-custom-series/tree/main/custom-series/violin)

## Examples

``` r
PlantGrowth |>
  e_charts(group) |>
  e_scatter(weight) |>
  e_violin(binCount = 200)

{"x":{"theme":"","tl":false,"draw":true,"renderer":"canvas","events":[],"buttons":[],"settings":{"crosstalk_key":null,"crosstalk_group":null},"opts":{"yAxis":[{"show":true}],"xAxis":[{"data":["ctrl","trt1","trt2"],"type":"category","boundaryGap":true}],"legend":{"data":["weight","violin"]},"series":[{"data":[{"value":["ctrl","4.17"]},{"value":["ctrl","5.58"]},{"value":["ctrl","5.18"]},{"value":["ctrl","6.11"]},{"value":["ctrl","4.50"]},{"value":["ctrl","4.61"]},{"value":["ctrl","5.17"]},{"value":["ctrl","4.53"]},{"value":["ctrl","5.33"]},{"value":["ctrl","5.14"]},{"value":["trt1","4.81"]},{"value":["trt1","4.17"]},{"value":["trt1","4.41"]},{"value":["trt1","3.59"]},{"value":["trt1","5.87"]},{"value":["trt1","3.83"]},{"value":["trt1","6.03"]},{"value":["trt1","4.89"]},{"value":["trt1","4.32"]},{"value":["trt1","4.69"]},{"value":["trt2","6.31"]},{"value":["trt2","5.12"]},{"value":["trt2","5.54"]},{"value":["trt2","5.50"]},{"value":["trt2","5.37"]},{"value":["trt2","5.29"]},{"value":["trt2","4.92"]},{"value":["trt2","6.15"]},{"value":["trt2","5.80"]},{"value":["trt2","5.26"]}],"name":"weight","type":"scatter","symbol":null,"coordinateSystem":"cartesian2d","yAxisIndex":0,"xAxisIndex":0,"symbolSize":3},{"type":"custom","renderItem":"violin","silent":true,"itemPayload":{"symbolSize":10,"areaOpacity":0.5,"binCount":200,"bandWidthScale":1},"name":"violin"}],"dataset":{"source":[["ctrl","4.17"],["ctrl","5.58"],["ctrl","5.18"],["ctrl","6.11"],["ctrl","4.50"],["ctrl","4.61"],["ctrl","5.17"],["ctrl","4.53"],["ctrl","5.33"],["ctrl","5.14"],["trt1","4.81"],["trt1","4.17"],["trt1","4.41"],["trt1","3.59"],["trt1","5.87"],["trt1","3.83"],["trt1","6.03"],["trt1","4.89"],["trt1","4.32"],["trt1","4.69"],["trt2","6.31"],["trt2","5.12"],["trt2","5.54"],["trt2","5.50"],["trt2","5.37"],["trt2","5.29"],["trt2","4.92"],["trt2","6.15"],["trt2","5.80"],["trt2","5.26"]]}},"dispose":true},"evals":[],"jsHooks":[]}
```
