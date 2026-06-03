# Draft

Add a draft watermark to your graph.

## Usage

``` r
e_draft(e, text = "DRAFT", size = "120px", opacity = 0.4, color = "#d3d3d3")
```

## Arguments

- e:

  An `echarts4r` object as returned by
  [`e_charts`](https://echarts4r.john-coene.com/reference/init.md) or a
  proxy as returned by
  [`echarts4rProxy`](https://echarts4r.john-coene.com/reference/echarts4r-shiny.md).

- text:

  Text to display.

- size:

  Font size of text.

- opacity, color:

  Opacity and color of text.

## Examples

``` r
cars |>
  e_charts(speed) |>
  e_scatter(dist) |>
  e_draft()

{"x":{"theme":"","tl":false,"draw":true,"renderer":"canvas","events":[],"buttons":[],"settings":{"crosstalk_key":null,"crosstalk_group":null},"opts":{"yAxis":[{"show":true}],"xAxis":[{"type":"value"}],"legend":{"data":["dist"]},"series":[{"data":[{"value":[4,2]},{"value":[4,10]},{"value":[7,4]},{"value":[7,22]},{"value":[8,16]},{"value":[9,10]},{"value":[10,18]},{"value":[10,26]},{"value":[10,34]},{"value":[11,17]},{"value":[11,28]},{"value":[12,14]},{"value":[12,20]},{"value":[12,24]},{"value":[12,28]},{"value":[13,26]},{"value":[13,34]},{"value":[13,34]},{"value":[13,46]},{"value":[14,26]},{"value":[14,36]},{"value":[14,60]},{"value":[14,80]},{"value":[15,20]},{"value":[15,26]},{"value":[15,54]},{"value":[16,32]},{"value":[16,40]},{"value":[17,32]},{"value":[17,40]},{"value":[17,50]},{"value":[18,42]},{"value":[18,56]},{"value":[18,76]},{"value":[18,84]},{"value":[19,36]},{"value":[19,46]},{"value":[19,68]},{"value":[20,32]},{"value":[20,48]},{"value":[20,52]},{"value":[20,56]},{"value":[20,64]},{"value":[22,66]},{"value":[23,54]},{"value":[24,70]},{"value":[24,92]},{"value":[24,93]},{"value":[24,120]},{"value":[25,85]}],"name":"dist","type":"scatter","symbol":null,"coordinateSystem":"cartesian2d","yAxisIndex":0,"xAxisIndex":0,"symbolSize":3}],"graphic":{"left":"center","top":"center","z":-1000,"style":{"fill":"#d3d3d3","text":"DRAFT","font":"120px \"STHeiti\", sans-serif","opacity":0.4},"type":"text","left.1":"center","top.1":"center","z.1":-1000,"style.1":{"fill":"#d3d3d3","text":"DRAFT","font":"120px \"STHeiti\", sans-serif","opacity":0.4}}},"dispose":true},"evals":[],"jsHooks":[]}
```
