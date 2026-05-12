# Initialise

Initialise a chart.

## Usage

``` r
e_charts(
  data,
  x,
  width = NULL,
  height = NULL,
  elementId = NULL,
  dispose = TRUE,
  draw = TRUE,
  renderer = "canvas",
  timeline = FALSE,
  ...,
  reorder = TRUE
)

# Default S3 method
e_charts(
  data,
  x,
  width = NULL,
  height = NULL,
  elementId = NULL,
  dispose = TRUE,
  draw = TRUE,
  renderer = "canvas",
  timeline = FALSE,
  ...,
  reorder = TRUE
)

# S3 method for class 'Node'
e_charts(
  data,
  x,
  width = NULL,
  height = NULL,
  elementId = NULL,
  dispose = TRUE,
  draw = TRUE,
  renderer = "canvas",
  timeline = FALSE,
  ...,
  reorder = TRUE
)

e_charts_(
  data,
  x = NULL,
  width = NULL,
  height = NULL,
  elementId = NULL,
  dispose = TRUE,
  draw = TRUE,
  renderer = "canvas",
  timeline = FALSE,
  ...,
  reorder = TRUE
)

e_chart(
  data,
  x,
  width = NULL,
  height = NULL,
  elementId = NULL,
  dispose = TRUE,
  draw = TRUE,
  renderer = "canvas",
  timeline = FALSE,
  ...,
  reorder = TRUE
)

e_data(e, data, x)
```

## Arguments

- data:

  A `data.frame` or a crosstalk `SharedData` object

- x:

  Column name containing x axis.

- width, height:

  Must be a valid CSS unit (like `'100%'`, `'400px'`, `'auto'`) or a
  number, which will be coerced to a string and have `'px'` appended.

- elementId:

  Id of element.

- dispose:

  Set to `TRUE` to force redraw of chart, set to `FALSE` to update.

- draw:

  Whether to draw the chart, intended to be used with
  [`e_draw_p`](https://echarts4r.john-coene.com/reference/e_draw_p.md).

- renderer:

  Renderer, takes `canvas` (default) or `svg`.

- timeline:

  Set to `TRUE` to build a timeline, see timeline section.

- ...:

  Any other argument.

- reorder:

  Set to `FALSE` to not reorder numeric x axis values.

- e:

  An object of class `echarts4r` as returned by `e_charts`.

## Details

The chart is created inside a parent `'<div>'` element, the dimensions
of which are controlled by the `'width'` and `'height'` arguments. When
these dimensions are small, it is possible that the chart `'grid'`
resizes to a size larger than the parent, which might result in
unexpected size given the input arguments. To disable this automatic
readjustment, define a static
[`e_grid`](https://echarts4r.john-coene.com/reference/e_grid.md) like
the following:
`'e_grid(e = current_chart, top = 0, left = 20, right = 0, bottom = 20)'`.

## Timeline

The timeline feature currently supports the following chart types.

- [`e_bar`](https://echarts4r.john-coene.com/reference/e_bar.md)

- [`e_line`](https://echarts4r.john-coene.com/reference/e_line.md)

- [`e_step`](https://echarts4r.john-coene.com/reference/e_step.md)

- [`e_area`](https://echarts4r.john-coene.com/reference/e_area.md)

- [`e_scatter`](https://echarts4r.john-coene.com/reference/scatter.md)

- [`e_effect_scatter`](https://echarts4r.john-coene.com/reference/scatter.md)

- [`e_candle`](https://echarts4r.john-coene.com/reference/e_candle.md)

- [`e_heatmap`](https://echarts4r.john-coene.com/reference/e_heatmap.md)

- [`e_pie`](https://echarts4r.john-coene.com/reference/e_pie.md)

- [`e_line_3d`](https://echarts4r.john-coene.com/reference/line3D.md)

- [`e_lines_3d`](https://echarts4r.john-coene.com/reference/line3D.md)

- [`e_bar_3d`](https://echarts4r.john-coene.com/reference/e_bar_3d.md)

- [`e_lines`](https://echarts4r.john-coene.com/reference/e_lines.md)

- [`e_scatter_3d`](https://echarts4r.john-coene.com/reference/e_scatter_3d.md)

- [`e_scatter_gl`](https://echarts4r.john-coene.com/reference/e_scatter_gl.md)

- [`e_histogram`](https://echarts4r.john-coene.com/reference/histogram.md)

- [`e_lm`](https://echarts4r.john-coene.com/reference/smooth.md)

- [`e_loess`](https://echarts4r.john-coene.com/reference/smooth.md)

- [`e_glm`](https://echarts4r.john-coene.com/reference/smooth.md)

- [`e_density`](https://echarts4r.john-coene.com/reference/histogram.md)

- [`e_pictorial`](https://echarts4r.john-coene.com/reference/e_pictorial.md)

- [`e_boxplot`](https://echarts4r.john-coene.com/reference/e_boxplot.md)

- [`e_map`](https://echarts4r.john-coene.com/reference/map.md)

- [`e_map_3d`](https://echarts4r.john-coene.com/reference/map.md)

- [`e_line_3d`](https://echarts4r.john-coene.com/reference/line3D.md)

- [`e_gauge`](https://echarts4r.john-coene.com/reference/e_gauge.md)

## Crosstalk

TO use crosstalk, pass `selectedMode = TRUE`) into the chart type
argument. crosstalk functionality currently supports the following chart
types (but only cartesion2d):

- [`e_area`](https://echarts4r.john-coene.com/reference/e_area.md)

- [`e_bar`](https://echarts4r.john-coene.com/reference/e_bar.md)

- [`e_candle`](https://echarts4r.john-coene.com/reference/e_candle.md)

- [`e_density`](https://echarts4r.john-coene.com/reference/histogram.md)

- [`e_effect_scatter`](https://echarts4r.john-coene.com/reference/scatter.md)

- [`e_funnel`](https://echarts4r.john-coene.com/reference/e_funnel.md)

- [`e_heatmap`](https://echarts4r.john-coene.com/reference/e_heatmap.md)

- [`e_histogram`](https://echarts4r.john-coene.com/reference/histogram.md)

- [`e_line`](https://echarts4r.john-coene.com/reference/e_line.md)

- [`e_pictorial`](https://echarts4r.john-coene.com/reference/e_pictorial.md)

- [`e_pie`](https://echarts4r.john-coene.com/reference/e_pie.md)

- [`e_scatter`](https://echarts4r.john-coene.com/reference/scatter.md)

- [`e_step`](https://echarts4r.john-coene.com/reference/e_step.md)

## Examples

``` r
mtcars |>
  e_charts(qsec) |>
  e_line(mpg)

{"x":{"theme":"","tl":false,"draw":true,"renderer":"canvas","events":[],"buttons":[],"settings":{"crosstalk_key":null,"crosstalk_group":null},"opts":{"yAxis":[{"show":true}],"xAxis":[{"type":"value"}],"legend":{"data":["mpg"]},"series":[{"data":[{"value":[14.5,15.8]},{"value":[14.6,15]},{"value":[15.41,13.3]},{"value":[15.5,19.7]},{"value":[15.84,14.3]},{"value":[16.46,21]},{"value":[16.7,26]},{"value":[16.87,15.5]},{"value":[16.9,30.4]},{"value":[17.02,21]},{"value":[17.02,18.7]},{"value":[17.05,19.2]},{"value":[17.3,15.2]},{"value":[17.4,16.4]},{"value":[17.42,14.7]},{"value":[17.6,17.3]},{"value":[17.82,10.4]},{"value":[17.98,10.4]},{"value":[18,15.2]},{"value":[18.3,19.2]},{"value":[18.52,30.4]},{"value":[18.6,21.4]},{"value":[18.61,22.8]},{"value":[18.9,17.8]},{"value":[18.9,27.3]},{"value":[19.44,21.4]},{"value":[19.47,32.4]},{"value":[19.9,33.9]},{"value":[20,24.4]},{"value":[20.01,21.5]},{"value":[20.22,18.1]},{"value":[22.9,22.8]}],"yAxisIndex":0,"xAxisIndex":0,"name":"mpg","type":"line","coordinateSystem":"cartesian2d"}]},"dispose":true},"evals":[],"jsHooks":[]}
sd <- crosstalk::SharedData$new(mtcars)
mtcars |>
  e_charts(qsec) |>
  e_line(mpg, selectedMode = TRUE)

{"x":{"theme":"","tl":false,"draw":true,"renderer":"canvas","events":[],"buttons":[],"settings":{"crosstalk_key":null,"crosstalk_group":null},"opts":{"yAxis":[{"show":true}],"xAxis":[{"type":"value"}],"legend":{"data":["mpg"]},"series":[{"data":[{"value":[14.5,15.8]},{"value":[14.6,15]},{"value":[15.41,13.3]},{"value":[15.5,19.7]},{"value":[15.84,14.3]},{"value":[16.46,21]},{"value":[16.7,26]},{"value":[16.87,15.5]},{"value":[16.9,30.4]},{"value":[17.02,21]},{"value":[17.02,18.7]},{"value":[17.05,19.2]},{"value":[17.3,15.2]},{"value":[17.4,16.4]},{"value":[17.42,14.7]},{"value":[17.6,17.3]},{"value":[17.82,10.4]},{"value":[17.98,10.4]},{"value":[18,15.2]},{"value":[18.3,19.2]},{"value":[18.52,30.4]},{"value":[18.6,21.4]},{"value":[18.61,22.8]},{"value":[18.9,17.8]},{"value":[18.9,27.3]},{"value":[19.44,21.4]},{"value":[19.47,32.4]},{"value":[19.9,33.9]},{"value":[20,24.4]},{"value":[20.01,21.5]},{"value":[20.22,18.1]},{"value":[22.9,22.8]}],"yAxisIndex":0,"xAxisIndex":0,"name":"mpg","type":"line","coordinateSystem":"cartesian2d","selectedMode":true}]},"dispose":true},"evals":[],"jsHooks":[]}
points <- mtcars[1:3, ]
mtcars |>
  e_charts_("qsec") |>
  e_line(mpg) |>
  e_data(points, qsec) |>
  e_scatter(mpg, color = "red", symbol_size = 20)

{"x":{"theme":"","tl":false,"draw":true,"renderer":"canvas","events":[],"buttons":[],"opts":{"yAxis":[{"show":true}],"xAxis":[{"type":"value"}],"legend":{"data":["mpg","mpg"]},"series":[{"data":[{"value":[14.5,15.8]},{"value":[14.6,15]},{"value":[15.41,13.3]},{"value":[15.5,19.7]},{"value":[15.84,14.3]},{"value":[16.46,21]},{"value":[16.7,26]},{"value":[16.87,15.5]},{"value":[16.9,30.4]},{"value":[17.02,21]},{"value":[17.02,18.7]},{"value":[17.05,19.2]},{"value":[17.3,15.2]},{"value":[17.4,16.4]},{"value":[17.42,14.7]},{"value":[17.6,17.3]},{"value":[17.82,10.4]},{"value":[17.98,10.4]},{"value":[18,15.2]},{"value":[18.3,19.2]},{"value":[18.52,30.4]},{"value":[18.6,21.4]},{"value":[18.61,22.8]},{"value":[18.9,17.8]},{"value":[18.9,27.3]},{"value":[19.44,21.4]},{"value":[19.47,32.4]},{"value":[19.9,33.9]},{"value":[20,24.4]},{"value":[20.01,21.5]},{"value":[20.22,18.1]},{"value":[22.9,22.8]}],"yAxisIndex":0,"xAxisIndex":0,"name":"mpg","type":"line","coordinateSystem":"cartesian2d"},{"data":[{"value":[16.46,21]},{"value":[17.02,21]},{"value":[18.61,22.8]}],"name":"mpg","type":"scatter","symbol":null,"color":"red","coordinateSystem":"cartesian2d","color.1":"red","yAxisIndex":0,"xAxisIndex":0,"symbolSize":20}]},"dispose":true},"evals":[],"jsHooks":[]}
```
