# Shiny bindings for echarts4r

Output and render functions for using echarts4r within Shiny
applications and interactive Rmd documents.

## Usage

``` r
echarts4rOutput(outputId, width = "100%", height = "400px")

renderEcharts4r(expr, env = parent.frame(), quoted = FALSE)

echarts4rProxy(
  id,
  data,
  x,
  timeline = FALSE,
  session = shiny::getDefaultReactiveDomain(),
  reorder = TRUE
)

echarts4r_proxy(
  id,
  data,
  x,
  timeline = FALSE,
  session = shiny::getDefaultReactiveDomain(),
  reorder = TRUE
)
```

## Arguments

- outputId:

  output variable to read from.

- width, height:

  Must be a valid CSS unit (like `'100%'`, `'400px'`, `'auto'`) or a
  number, which will be coerced to a string and have `'px'` appended.

- expr:

  An expression that generates a echarts4r

- env:

  The environment in which to evaluate `expr`.

- quoted:

  Is `expr` a quoted expression (with
  [`quote()`](https://rdrr.io/r/base/substitute.html))? This is useful
  if you want to save an expression in a variable.

- id:

  Target chart id.

- data:

  A `data.frame` or a crosstalk `SharedData` object

- x:

  Column name containing x axis.

- timeline:

  Set to `TRUE` to build a timeline, see timeline section.

- session:

  Shiny session.

- reorder:

  Set to `FALSE` to not reorder numeric x axis values.

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

## Callbacks

- `id_brush`: returns data on brushed data points.

- `id_legend_change`: returns series name of legend selected/unselected.

- `id_clicked_data`: returns data of clicked data point.

- `id_clicked_data_value`: returns value of clicked data point.

- `id_clicked_row`: returns row number of clicked data point.

- `id_clicked_serie`: returns name of serie of clicked data point.

- `id_mouseover_data`: returns data on hovered data point.

- `id_mouseover_data_value`: returns value of hovered data point.

- `id_mouseover_row`: returns row o hovered data point.

- `id_mouseover_serie`: returns name of serie of hovered data point.

- `id_dragged_annotation`: returns data on dragged annotation.

## Proxies

The `echarts4rProxy` function returns a proxy for chart which allows
manipulating a drawn chart, adding data, adding or removing series, etc.
without redrawing the entire chart.

- [`e_append1_p`](https://echarts4r.john-coene.com/reference/append.md)
  &
  [`e_append2_p`](https://echarts4r.john-coene.com/reference/append.md)

- [`e_showtip_p`](https://echarts4r.john-coene.com/reference/tooltip_proxy.md)
  &
  [`e_hidetip_p`](https://echarts4r.john-coene.com/reference/tooltip_proxy.md)

- [`e_highlight_p`](https://echarts4r.john-coene.com/reference/highlight_proxy.md)
  &
  [`e_downplay_p`](https://echarts4r.john-coene.com/reference/highlight_proxy.md)

- [`e_focus_adjacency`](https://echarts4r.john-coene.com/reference/graph_action.md)
  &
  [`e_unfocus_adjacency`](https://echarts4r.john-coene.com/reference/graph_action.md)

- [`e_dispatch_action_p`](https://echarts4r.john-coene.com/reference/e_dispatch_action_p.md)

- [`e_execute`](https://echarts4r.john-coene.com/reference/e_execute.md)

- [`e_remove_serie_p`](https://echarts4r.john-coene.com/reference/e_remove.md)
