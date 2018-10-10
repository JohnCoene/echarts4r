# echarts4r 0.1.2.9000

* `e_labels` to easily add labels.
* Improved `e_scatter`.
* Improved `e_toolbox`.
* Fixed `e_axis` (and corresponding `e_x_axis` and `e_y_axis`) bugs.
* `e_add` added for nested data.
* `e_showtip_p` and `e_hidetip_p` fixed.
* `e_focus_adjacency_p` and `e_unfocus_adjacency_p` proxies to highlight adjacent nodes (on graph/network) added.
* Deprecation of texture related functions in favour of [echarts4r.assets](http://echarts4r-assets.john-coene.com/).
* `e_single_axis` added, works with `e_scatter`.
* `e_text_style` added, to customise global fonts.

First iteration of the graphic low-level API:

- `g_graphic_g`
- `g_group_g`
- `g_image_g`
- `g_text_g`
- `g_rect_g`
- `g_circle_g`
- `g_ring_g`
- `g_sector_g`
- `g_arc_g`
- `g_polygon_g`
- `g_polyline_g`
- `g_line_g`
- `g_bezier_curve_g`

# echarts4r 0.1.1

*Now available on CRAN*

* No longer passes entirety of data.frame to the fron-end.

# echarts4r 0.1.0

`echarts4r` now understands `dplyr::group_by`

* `e_clean` to remove original data from visualisation and lighten up end product.

# echarts4r 0.0.2.9000

* `e_keras_history` added - plot `keras` history (loss & acc).
* `e_format_axis`, `e_format_x_axis`, `e_format_y_axis` added - format axis labels & pointers.
* `e_tooltip` now takes a default `trigger = "item"`.
* `e_flip_coords` added - to flip x/y corrdinates.
* Grid & Axis vignettes.
* `e_register_map` fixed.
* `renderer` argument added to `e_charts` to enable SVG renderer.

# echarts4r 0.0.1

Added:

* `e_lm`, `e_glm` and `e_loess`.

# echarts4r 0.0.1.9000

Initial version with chart types:

* Bar
* Line
* Area
* Step
* Scatter
* Effect Scatter
* Candlestick
* Funnel
* Sankey
* Graph
* Heatmap
* Parallel
* Pie
* Tree
* Treemap
* River (streamgraph)
* Sunburst
* Boxplot
* Calendar
* Gauge
* Polar 
* wordcloud 
* liquidFill 
* Map
* Pictorial
* Line 3D
* Bar 3D
* Scatter 3D
* Map 3D
* Geo 3D
* Globe 3D
* Flow GL
* Graph GL
* Scatter GL

