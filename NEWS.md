# echarts4r 0.2.0

Arguments containing `.` have been replaced by `_` for clarity. I.e.: `coord.system` => `coord_system`.

Deprecated functions from `0.1.2` have been removed, they can be found in the [echarts4r.assets](http://echarts4r-assets.john-coene.com/) package:

- `e_convert_texture`
- `e_stars_texture`
- `e_composite_texture`
- `e_globe_texture`
- `e_map_texture`

The `scatter` family of function has been reworked: the `scale` argument now takes a scaling function. To accomodate to this, the `e_visual_map` function also takes, optionally, a `scaling` function.
Retired `e_keras_history` due to 

# echarts4r 0.1.2

* `e_labels` to easily add labels.
* Mapbox support see `e_mapbox`.
* Improved `e_scatter`.
* Improved `e_toolbox`.
* Fixed `e_axis` (and corresponding `e_x_axis` and `e_y_axis`) bugs.
* `e_add` added for nested data.
* `e_showtip_p` and `e_hidetip_p` fixed.
* `e_focus_adjacency_p` and `e_unfocus_adjacency_p` proxies to highlight adjacent nodes (on graph/network) added.
* Deprecation of texture related functions in favour of [echarts4r.assets](http://echarts4r-assets.john-coene.com/).
* `e_single_axis` added, works with `e_scatter`.
* `e_text_style` added, to customise global fonts.
* `e_angle_axis` now takes `serie` argument, see example.
* Improved documentation of many functions.

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

* Added actions that can be tied to buttons.
* Added the ability to add buttons.
* Added `e_arrange` & `e_connect` to connect charts interactions and visualise them together.
* Better supprot for radial, angle, and polar charts.
* Callbacks added `e_on` and `e_off`.
* `e_map_custom` added to work with custom registered maps.
* `e_datazoom` takes a `toolbox` argument to optionally not the `dataZoom` toolbox element.
* Much improved documentation
* `e_map` and `e_map_3d` no longer require the `serie` argument.
* `e_mark_*`: The `serie` argument of the mark family functions (i.e.: `e_mark_point`) can now take a vector _and_ if left `NULL` the point is marked on all series.

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

