# echarts4r 0.3.2

- Bump echarts.js to 4.8.0 and echarts-gl to 1.1.2
- Fix `e_add` when used with timeline feature see [#165](https://github.com/JohnCoene/echarts4r/issues/165).
- Bump echart.js to 4.8.0 now in english (no longer chinese.

Dynamic dependencies:

- themes
- leaflet
- mapbox
- wordcloud
- liquidfill
- world.js (underlying world map for e_geo and e_map)
- error bars (custom renderer)

This greatly improves the performances of echarts4r which now loads much faster.

# echarts4r 0.3.1.9000

- Added convenience `e_hide_grid_lines` to easily hide grid lines.
- `e_map_register_ui` added to register a custom map in the Shiny UI, for faster serialisation.
- `e_leaflet*` functions now correctly apply the `roam` argument [#147](https://github.com/JohnCoene/echarts4r/issues/147)
- Internals changes for dplyr >= 0.7.0 
- Correct bug in [boxplot](https://echarts4r.john-coene.com/articles/stats.html#boxplot) thanks to [Matthias](https://github.com/matthias-a) in [issue #163](https://github.com/JohnCoene/echarts4r/issues/163) for the fix.
- Avoid page breaks on PDF, see [#159](https://github.com/JohnCoene/echarts4r/issues/159), thanks [Steve](https://github.com/sdmcallister).

# echarts4r 0.3.0.9000

Bumped echarts JavaScript library to `4.6.0`, [PR #121](https://github.com/JohnCoene/echarts4r/pull/121), also bumped echarts gl to `1.2.1`.

Every function that adds a serie to chart (e.g.: `e_bar`) have been turned into methods that now can be applied to proxies (`echarts4rProxy`), see the [development vignette](https://echarts4r.john-coene.com/articles/development.html) for more. This implies some minute changes to the `echarts4rProxy`: nothing broken though.

- Warning in `e_axis_fomatter` if user uses RStudio, relates to [#100](https://github.com/JohnCoene/echarts4r/issues/100)
- Fixed [#98](https://github.com/JohnCoene/echarts4r/issues/98) where a bug was introduced in `e_radar_opts` when the timeline component was added.
- `bind` argument added to `e_heatmap`
- `e_scatter_3d` correctly takes `bind` argument when `coords` is set to `cartesian3D` (previously incorrect).
- Bumped number of built-in themes to 40+.
- Added `e_remove_serie_p` to remove series from the graph.
- Added `e_axis_labels` to easily place axis labels.
- Added `e_resize` to dynamically resize the chart.
- Added `e_map_register_p` works like a proxy but does not require a chart.
- Fixed a few issues with `e_mark*` family, labels and multiple marks correctly work.
- Initialisation function now take `reorder` argument to specify whether to reorder numeric values. [#126](https://github.com/JohnCoene/echarts4r/issues/126)
- Added `e_register_theme` to register a global theme, see [#127](https://github.com/JohnCoene/echarts4r/issues/127).
- `e_theme_custom` now accepts path to a JSON file.
- `e_visual_map` drops `NA`s.
- Added `e_axis_stagger` function to stagger x axis labels.

# echarts4r 0.2.3

- Bar chart's `bind` argument has been fixed.
- `e_tooltip_choro_formatter` tooltip formatter for choropleth maps added, thanks to [Artem](https://github.com/artemklevtsov).
- Tooltip formatters correctly identify locale on UNIX systems, thanks to [Artem](https://github.com/artemklevtsov).
- `e_mark_point()`, `e_mark_line()` and `e_mark_area()` no longer use `grep()`, which means the serie will only be matched with the exact same name. Thanks @shrektan for the reporting and PR (#80, #81).
- Multiple `e_title` now allowed.
- Multiple `e_visual_map` in [timelines](https://echarts4r.john-coene.com/articles/timeline.html) is now fixed.
- Corrected all links (or so I think).
- `e_sankey` fix thanks to [Artem Klevtsov](https://github.com/artemklevtsov).
- Many dependencies removed, it is now much easier to install.

# echarts4r 0.2.2

- `draw` argument added to `e_charts`, to set whether to draw the chart, see bullet below.
- `e_draw_p` added to choose draw chart.
- `e_inspect` to inspect the output of echarts4r.
- `echarts_from_json` to create an echarts4r object from JSON options.
- `e_axis` family of functions (e.g.: `e_x_axis`) now accepts a column name as first argument to easily adapt the range of the axes, in a similar fashion as with `e_visual_map`.
- `e_charts` family forces data.frame conversion to avoid jsonlite bug.
- Fixed bug in the `formatter` argument of `e_tooltip` where passing custom function was not working.

# echarts4r 0.2.1

__Support for timeline added__

- `e_clean` removed as it was not of use anymore.
- `e_list` added to pas raw list of options.
- Improved `e_color_range` helper.
- `e_lm` & `e_loess` now support `group_by`.
- `e_scattter` scaling much more robust with new arguments. Also not takes an optional `symbol` argument.
- Jitter points with `e_scatter`
- `e_append` adapated to work with new scatter-family of function (scaling).
- `e_lines_gl` added.
- Improved examples.
- Can now register multiple maps with `e_map_regiter`.
- Error handling in `e_loess` and `e_lm` to better work with groups.
- `e_capture` to capture events not implemented.
- `e_bands` to add confidence bands.
- Added example of heatmap with geo.
- Fixed bug where the background color was not applied with `e_color`.
- `e_scatter` size forced to numeric to avoid buggy character vector caused by `apply` function.
- Forces `webgl` rendering for `e_globe`, `e_scatter_gl` and `e_flow_gl`.
- Groups are now ordered in order to ensure match across series when using `timeline = TRUE`.
- `e_theme` now also accepts `default`.
- `auritus` theme added.
- `bar.width` argument renamed to `bar_width` in `e_histogram`.
- `e_map_3d` and `e_map` correctly assign `name` argument.
- Axis and tooltip formatters helpers added `e_tooltip_item_formatter` & `e_axis_formatter`, thanks to [Artem Klevtsov](https://github.com/artemklevtsov).
- `e_error_bar` added to plot error bars.
- Ability to specify `icons` in `e_legend`.
- `e_aria` added to enable generating Accessible Rich Internet Applications.

# echarts4r 0.2.0

Arguments containing `.` have been replaced by `_` for clarity. I.e.: `coord.system` => `coord_system`.

Deprecated functions from `0.1.2` have been removed, they can be found in the [echarts4r.assets](http://echarts4r-assets.john-coene.com/) package:

- `e_convert_texture`
- `e_stars_texture`
- `e_composite_texture`
- `e_globe_texture`
- `e_map_texture`

The `scatter` family of function has been reworked: the `scale` argument now takes a scaling function. To accomodate to this, the `e_visual_map` function also takes, optionally, a `scaling` function.
Retired `e_keras_history` due to its uselessness.

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
* `e_common` to pass options that will apply to all subsequent charts.

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

# echarts4r 0.0.2

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

