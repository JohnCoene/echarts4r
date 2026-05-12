# Register map

Register a [geojson](https://geojson.org/) map.

## Usage

``` r
e_map_register(e, name, json, ...)

e_svg_register(e, name, svg)

e_map_register_p(
  name,
  json,
  async = FALSE,
  session = shiny::getDefaultReactiveDomain()
)

e_map_register_ui(name, json, async = FALSE)
```

## Arguments

- e:

  An `echarts4r` object as returned by
  [`e_charts`](https://echarts4r.john-coene.com/reference/init.md).

- name:

  Name of map, to used in
  [`e_map`](https://echarts4r.john-coene.com/reference/map.md).

- json, svg:

  [Geojson](https://geojson.org/), or SVG.

- ...:

  Additional options passed to
  [registerMap](https://echarts.apache.org/en/api.html#echarts.registerMap).

- async:

  Whether to read the file asynchronously.

- session:

  A valid Shiny session.

## Details

`e_map_register_p` is not truly a proxy as it does not require a chart
to function. While the function `e_map_register_ui` is meant to register
the map globally in the Shiny UI, not that then `json` must be
accessible from the UI (generally www folder).

## Examples

``` r
if (FALSE) { # \dontrun{
json <- jsonlite::read_json("https://echarts.apache.org/examples/data/asset/geo/USA.json")

USArrests |>
  tibble::rownames_to_column("states") |>
  e_charts(states) |>
  e_map_register("USA", json) |>
  e_map(Murder, map = "USA") |>
  e_visual_map(Murder)
} # }
```
