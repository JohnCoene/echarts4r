# Mapbox

Use mapbox.

## Usage

``` r
e_mapbox(e, token, ...)
```

## Arguments

- e:

  An `echarts4r` object as returned by
  [`e_charts`](https://echarts4r.john-coene.com/reference/init.md) or a
  proxy as returned by
  [`echarts4rProxy`](https://echarts4r.john-coene.com/reference/echarts4r-shiny.md).

- token:

  Your mapbox token from [mapbox](https://www.mapbox.com/).

- ...:

  Any option.

## Note

Mapbox may not work properly in the RSudio console.

## See also

[Official
documentation](https://echarts.apache.org/en/option-gl.html#mapbox3D.style),
[mapbox documentation](https://docs.mapbox.com/mapbox-gl-js/api/)

## Examples

``` r
if (FALSE) { # \dontrun{
url <- paste0(
  "https://echarts.apache.org/examples/",
  "data-gl/asset/data/population.json"
)
data <- jsonlite::fromJSON(url)
data <- as.data.frame(data)
names(data) <- c("lon", "lat", "value")

data |>
  e_charts(lon) |>
  e_mapbox(
    token = "YOUR_MAPBOX_TOKEN",
    style = "mapbox://styles/mapbox/dark-v9"
  ) |>
  e_bar_3d(lat, value, coord_system = "mapbox") |>
  e_visual_map()
} # }
```
