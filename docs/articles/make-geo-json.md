# GeoJSON data

## Introduction

You will find *215 maps* in the companion package
[echarts4r.maps](https://echarts4r-maps.john-coene.com/). However, since
you may need different kinds of maps i.e.: at the city or county level
that are not included in this package, you may need to get detailed map
data from a third-party source such as [gadm.org](https://gadm.org/).
This document shows how to make your own `GeoJSON` data which can be
used in `e_map_register`.

## GeoJSON structure

A GeoJSON object used with `e_map_register` is a list with a `type` of
`"FeatureCollection"` and a `features` array. Each feature needs a
`properties` list with at least a `name` field (used by echarts for
tooltips and matching data) and a `geometry` defining the shape.

Below we build a small example with three rectangular regions entirely
in R — no external data or downloads required.

``` r
library(echarts4r)

my_geojson <- list(
  type = "FeatureCollection",
  features = list(
    list(
      type = "Feature",
      properties = list(name = "Region A"),
      geometry = list(
        type = "Polygon",
        coordinates = list(list(
          c(0, 0), c(2, 0), c(2, 2), c(0, 2), c(0, 0)
        ))
      )
    ),
    list(
      type = "Feature",
      properties = list(name = "Region B"),
      geometry = list(
        type = "Polygon",
        coordinates = list(list(
          c(2, 0), c(4, 0), c(4, 2), c(2, 2), c(2, 0)
        ))
      )
    ),
    list(
      type = "Feature",
      properties = list(name = "Region C"),
      geometry = list(
        type = "Polygon",
        coordinates = list(list(
          c(0, 2), c(4, 2), c(4, 4), c(0, 4), c(0, 2)
        ))
      )
    )
  )
)
```

## Registering and plotting the map

Once you have a GeoJSON list, pass it to `e_map_register` and then use
`e_map` to plot data on it. The `name` column in your data must match
the `name` property in the GeoJSON features.

``` r
data <- data.frame(
  name = c("Region A", "Region B", "Region C"),
  value = c(10, 25, 40)
)

data |>
  e_charts(name) |>
  e_map_register("my_map", my_geojson) |>
  e_map(value, map = "my_map") |>
  e_visual_map(value) |>
  e_tooltip()
```

## Using the nameProperty option

By default, echarts expects each GeoJSON feature to have a `name`
property. If your GeoJSON uses a different property name (e.g., `NAME_2`
from sources like [gadm.org](https://gadm.org/)), you can use the
`nameProperty` argument to tell echarts which property to use.

``` r
gadm_style_geojson <- list(
  type = "FeatureCollection",
  features = list(
    list(
      type = "Feature",
      properties = list(NAME_2 = "District X", GID_2 = "IND.1.1_1"),
      geometry = list(
        type = "Polygon",
        coordinates = list(list(
          c(0, 0), c(3, 0), c(3, 3), c(0, 3), c(0, 0)
        ))
      )
    ),
    list(
      type = "Feature",
      properties = list(NAME_2 = "District Y", GID_2 = "IND.1.2_1"),
      geometry = list(
        type = "Polygon",
        coordinates = list(list(
          c(3, 0), c(6, 0), c(6, 3), c(3, 3), c(3, 0)
        ))
      )
    )
  )
)

data2 <- data.frame(
  name = c("District X", "District Y"),
  value = c(100, 200)
)

data2 |>
  e_charts(name) |>
  e_map_register("gadm_map", gadm_style_geojson) |>
  e_map(value, map = "gadm_map", nameProperty = "NAME_2") |>
  e_visual_map(value) |>
  e_tooltip()
```

## Working with real spatial data

For real-world maps, the typical workflow is:

1.  **Obtain spatial data** — from packages like `geodata`,
    `rnaturalearth`, or download shapefiles from
    [gadm.org](https://gadm.org/).
2.  **Convert to GeoJSON** — use
    [`geojsonio::geojson_list()`](https://docs.ropensci.org/geojsonio/reference/geojson_list.html)
    or
    [`sf::st_read()`](https://r-spatial.github.io/sf/reference/st_read.html)
    combined with
    [`jsonlite::fromJSON()`](https://jeroen.r-universe.dev/jsonlite/reference/fromJSON.html).
3.  **Simplify** — detailed maps can be very large. Use
    [`rmapshaper::ms_simplify()`](http://andyteucher.ca/rmapshaper/reference/ms_simplify.md)
    to reduce file size before converting to GeoJSON.
4.  **Register and plot** — pass the GeoJSON list to `e_map_register`
    and plot with `e_map`.

``` r
# Example workflow (not run — requires geodata and geojsonio packages)
india_sp <- geodata::gadm(country = "INDIA", path = tempdir(), level = 2)
india_sp <- methods::as(india_sp, "Spatial")

# Simplify to reduce size
india_small <- rmapshaper::ms_simplify(india_sp, keep = 0.05)

# Convert to GeoJSON list
india_json <- geojsonio::geojson_list(india_small)

# Plot
e_charts() |>
  e_map_register("India", india_json) |>
  e_map(map = "India", nameProperty = "NAME_2")
```
