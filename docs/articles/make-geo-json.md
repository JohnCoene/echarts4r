# GeoJSON data

## Introduction

You will find *215 maps* in the companion package
[echarts4r.maps](https://echarts4r-maps.john-coene.com/). However, since
you may need different kinds of maps i.e.: at the citiy or county level
that are not included in this package, you may need to get the detailed
map data from third-party source such as [gadm.org](https://gadm.org/).
This document shows how to make your own `GeoJSON` file which can be
used in `e_map_register`.

## Packages

These are packages to help you building such maps

- `sp`: spatial data management package.
- `raster`: get spatial data from [gadm.org](https://gadm.org/).
- `geojsonio`: convert spatial data into json format.
- `rmapshaper`: simplify the spatial data.

``` r
library(echarts4r)
library(geodata)
#> Loading required package: terra
#> terra 1.9.27
library(sp)
library(raster)
library(geojsonio)
#> Registered S3 method overwritten by 'geojsonsf':
#>   method        from   
#>   print.geojson geojson
#> 
#> Attaching package: 'geojsonio'
#> The following object is masked from 'package:base':
#> 
#>     pretty
library(rmapshaper)
```

## Example

Get India map data from gadm website, this command need network
available, it will download the rds data to the current directory.

``` r
# india_sp <- geodata::gadm(country = "INDIA", path = ".", level = 2)
# 
# india_sp <- methods::as(india_sp, "Spatial")
# india_sp |> 
#   head() |> 
#   knitr::kable()
```

Note that you can then combine maps with `raster::union(map1, map2)` or
with the `em_maps` function from the
[echarts4r.maps](https://echarts4r-maps.john-coene.com) package. Then
convert the `SpatialPolygonsDataFrame` into `GeoJSON` with `geojsonio`:
*this will take a long time as the map is very detailed*.

``` r
# india_json <- geojsonio::geojson_list(india_sp)

# print(object.size(india_json), units = "Mb")
```

Therefore we can simplify the map to make it smaller.

``` r
# india_small <- rmapshaper::ms_simplify(india_sp, keep = 0.05)
# india_json_small <- geojsonio::geojson_list(india_small)
# print(object.size(india_json_small), units = "Mb") 
```

The function
[`geodata::gadm`](https://rspatial.github.io/geodata/reference/gadm.html)
gives each polygon a `NAME_*` property (where `*` is the `level`
specified) rather than just `name`. The tooltip will not display
properly unless we use the `nameMap` to point to that property, by
default echarts expects the `name` property.

Now we can use the `GeoJSON` with `e_map_register`.

``` r
# plot 
# e_charts() |>
  # e_map_register("India", india_json_small) |>
  # e_map(map = "India", nameProperty = "NAME_2")
```
