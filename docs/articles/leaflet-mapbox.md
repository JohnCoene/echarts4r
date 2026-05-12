# Leaflet & Mapbox

Leaflet only works with `e_scatter` and `e_effect_scatter`.

## Scatter

``` r
echarts4r::population |> 
  e_charts(lon) |> 
  e_leaflet() |>
  e_leaflet_tile() |>  
  e_scatter(lat, size = value, coord_system = "leaflet")
```

## Mapbox

You will need [mapbox](https://www.mapbox.com/) token.

``` r
echarts4r::population |> 
  e_charts(lon) |> 
  e_mapbox(
    token = "YOUR_TOKEN",
    style = "mapbox://styles/mapbox/dark-v9"
  ) |> 
  e_bar_3d(lat, value, coord_system = "mapbox") |> 
  e_visual_map(value)
```
