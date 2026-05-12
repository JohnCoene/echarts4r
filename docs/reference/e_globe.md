# Globe

Add globe.

## Usage

``` r
e_globe(e, environment = NULL, base_texture = NULL, height_texture = NULL, ...)
```

## Arguments

- e:

  An `echarts4r` object as returned by
  [`e_charts`](https://echarts4r.john-coene.com/reference/init.md) or a
  proxy as returned by
  [`echarts4rProxy`](https://echarts4r.john-coene.com/reference/echarts4r-shiny.md).

- environment:

  Texture of background.

- base_texture:

  Base texture of globe.

- height_texture:

  Texture of height.

- ...:

  Any other option to pass, check See Also section.

## See also

[`e_country_names`](https://echarts4r.john-coene.com/reference/e_country_names.md),
[Additional
arguments](https://echarts.apache.org/en/option-gl.html#globe)

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
  e_globe(
    displacementScale = 0.04
  ) |>
  e_bar_3d(lat, value, "globe") |>
  e_visual_map(show = FALSE)
} # }
```
