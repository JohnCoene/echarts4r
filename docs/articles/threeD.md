# 3D

## Surface

``` r
data <- expand.grid(
  x = seq(-3, 3, by = 0.05),
  y = seq(-3, 3, by = 0.05)
) |> 
  dplyr::mutate(z = sin(x * x + y * y) * x / 3.14)

data |> 
  e_charts(x) |> 
  e_surface(y, z, wireframe = list(show = FALSE)) |> 
  e_visual_map(z)
```

## Bar 3D

``` r
v <- LETTERS[1:10]
matrix <- data.frame(
  x = sample(v, 300, replace = TRUE), 
  y = sample(v, 300, replace = TRUE), 
  z1 = rnorm(300, 10, 1),
  z2 = rnorm(300, 10, 1),
  stringsAsFactors = FALSE
) |> 
  dplyr::group_by(x, y) |> 
  dplyr::summarise(
    z1 = sum(z1),
    z2 = sum(z2)
  ) |> 
  dplyr::ungroup() 
#> `summarise()` has regrouped the output.
#> ℹ Summaries were computed grouped by x and y.
#> ℹ Output is grouped by x.
#> ℹ Use `summarise(.groups = "drop_last")` to silence this message.
#> ℹ Use `summarise(.by = c(x, y))` for per-operation grouping
#>   (`?dplyr::dplyr_by`) instead.
  
trans <- list(opacity = 0.4) # transparency
emphasis <- list(itemStyle = list(color = "#313695"))
  
matrix |> 
  e_charts(x) |> 
  e_bar_3d(y, z1, stack = "stack", name = "Serie 1", itemStyle = trans, emphasis = emphasis) |>
  e_bar_3d(y, z2, stack = "stack", name = "Serie 2", itemStyle = trans, emphasis = emphasis) |> 
  e_legend()
```

## Scatter

``` r
v <- LETTERS[1:10]
matrix <- data.frame(
  x = sample(v, 300, replace = TRUE), 
  y = sample(v, 300, replace = TRUE), 
  z = rnorm(300, 10, 1),
  color = rnorm(300, 10, 1),
  size = rnorm(300, 10, 1),
  stringsAsFactors = FALSE
) |> 
  dplyr::group_by(x, y) |> 
  dplyr::summarise(
    z = sum(z),
    color = sum(color),
    size = sum(size)
  ) |> 
  dplyr::ungroup() 
#> `summarise()` has regrouped the output.
#> ℹ Summaries were computed grouped by x and y.
#> ℹ Output is grouped by x.
#> ℹ Use `summarise(.groups = "drop_last")` to silence this message.
#> ℹ Use `summarise(.by = c(x, y))` for per-operation grouping
#>   (`?dplyr::dplyr_by`) instead.
  
matrix |> 
  e_charts(x) |> 
  e_scatter_3d(y, z, size, color) |> 
  e_visual_map(
    size,
    inRange = list(symbolSize = c(1, 30)), # scale size
    dimension = 3 # third dimension 0 = x, y = 1, z = 2, size = 3
  ) |> 
  e_visual_map(
    color,
    inRange = list(color = c('#bf444c', '#d88273', '#f6efa6')), # scale colors
    dimension = 4, # third dimension 0 = x, y = 1, z = 2, size = 3, color = 4
    bottom = 300 # padding to avoid visual maps overlap
  )
```

## Line

``` r
df <- data.frame(
  x = 1:100,
  y = runif(100, 10, 25),
  z = rnorm(100, 100, 50)
)

df |> 
  e_charts(x) |> 
  e_line_3d(y, z, smooth = TRUE) |> 
  e_visual_map()
```
