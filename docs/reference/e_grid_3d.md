# Grid

Customise grid.

## Usage

``` r
e_grid_3d(e, index = 0, ...)
```

## Arguments

- e:

  An `echarts4r` object as returned by
  [`e_charts`](https://echarts4r.john-coene.com/reference/init.md) or a
  proxy as returned by
  [`echarts4rProxy`](https://echarts4r.john-coene.com/reference/echarts4r-shiny.md).

- index:

  Index of axis to customise.

- ...:

  Any other option to pass, check See Also section.

## See also

[Additional
arguments](https://echarts.apache.org/en/option-gl.html#grid3D)

## Examples

``` r
# phony data
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
  e_grid_3d(splitLine = list(lineStyle = list(color = "blue")))

{"x":{"theme":"","tl":false,"draw":true,"renderer":"canvas","events":[],"buttons":[],"settings":{"crosstalk_key":null,"crosstalk_group":null},"opts":{"xAxis3D":[{"type":"category","data":["A","B","C","D","E","F","G","H","I","J"]}],"yAxis3D":[{"type":"category","data":["A","B","C","D","E","G","H","I","J","F"]}],"zAxis3D":[{"type":"value"}],"grid3D":[{"show":true,"splitLine":{"lineStyle":{"color":"blue"}}}],"legend":{"data":["Serie 1","Serie 2"]},"series":[{"name":"Serie 1","type":"bar3D","coordinateSystem":"cartesian3D","data":[{"value":["A","A","38.413403"]},{"value":["A","B","21.879468"]},{"value":["A","C","37.383994"]},{"value":["A","D","22.745511"]},{"value":["A","E","58.697645"]},{"value":["A","G"," 9.746510"]},{"value":["A","H","20.553786"]},{"value":["A","I","18.850679"]},{"value":["A","J","27.944626"]},{"value":["B","A","70.925846"]},{"value":["B","B","21.718371"]},{"value":["B","C","51.631651"]},{"value":["B","D"," 9.919840"]},{"value":["B","E","20.580026"]},{"value":["B","F"," 9.426561"]},{"value":["B","G","42.234444"]},{"value":["B","H","19.205849"]},{"value":["B","I","20.177775"]},{"value":["B","J","40.075193"]},{"value":["C","A","39.954620"]},{"value":["C","B","21.578278"]},{"value":["C","C","62.245521"]},{"value":["C","D","29.384202"]},{"value":["C","E","29.309653"]},{"value":["C","G","29.053802"]},{"value":["C","H","20.320833"]},{"value":["C","I","21.518929"]},{"value":["C","J","50.215034"]},{"value":["D","A","30.200188"]},{"value":["D","B","20.805184"]},{"value":["D","C","45.253193"]},{"value":["D","D","59.496814"]},{"value":["D","E","31.845738"]},{"value":["D","F","40.013989"]},{"value":["D","G","39.599312"]},{"value":["D","H","10.285622"]},{"value":["D","J"," 9.665886"]},{"value":["E","A","41.180189"]},{"value":["E","B","19.161568"]},{"value":["E","C","41.150882"]},{"value":["E","D","40.318415"]},{"value":["E","E","48.828650"]},{"value":["E","G","29.496764"]},{"value":["E","H","30.946426"]},{"value":["E","I","38.396422"]},{"value":["E","J","60.763799"]},{"value":["F","A","17.660902"]},{"value":["F","B","37.693356"]},{"value":["F","C","29.847912"]},{"value":["F","D","58.816238"]},{"value":["F","E","32.664998"]},{"value":["F","F"," 8.865362"]},{"value":["F","G","18.674898"]},{"value":["F","H","33.418206"]},{"value":["F","I","25.509117"]},{"value":["F","J","58.476720"]},{"value":["G","A"," 9.739160"]},{"value":["G","B","48.785143"]},{"value":["G","C"," 9.128743"]},{"value":["G","D","40.165372"]},{"value":["G","E","31.626147"]},{"value":["G","F","10.392218"]},{"value":["G","G","22.215972"]},{"value":["G","H","29.230345"]},{"value":["G","I","20.438617"]},{"value":["G","J","38.706552"]},{"value":["H","A","57.461235"]},{"value":["H","B","16.803309"]},{"value":["H","C"," 8.595303"]},{"value":["H","D","27.473537"]},{"value":["H","E","19.246674"]},{"value":["H","F","53.388957"]},{"value":["H","G","19.440925"]},{"value":["H","H","19.544040"]},{"value":["H","I","31.388387"]},{"value":["H","J","32.946293"]},{"value":["I","A","30.246096"]},{"value":["I","B"," 9.868484"]},{"value":["I","C","26.692428"]},{"value":["I","D","30.938870"]},{"value":["I","E","32.059321"]},{"value":["I","F","16.557350"]},{"value":["I","G","41.152091"]},{"value":["I","H","46.182638"]},{"value":["I","I","31.970862"]},{"value":["I","J","28.040935"]},{"value":["J","A","30.556335"]},{"value":["J","B"," 9.895108"]},{"value":["J","C","49.794106"]},{"value":["J","D","55.991006"]},{"value":["J","E","10.300943"]},{"value":["J","F","42.073127"]},{"value":["J","G","17.462425"]},{"value":["J","H","38.466825"]},{"value":["J","I","52.488973"]},{"value":["J","J"," 8.487839"]}],"stack":"stack","itemStyle":{"opacity":0.4},"emphasis":{"itemStyle":{"color":"#313695"}}},{"name":"Serie 2","type":"bar3D","coordinateSystem":"cartesian3D","data":[{"value":["A","A","37.403900"]},{"value":["A","B","19.944192"]},{"value":["A","C","38.502542"]},{"value":["A","D","19.366972"]},{"value":["A","E","60.343037"]},{"value":["A","G"," 9.777743"]},{"value":["A","H","18.655616"]},{"value":["A","I","21.083142"]},{"value":["A","J","30.408646"]},{"value":["B","A","69.667620"]},{"value":["B","B","17.912110"]},{"value":["B","C","50.295231"]},{"value":["B","D"," 9.843662"]},{"value":["B","E","16.950302"]},{"value":["B","F","11.143774"]},{"value":["B","G","39.151162"]},{"value":["B","H","17.936007"]},{"value":["B","I","20.109094"]},{"value":["B","J","39.423645"]},{"value":["C","A","45.086191"]},{"value":["C","B","20.272156"]},{"value":["C","C","53.337067"]},{"value":["C","D","29.482126"]},{"value":["C","E","30.811545"]},{"value":["C","G","27.501526"]},{"value":["C","H","19.461791"]},{"value":["C","I","20.181122"]},{"value":["C","J","50.157998"]},{"value":["D","A","31.254909"]},{"value":["D","B","20.105255"]},{"value":["D","C","50.857715"]},{"value":["D","D","56.888404"]},{"value":["D","E","29.859531"]},{"value":["D","F","39.236820"]},{"value":["D","G","39.188255"]},{"value":["D","H","10.299036"]},{"value":["D","J","10.560454"]},{"value":["E","A","39.195379"]},{"value":["E","B","19.766734"]},{"value":["E","C","36.582541"]},{"value":["E","D","41.998974"]},{"value":["E","E","46.973422"]},{"value":["E","G","29.789700"]},{"value":["E","H","30.436287"]},{"value":["E","I","38.468400"]},{"value":["E","J","59.220164"]},{"value":["F","A","21.103143"]},{"value":["F","B","39.582421"]},{"value":["F","C","30.335751"]},{"value":["F","D","62.329571"]},{"value":["F","E","30.104844"]},{"value":["F","F","10.324774"]},{"value":["F","G","21.443998"]},{"value":["F","H","28.423513"]},{"value":["F","I","30.578610"]},{"value":["F","J","60.172339"]},{"value":["G","A"," 9.417241"]},{"value":["G","B","55.327596"]},{"value":["G","C","10.979521"]},{"value":["G","D","36.893231"]},{"value":["G","E","28.672832"]},{"value":["G","F","11.304537"]},{"value":["G","G","21.873736"]},{"value":["G","H","31.254363"]},{"value":["G","I","20.056322"]},{"value":["G","J","41.655182"]},{"value":["H","A","58.682066"]},{"value":["H","B","24.015237"]},{"value":["H","C"," 9.400077"]},{"value":["H","D","28.502584"]},{"value":["H","E","19.634947"]},{"value":["H","F","50.285440"]},{"value":["H","G","21.327450"]},{"value":["H","H","21.051240"]},{"value":["H","I","27.903134"]},{"value":["H","J","31.784719"]},{"value":["I","A","30.547895"]},{"value":["I","B","10.534503"]},{"value":["I","C","31.673322"]},{"value":["I","D","30.552732"]},{"value":["I","E","29.747498"]},{"value":["I","F","21.508005"]},{"value":["I","G","38.633154"]},{"value":["I","H","45.846450"]},{"value":["I","I","28.633370"]},{"value":["I","J","27.598478"]},{"value":["J","A","29.124095"]},{"value":["J","B","12.332930"]},{"value":["J","C","49.666399"]},{"value":["J","D","57.240486"]},{"value":["J","E","10.092854"]},{"value":["J","F","35.722089"]},{"value":["J","G","17.658024"]},{"value":["J","H","42.094991"]},{"value":["J","I","47.353816"]},{"value":["J","J"," 8.919260"]}],"stack":"stack","itemStyle":{"opacity":0.4},"emphasis":{"itemStyle":{"color":"#313695"}}}]},"dispose":true},"evals":[],"jsHooks":[]}
```
