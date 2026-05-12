# Axis 3D

Customise 3D axis.

## Usage

``` r
e_axis_3d(e, axis = c("x", "y", "z"), index = 0, ...)

e_x_axis_3d(e, index = 0, ...)

e_y_axis_3d(e, index = 0, ...)

e_z_axis_3d(e, index = 0, ...)
```

## Arguments

- e:

  An `echarts4r` object as returned by
  [`e_charts`](https://echarts4r.john-coene.com/reference/init.md) or a
  proxy as returned by
  [`echarts4rProxy`](https://echarts4r.john-coene.com/reference/echarts4r-shiny.md).

- axis:

  Axis to customise.

- index:

  Index of axis to customise.

- ...:

  Any other option to pass, check See Also section.

## See also

[Additional x
arguments](https://echarts.apache.org/en/option-gl.html#xAxis3D),
[Additional y
arguments](https://echarts.apache.org/en/option-gl.html#yAxis3D),
[Additional z
arguments](https://echarts.apache.org/en/option-gl.html#zAxis3D)

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
  e_x_axis_3d(axisLine = list(lineStyle = list(color = "blue")))

{"x":{"theme":"","tl":false,"draw":true,"renderer":"canvas","events":[],"buttons":[],"settings":{"crosstalk_key":null,"crosstalk_group":null},"opts":{"xAxis3D":[{"type":"category","data":["A","B","C","D","E","F","G","H","I","J"],"axisLine":{"lineStyle":{"color":"blue"}}}],"yAxis3D":[{"type":"category","data":["A","B","C","D","F","G","H","I","J","E"]}],"zAxis3D":[{"type":"value"}],"grid3D":[{"show":true}],"legend":{"data":["Serie 1","Serie 2"]},"series":[{"name":"Serie 1","type":"bar3D","coordinateSystem":"cartesian3D","data":[{"value":["A","A"," 63.265440"]},{"value":["A","B"," 40.717517"]},{"value":["A","C"," 25.053953"]},{"value":["A","D"," 30.114120"]},{"value":["A","F"," 19.471472"]},{"value":["A","G"," 42.497981"]},{"value":["A","H"," 10.791534"]},{"value":["A","I"," 29.775953"]},{"value":["A","J"," 22.355246"]},{"value":["B","A"," 36.869958"]},{"value":["B","B"," 10.303848"]},{"value":["B","C"," 83.945983"]},{"value":["B","D"," 16.247874"]},{"value":["B","F"," 50.829174"]},{"value":["B","G"," 40.356684"]},{"value":["B","H"," 10.028357"]},{"value":["B","I"," 29.565625"]},{"value":["B","J"," 67.144393"]},{"value":["C","A"," 43.503708"]},{"value":["C","B"," 39.186942"]},{"value":["C","C"," 11.170056"]},{"value":["C","D"," 51.741780"]},{"value":["C","E"," 20.400340"]},{"value":["C","F"," 19.341293"]},{"value":["C","G"," 31.498521"]},{"value":["C","H","  8.645072"]},{"value":["C","I"," 19.473562"]},{"value":["C","J"," 42.064791"]},{"value":["D","A"," 28.373887"]},{"value":["D","B","  9.299440"]},{"value":["D","C"," 27.313951"]},{"value":["D","D"," 10.476844"]},{"value":["D","E"," 17.626002"]},{"value":["D","F"," 29.943936"]},{"value":["D","G"," 30.701857"]},{"value":["D","H"," 38.040054"]},{"value":["D","J"," 51.836922"]},{"value":["E","A"," 30.212931"]},{"value":["E","B"," 29.590195"]},{"value":["E","D"," 18.130478"]},{"value":["E","E"," 28.646270"]},{"value":["E","F"," 25.612796"]},{"value":["E","G"," 29.054849"]},{"value":["E","H"," 61.010453"]},{"value":["E","I"," 50.446596"]},{"value":["E","J"," 27.335579"]},{"value":["F","A"," 39.320095"]},{"value":["F","B"," 32.240625"]},{"value":["F","C"," 20.250088"]},{"value":["F","D"," 58.328092"]},{"value":["F","E"," 27.059195"]},{"value":["F","F"," 19.533713"]},{"value":["F","G"," 48.533770"]},{"value":["F","H"," 21.352927"]},{"value":["F","I"," 20.277331"]},{"value":["F","J"," 39.318187"]},{"value":["G","A"," 37.234023"]},{"value":["G","B"," 19.663777"]},{"value":["G","C"," 19.712900"]},{"value":["G","D"," 18.906237"]},{"value":["G","E"," 28.980541"]},{"value":["G","F"," 31.886909"]},{"value":["G","H"," 37.889970"]},{"value":["G","I"," 48.098370"]},{"value":["G","J"," 58.039521"]},{"value":["H","A"," 50.123117"]},{"value":["H","B"," 18.111461"]},{"value":["H","C"," 57.944606"]},{"value":["H","D"," 19.315956"]},{"value":["H","E"," 11.369776"]},{"value":["H","F"," 10.362159"]},{"value":["H","G"," 17.878152"]},{"value":["H","H"," 11.433701"]},{"value":["H","I"," 37.700514"]},{"value":["H","J"," 10.063919"]},{"value":["I","A"," 29.694539"]},{"value":["I","B"," 49.185857"]},{"value":["I","C"," 19.537893"]},{"value":["I","D"," 37.171589"]},{"value":["I","E","  9.837883"]},{"value":["I","F"," 42.899733"]},{"value":["I","G"," 18.545395"]},{"value":["I","H"," 41.682941"]},{"value":["I","I"," 10.626310"]},{"value":["I","J"," 31.754732"]},{"value":["J","A"," 10.736068"]},{"value":["J","B"," 39.439238"]},{"value":["J","C"," 31.779978"]},{"value":["J","D"," 38.644664"]},{"value":["J","E","101.434774"]},{"value":["J","G"," 30.575785"]},{"value":["J","H"," 19.591690"]},{"value":["J","I"," 39.621060"]},{"value":["J","J"," 37.648541"]}],"stack":"stack","itemStyle":{"opacity":0.4},"emphasis":{"itemStyle":{"color":"#313695"}}},{"name":"Serie 2","type":"bar3D","coordinateSystem":"cartesian3D","data":[{"value":["A","A","57.323069"]},{"value":["A","B","39.390543"]},{"value":["A","C","27.582334"]},{"value":["A","D","31.019027"]},{"value":["A","F","19.709857"]},{"value":["A","G","40.712966"]},{"value":["A","H"," 9.130161"]},{"value":["A","I","32.783241"]},{"value":["A","J","19.741780"]},{"value":["B","A","38.060176"]},{"value":["B","B"," 8.346719"]},{"value":["B","C","78.291348"]},{"value":["B","D","18.274229"]},{"value":["B","F","48.343668"]},{"value":["B","G","38.775050"]},{"value":["B","H"," 9.386820"]},{"value":["B","I","27.660358"]},{"value":["B","J","70.306347"]},{"value":["C","A","39.829361"]},{"value":["C","B","43.889963"]},{"value":["C","C"," 9.140464"]},{"value":["C","D","51.781195"]},{"value":["C","E","19.442855"]},{"value":["C","F","21.418344"]},{"value":["C","G","32.276883"]},{"value":["C","H","10.539466"]},{"value":["C","I","18.418222"]},{"value":["C","J","38.578814"]},{"value":["D","A","29.152382"]},{"value":["D","B"," 9.084079"]},{"value":["D","C","34.756639"]},{"value":["D","D","11.212490"]},{"value":["D","E","19.339507"]},{"value":["D","F","29.982206"]},{"value":["D","G","31.072660"]},{"value":["D","H","38.784021"]},{"value":["D","J","47.720683"]},{"value":["E","A","30.763996"]},{"value":["E","B","29.013676"]},{"value":["E","D","19.520783"]},{"value":["E","E","28.652849"]},{"value":["E","F","32.841137"]},{"value":["E","G","31.078571"]},{"value":["E","H","59.411032"]},{"value":["E","I","47.532755"]},{"value":["E","J","27.906170"]},{"value":["F","A","38.645081"]},{"value":["F","B","25.545925"]},{"value":["F","C","20.733582"]},{"value":["F","D","59.816944"]},{"value":["F","E","32.778008"]},{"value":["F","F","21.524803"]},{"value":["F","G","47.041266"]},{"value":["F","H","19.396463"]},{"value":["F","I","21.184520"]},{"value":["F","J","43.300262"]},{"value":["G","A","38.880487"]},{"value":["G","B","19.444310"]},{"value":["G","C","20.471177"]},{"value":["G","D","20.706461"]},{"value":["G","E","32.956102"]},{"value":["G","F","29.634695"]},{"value":["G","H","39.586698"]},{"value":["G","I","45.043167"]},{"value":["G","J","60.567965"]},{"value":["H","A","50.435653"]},{"value":["H","B","20.656170"]},{"value":["H","C","59.828500"]},{"value":["H","D","21.037008"]},{"value":["H","E"," 9.803681"]},{"value":["H","F","10.139131"]},{"value":["H","G","18.326312"]},{"value":["H","H","10.375116"]},{"value":["H","I","40.117224"]},{"value":["H","J","10.266169"]},{"value":["I","A","33.034400"]},{"value":["I","B","52.562119"]},{"value":["I","C","18.738313"]},{"value":["I","D","40.362678"]},{"value":["I","E","10.383657"]},{"value":["I","F","40.452191"]},{"value":["I","G","21.722957"]},{"value":["I","H","40.642591"]},{"value":["I","I","10.743228"]},{"value":["I","J","29.830628"]},{"value":["J","A","10.272059"]},{"value":["J","B","38.147631"]},{"value":["J","C","31.356145"]},{"value":["J","D","43.301173"]},{"value":["J","E","99.049619"]},{"value":["J","G","31.051042"]},{"value":["J","H","19.678744"]},{"value":["J","I","42.980212"]},{"value":["J","J","39.476486"]}],"stack":"stack","itemStyle":{"opacity":0.4},"emphasis":{"itemStyle":{"color":"#313695"}}}]},"dispose":true},"evals":[],"jsHooks":[]}
```
