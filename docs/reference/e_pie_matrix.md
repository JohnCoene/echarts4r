# Generate pie chart for matrix

Draw pie or donut chart in matrix coordinate system

## Usage

``` r
e_pie_matrix(e, x, y, legend = TRUE, ...)
```

## Arguments

- e:

  An `echarts4r` object as returned by
  [`e_charts`](https://echarts4r.john-coene.com/reference/init.md) or a
  proxy as returned by
  [`echarts4rProxy`](https://echarts4r.john-coene.com/reference/echarts4r-shiny.md).

- x, y:

  x and y column names for data to be used in the pie chart

- legend:

  Whether to add a legend, defaults to `TRUE`.

- ...:

  Any other option to pass, check See Also section.

## See also

[Additional
arguments](https://echarts.apache.org/en/option.html#series-pie)

## Examples

``` r
df <- data.frame("Class" = rep(c("Class1", "Class2", "Class3"),each = 3),
                   "Grade" = c("Grade1","Grade2", "Grade3"),
                  "A" = sample(1:10, 9),
                  "B" = sample(1:10,9),
                  "C" = sample(1:10,9))

df |> e_chart(x = A) |>
 e_matrix(xAxis = "Class", yAxis = "Grade") |>
 e_matrix_parent(value = "Primary", children = c("Class1", "Class2")) |>
 e_matrix_parent(value = "High", children = "Class3") |>
 e_matrix_corner(value = "All School",
   label = list(fontSize = 24, color = "#555", position = "inside")) |>
 e_pie(B, coord_system = "matrix", label = list(show = FALSE)) |>
 e_tooltip(trigger = "item") |>
 e_legend()

{"x":{"theme":"","tl":false,"draw":true,"renderer":"canvas","events":[],"buttons":[],"settings":{"crosstalk_key":null,"crosstalk_group":null},"opts":{"matrix":{"x":{"data":[{"value":"Primary","children":["Class1","Class2"]},{"value":"High","children":["Class3"]}],"name":"Class"},"y":{"data":["Grade3","Grade2","Grade1"],"name":"Grade"},"corner":{"data":[{"coord":[-1,-1],"value":"All School","mergeCells":true,"coordClamp":false}],"label":{"fontSize":24,"color":"#555","position":"inside"}}},"series":[{"type":"pie","coordinateSystem":"matrix","center":["Class1","Grade3"],"data":[{"name":"A","value":2},{"name":"B","value":10}],"label":{"show":false}},{"type":"pie","coordinateSystem":"matrix","center":["Class1","Grade2"],"data":[{"name":"A","value":8},{"name":"B","value":7}],"label":{"show":false}},{"type":"pie","coordinateSystem":"matrix","center":["Class1","Grade1"],"data":[{"name":"A","value":9},{"name":"B","value":8}],"label":{"show":false}},{"type":"pie","coordinateSystem":"matrix","center":["Class2","Grade3"],"data":[{"name":"A","value":10},{"name":"B","value":2}],"label":{"show":false}},{"type":"pie","coordinateSystem":"matrix","center":["Class2","Grade2"],"data":[{"name":"A","value":6},{"name":"B","value":3}],"label":{"show":false}},{"type":"pie","coordinateSystem":"matrix","center":["Class2","Grade1"],"data":[{"name":"A","value":4},{"name":"B","value":5}],"label":{"show":false}},{"type":"pie","coordinateSystem":"matrix","center":["Class3","Grade3"],"data":[{"name":"A","value":7},{"name":"B","value":4}],"label":{"show":false}},{"type":"pie","coordinateSystem":"matrix","center":["Class3","Grade2"],"data":[{"name":"A","value":3},{"name":"B","value":6}],"label":{"show":false}},{"type":"pie","coordinateSystem":"matrix","center":["Class3","Grade1"],"data":[{"name":"A","value":5},{"name":"B","value":1}],"label":{"show":false}}],"tooltip":{"trigger":"item"},"legend":{"show":true,"type":"plain"}},"dispose":true},"evals":[],"jsHooks":[]}
 df |> e_chart() |>
  e_matrix(xAxis = "Class", yAxis = "Grade") |>
  e_pie_(serie = c("A","B","C"),
   coord_system = "matrix",
     label = list(show = FALSE)) |>
  e_tooltip(trigger = "item") |>
  e_legend()

{"x":{"theme":"","tl":false,"draw":true,"renderer":"canvas","events":[],"buttons":[],"settings":{"crosstalk_key":null,"crosstalk_group":null},"opts":{"matrix":{"x":{"data":["Class1","Class2","Class3"],"name":"Class"},"y":{"data":["Grade1","Grade2","Grade3"],"name":"Grade"}},"series":[{"type":"pie","coordinateSystem":"matrix","center":["Class1","Grade1"],"data":[{"name":"A","value":9},{"name":"B","value":8},{"name":"C","value":6}],"label":{"show":false}},{"type":"pie","coordinateSystem":"matrix","center":["Class1","Grade2"],"data":[{"name":"A","value":8},{"name":"B","value":7},{"name":"C","value":8}],"label":{"show":false}},{"type":"pie","coordinateSystem":"matrix","center":["Class1","Grade3"],"data":[{"name":"A","value":2},{"name":"B","value":10},{"name":"C","value":1}],"label":{"show":false}},{"type":"pie","coordinateSystem":"matrix","center":["Class2","Grade1"],"data":[{"name":"A","value":4},{"name":"B","value":5},{"name":"C","value":9}],"label":{"show":false}},{"type":"pie","coordinateSystem":"matrix","center":["Class2","Grade2"],"data":[{"name":"A","value":6},{"name":"B","value":3},{"name":"C","value":2}],"label":{"show":false}},{"type":"pie","coordinateSystem":"matrix","center":["Class2","Grade3"],"data":[{"name":"A","value":10},{"name":"B","value":2},{"name":"C","value":4}],"label":{"show":false}},{"type":"pie","coordinateSystem":"matrix","center":["Class3","Grade1"],"data":[{"name":"A","value":5},{"name":"B","value":1},{"name":"C","value":5}],"label":{"show":false}},{"type":"pie","coordinateSystem":"matrix","center":["Class3","Grade2"],"data":[{"name":"A","value":3},{"name":"B","value":6},{"name":"C","value":10}],"label":{"show":false}},{"type":"pie","coordinateSystem":"matrix","center":["Class3","Grade3"],"data":[{"name":"A","value":7},{"name":"B","value":4},{"name":"C","value":7}],"label":{"show":false}}],"tooltip":{"trigger":"item"},"legend":{"show":true,"type":"plain"}},"dispose":true},"evals":[],"jsHooks":[]}
 df |> e_chart() |>
 e_matrix(xAxis = "Class", yAxis = "Grade") |>
 e_pie_matrix(x = "A", y = c( "B", "C"))

{"x":{"theme":"","tl":false,"draw":true,"renderer":"canvas","events":[],"buttons":[],"settings":{"crosstalk_key":null,"crosstalk_group":null},"opts":{"matrix":{"x":{"data":["Class1","Class2","Class3"],"name":"Class"},"y":{"data":["Grade1","Grade2","Grade3"],"name":"Grade"}},"series":[{"type":"pie","coordinateSystem":"matrix","center":["Class1","Grade1"],"data":[{"name":"A","value":9},{"name":"B","value":8},{"name":"C","value":6}]},{"type":"pie","coordinateSystem":"matrix","center":["Class1","Grade2"],"data":[{"name":"A","value":8},{"name":"B","value":7},{"name":"C","value":8}]},{"type":"pie","coordinateSystem":"matrix","center":["Class1","Grade3"],"data":[{"name":"A","value":2},{"name":"B","value":10},{"name":"C","value":1}]},{"type":"pie","coordinateSystem":"matrix","center":["Class2","Grade1"],"data":[{"name":"A","value":4},{"name":"B","value":5},{"name":"C","value":9}]},{"type":"pie","coordinateSystem":"matrix","center":["Class2","Grade2"],"data":[{"name":"A","value":6},{"name":"B","value":3},{"name":"C","value":2}]},{"type":"pie","coordinateSystem":"matrix","center":["Class2","Grade3"],"data":[{"name":"A","value":10},{"name":"B","value":2},{"name":"C","value":4}]},{"type":"pie","coordinateSystem":"matrix","center":["Class3","Grade1"],"data":[{"name":"A","value":5},{"name":"B","value":1},{"name":"C","value":5}]},{"type":"pie","coordinateSystem":"matrix","center":["Class3","Grade2"],"data":[{"name":"A","value":3},{"name":"B","value":6},{"name":"C","value":10}]},{"type":"pie","coordinateSystem":"matrix","center":["Class3","Grade3"],"data":[{"name":"A","value":7},{"name":"B","value":4},{"name":"C","value":7}]}]},"dispose":true},"evals":[],"jsHooks":[]}
```
