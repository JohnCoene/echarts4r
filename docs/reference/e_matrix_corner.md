# Fill Matrix Axis Corner

helper function for adding data to the corner of matrix

## Usage

``` r
e_matrix_corner(
  e,
  coord = c(-1, -1),
  value,
  mergeCells = TRUE,
  coordClamp = FALSE,
  ...
)
```

## Arguments

- e:

  An `echarts4r` object as returned by
  [`e_charts`](https://echarts4r.john-coene.com/reference/init.md) or a
  proxy as returned by
  [`echarts4rProxy`](https://echarts4r.john-coene.com/reference/echarts4r-shiny.md).

- coord:

  corner cell coordinate location

- value:

  text to display in corner cell

- mergeCells:

  whether the body cells and corner cells can be merged

- coordClamp:

  determines whether null values can be used to indicate an entire
  row/column

- ...:

  Any other option to pass, check See Also section.

## See also

[Additional
arguments](https://echarts.apache.org/en/option.html#matrix.corner)

## Examples

``` r
df <- data.frame("Class" = rep(c("Class1", "Class2", "Class3"),each = 3),
"Grade" = c("Grade1","Grade2", "Grade3"),
"A" = sample(1:10, 9),
"B" = sample(1:10,9))

df |> e_charts() |> e_matrix(xAxis = "Class", yAxis = "Grade") |>
e_matrix_parent(value = "Primary", children = c("Class1", "Class2")) |>
e_matrix_parent(value = "High", children = "Class3") |>
e_matrix_corner(value = "All School",
label = list(fontSize = 24, color = "#555", position = "inside"))

{"x":{"theme":"","tl":false,"draw":true,"renderer":"canvas","events":[],"buttons":[],"settings":{"crosstalk_key":null,"crosstalk_group":null},"opts":{"matrix":{"x":{"data":[{"value":"Primary","children":["Class1","Class2"]},{"value":"High","children":["Class3"]}],"name":"Class"},"y":{"data":["Grade1","Grade2","Grade3"],"name":"Grade"},"corner":{"data":[{"coord":[-1,-1],"value":"All School","mergeCells":true,"coordClamp":false}],"label":{"fontSize":24,"color":"#555","position":"inside"}}}},"dispose":true},"evals":[],"jsHooks":[]}
```
