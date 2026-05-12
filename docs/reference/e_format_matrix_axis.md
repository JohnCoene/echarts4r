# Format Matrix Axis

helper function for formatting the x and y axes for a matrix grid.

## Usage

``` r
e_format_matrix_axis(e, axis = "x", ...)
```

## Arguments

- e:

  An `echarts4r` object as returned by
  [`e_charts`](https://echarts4r.john-coene.com/reference/init.md) or a
  proxy as returned by
  [`echarts4rProxy`](https://echarts4r.john-coene.com/reference/echarts4r-shiny.md).

- axis:

  indicate which axis shoud be adjusted

- ...:

  Any other option to pass, check See Also section.

## See also

[Additional arguments](https://echarts.apache.org/en/option.html#matrix)

## Examples

``` r
df <- data.frame("Class" = rep(c("Class1", "Class2", "Class3"),each = 3),
"Grade" = c("Grade1","Grade2", "Grade3"),
"A" = sample(1:10, 9),
"B" = sample(1:10,9))

df |> e_charts() |> e_matrix(xAxis = "Class", yAxis = "Grade") |>
e_format_matrix_axis(axis = "x", label = list(color = "red"))

{"x":{"theme":"","tl":false,"draw":true,"renderer":"canvas","events":[],"buttons":[],"settings":{"crosstalk_key":null,"crosstalk_group":null},"opts":{"matrix":{"x":{"data":["Class1","Class2","Class3"],"name":"Class","label":{"color":"red"}},"y":{"data":["Grade1","Grade2","Grade3"],"name":"Grade"}}},"dispose":true},"evals":[],"jsHooks":[]}
```
