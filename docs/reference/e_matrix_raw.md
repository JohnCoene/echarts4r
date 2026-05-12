# Generate Matrix

helper function for generating default

## Usage

``` r
e_matrix_raw(rows = NULL, cols = NULL, ...)
```

## Arguments

- rows, cols:

  provide integer values for the number of rows and columns in the
  matrix grid

- ...:

  Any other option to pass, check See Also section.

## See also

[Additional arguments](https://echarts.apache.org/en/option.html#matrix)

## Examples

``` r
e_matrix_raw(rows = 3, cols = 3, backgroundStyle=list(borderWidth=0))

{"x":{"theme":"","tl":false,"draw":true,"renderer":"canvas","events":[],"buttons":[],"settings":{"crosstalk_key":null,"crosstalk_group":null},"opts":{"matrix":{"x":{"data":[null,null,null],"show":false},"y":{"data":[null,null,null],"show":false},"backgroundStyle":{"borderWidth":0}}},"dispose":true},"evals":[],"jsHooks":[]}
e_matrix_raw(rows = 3, cols = 3, body = list(itemStyle = list(borderWidth = 0)))

{"x":{"theme":"","tl":false,"draw":true,"renderer":"canvas","events":[],"buttons":[],"settings":{"crosstalk_key":null,"crosstalk_group":null},"opts":{"matrix":{"x":{"data":[null,null,null],"show":false},"y":{"data":[null,null,null],"show":false},"body":{"itemStyle":{"borderWidth":0}}}},"dispose":true},"evals":[],"jsHooks":[]}
```
