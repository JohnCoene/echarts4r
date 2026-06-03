# Parallel

Draw parallel coordinates.

## Usage

``` r
e_parallel(e, ..., name = NULL, rm_x = TRUE, rm_y = TRUE, opts = list())

e_parallel_(e, ..., name = NULL, rm_x = TRUE, rm_y = TRUE, opts = list())
```

## Arguments

- e:

  An `echarts4r` object as returned by
  [`e_charts`](https://echarts4r.john-coene.com/reference/init.md) or a
  proxy as returned by
  [`echarts4rProxy`](https://echarts4r.john-coene.com/reference/echarts4r-shiny.md).

- ...:

  Columns to select from the data passed to
  [`e_charts`](https://echarts4r.john-coene.com/reference/init.md).

- name:

  name of the serie.

- rm_x, rm_y:

  Whether to remove x and y axis, defaults to `TRUE`.

- opts:

  A list of additional options to pass to the serie.

## See also

[Additional
arguments](https://echarts.apache.org/en/option.html#series-parallel)

## Examples

``` r
df <- data.frame(
  price = rnorm(5, 10),
  amount = rnorm(5, 15),
  letter = LETTERS[1:5]
)

df |>
  e_charts() |>
  e_parallel(price, amount, letter, opts = list(smooth = TRUE))

{"x":{"theme":"","tl":false,"draw":true,"renderer":"canvas","events":[],"buttons":[],"settings":{"crosstalk_key":null,"crosstalk_group":null},"opts":{"series":{"name":null,"type":"parallel","data":[["10.576402","15.73775","A"],["11.136540","14.34615","B"],["10.419682","15.66239","C"],["8.214609","14.80731","D"],["11.779514","15.81688","E"]],"smooth":true},"parallelAxis":[{"dim":0,"name":"price"},{"dim":1,"name":"amount"},{"dim":2,"name":"letter","type":"category","data":["A","B","C","D","E"]}]},"dispose":true},"evals":[],"jsHooks":[]}
```
