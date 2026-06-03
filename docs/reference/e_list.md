# List

simply pass a list of options, similar to a `JSON`.

## Usage

``` r
e_list(e, list, append = FALSE)
```

## Arguments

- e:

  An `echarts4r` object as returned by
  [`e_charts`](https://echarts4r.john-coene.com/reference/init.md) or a
  proxy as returned by
  [`echarts4rProxy`](https://echarts4r.john-coene.com/reference/echarts4r-shiny.md).

- list:

  A `list` of options passed to `setOptions`.

- append:

  if `TRUE` the `list` is appended to the options, otherwise it
  *overwrites* everything.

## Examples

``` r
N <- 20 # data points

opts <- list(
  xAxis = list(
    type = "category",
    data = LETTERS[1:N]
  ),
  yAxis = list(
    type = "value"
  ),
  series = list(
    list(
      type = "line",
      data = round(runif(N, 5, 20))
    )
  )
)

e_charts() |>
  e_list(opts)

{"x":{"theme":"","tl":false,"draw":true,"renderer":"canvas","events":[],"buttons":[],"settings":{"crosstalk_key":null,"crosstalk_group":null},"opts":{"xAxis":{"type":"category","data":["A","B","C","D","E","F","G","H","I","J","K","L","M","N","O","P","Q","R","S","T"]},"yAxis":{"type":"value"},"series":[{"type":"line","data":[12,18,14,9,15,13,16,12,14,11,12,7,20,9,14,13,13,20,10,19]}]},"dispose":true},"evals":[],"jsHooks":[]}
```
