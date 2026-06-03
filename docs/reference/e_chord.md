# Chord

Draw a Chord chart.

## Usage

``` r
e_chord(e, source, target, value, rm_x = TRUE, rm_y = TRUE, ...)

e_chord_(e, source, target, value, rm_x = TRUE, rm_y = TRUE, ...)
```

## Arguments

- e:

  An `echarts4r` object as returned by
  [`e_charts`](https://echarts4r.john-coene.com/reference/init.md) or a
  proxy as returned by
  [`echarts4rProxy`](https://echarts4r.john-coene.com/reference/echarts4r-shiny.md).

- source, target:

  Source and target columns.

- value:

  Value shared between `source` and `target`.

- rm_x, rm_y:

  Whether to remove the x and y axis, defaults to `TRUE`.

- ...:

  Any other option to pass, check See Also section.

## See also

[Additional
arguments](https://echarts.apache.org/en/option.html#series-chord)

## Examples

``` r
chord_data <- data.frame(
  source = c("a", "b", "c", "d", "c"),
  target = c("b", "c", "d", "e", "e"),
  value = ceiling(rnorm(5, 10, 1)),
  stringsAsFactors = FALSE
)

chord_data |>
  e_charts() |>
  e_chord(source, target, value)

{"x":{"theme":"","tl":false,"draw":true,"renderer":"canvas","events":[],"buttons":[],"settings":{"crosstalk_key":null,"crosstalk_group":null},"opts":{"series":[{"type":"chord","data":[{"name":"a"},{"name":"b"},{"name":"c"},{"name":"d"},{"name":"e"}],"links":[{"source":"a","target":"b","value":"12"},{"source":"b","target":"c","value":"10"},{"source":"c","target":"d","value":"10"},{"source":"d","target":"e","value":"10"},{"source":"c","target":"e","value":"10"}]}]},"dispose":true},"evals":[],"jsHooks":[]}
```
