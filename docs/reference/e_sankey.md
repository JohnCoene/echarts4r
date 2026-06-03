# Sankey

Draw a sankey diagram.

## Usage

``` r
e_sankey(
  e,
  source,
  target,
  value,
  layout = "none",
  rm_x = TRUE,
  rm_y = TRUE,
  ...
)

e_sankey_(
  e,
  source,
  target,
  value,
  layout = "none",
  rm_x = TRUE,
  rm_y = TRUE,
  ...
)
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

  Value change from `source` to `target`.

- layout:

  Layout of sankey.

- rm_x, rm_y:

  Whether to remove the x and y axis, defaults to `TRUE`.

- ...:

  Any other option to pass, check See Also section.

## See also

[Additional
arguments](https://echarts.apache.org/en/option.html#series-sankey)

## Examples

``` r
sankey <- data.frame(
  source = c("a", "b", "c", "d", "c"),
  target = c("b", "c", "d", "e", "e"),
  value = ceiling(rnorm(5, 10, 1)),
  stringsAsFactors = FALSE
)

sankey |>
  e_charts() |>
  e_sankey(source, target, value)

{"x":{"theme":"","tl":false,"draw":true,"renderer":"canvas","events":[],"buttons":[],"settings":{"crosstalk_key":null,"crosstalk_group":null},"opts":{"series":[{"type":"sankey","orient":"none","data":[{"name":"a"},{"name":"b"},{"name":"c"},{"name":"d"},{"name":"e"}],"links":[{"source":"a","target":"b","value":" 9"},{"source":"b","target":"c","value":"11"},{"source":"c","target":"d","value":"10"},{"source":"d","target":"e","value":"10"},{"source":"c","target":"e","value":"12"}]}]},"dispose":true},"evals":[],"jsHooks":[]}
```
