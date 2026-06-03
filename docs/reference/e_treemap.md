# Treemap

Build a treemap.

## Usage

``` r
e_treemap(
  e,
  styles = NULL,
  names = NULL,
  levels = NULL,
  rm_x = TRUE,
  rm_y = TRUE,
  ...
)

e_treemap_(
  e,
  styles = NULL,
  names = NULL,
  levels = NULL,
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

- styles:

  Vector of style lists, defaults to `NULL`.

- names:

  Names of items to style, expects a `list`, defaults to `NULL`.

- levels:

  Hierarchical levels to style, expects a `list`, defaults to `NULL`.

- rm_x, rm_y:

  Whether to remove x and y axis, defaults to `TRUE`.

- ...:

  Any other option to pass, check See Also section.

## See also

[Additional
arguments](https://echarts.apache.org/en/option.html#series-treemap)

## Examples

``` r
library(dplyr)
df <- tibble(
  name = c("earth", "mars", "venus"),
  value = c(30, 40, 30),
  # 1st level
  itemStyle = tibble(color = c(NA, "red", "blue")),
  # embedded styles, optional
  children = list(
    tibble(
      name = c("land", "ocean"),
      value = c(10, 20),
      # 2nd level
      children = list(
        tibble(name = c("forest", "river"), value = c(3, 7)),
        # 3rd level
        tibble(
          name = c("fish", "kelp"),
          value = c(10, 5),
          children = list(
            tibble(name = c("shark", "tuna"), value = c(2, 6)),
            # 4th level
            NULL # kelp
          )
        )
      )
    ),
    tibble(name = c("crater", "valley"), value = c(20, 20)),
    NULL # venus
  )
)

df |>
  e_charts() |>
  e_treemap()

{"x":{"theme":"","tl":false,"draw":true,"renderer":"canvas","events":[],"buttons":[],"settings":{"crosstalk_key":null,"crosstalk_group":null},"opts":{"series":[{"type":"treemap","data":[{"name":"earth","value":30,"itemStyle":{},"children":[{"name":"land","value":10,"children":[{"name":"forest","value":3},{"name":"river","value":7}]},{"name":"ocean","value":20,"children":[{"name":"fish","value":10,"children":[{"name":"shark","value":2},{"name":"tuna","value":6}]},{"name":"kelp","value":5,"children":{}}]}]},{"name":"mars","value":40,"itemStyle":{"color":"red"},"children":[{"name":"crater","value":20},{"name":"valley","value":20}]},{"name":"venus","value":30,"itemStyle":{"color":"blue"},"children":{}}]}]},"dispose":true},"evals":[],"jsHooks":[]}
```
