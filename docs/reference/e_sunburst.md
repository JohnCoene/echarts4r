# Sunburst

Build a sunburst.

## Usage

``` r
e_sunburst(
  e,
  styles = NULL,
  names = NULL,
  levels = NULL,
  rm_x = TRUE,
  rm_y = TRUE,
  ...
)

e_sunburst_(
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

## Details

Charts e_sunburst, e_treemap and e_tree require hierarchical input data.
Such structure could be represented thru json lists or nested tibbles
(data.frame). Input data may contain styles, see `itemStyle` in examples
`jsonl` and `df` below. The number of lists in the `styles` parameter
should match the number of elements in `names` and/or `levels`. If both
`names` and `levels` are present, name styles will take precedence over
level styles. Multiple names may have the same style, see
`c('land','river')` below. Multiple levels may have the same style, see
`c(3,4)` below. `styles` lists contain items such as `color`, or
`borderColor` as specified in the [official
documentation](https://echarts.apache.org/en/option.html#series-sunburst.data.itemStyle).

## See also

[Additional
arguments](https://echarts.apache.org/en/option.html#series-sunburst)

## Examples

``` r
# json list hierarchical data representation
jsonl <- jsonlite::fromJSON('[
  {"name": "earth", "value": 30,
    "children": [
      {"name": "land", "value":10,
        "children": [
                {"name": "forest", "value": 3},
                {"name": "river", "value": 7}
        ]},
      {"name": "ocean", "value":20,
        "children": [
          {"name": "fish", "value": 10,
            "children": [
              {"name": "shark", "value":2},
              {"name": "tuna", "value":6}
            ]},
          {"name": "kelp", "value": 5}
        ]}
    ]
  },
  {"name": "mars", "value": 30,
    "children": [
      {"name": "crater", "value": 20},
      {"name": "valley", "value": 20}
    ]},
  {"name": "venus", "value": 40, "itemStyle": {"color": "blue"} }
]', simplifyDataFrame = FALSE)

jsonl |>
  e_charts() |>
  e_sunburst() # demo

{"x":{"theme":"","tl":false,"draw":true,"renderer":"canvas","events":[],"buttons":[],"settings":{"crosstalk_key":null,"crosstalk_group":null},"opts":{"series":[{"type":"sunburst","data":[{"name":"earth","value":30,"children":[{"name":"land","value":10,"children":[{"name":"forest","value":3},{"name":"river","value":7}]},{"name":"ocean","value":20,"children":[{"name":"fish","value":10,"children":[{"name":"shark","value":2},{"name":"tuna","value":6}]},{"name":"kelp","value":5}]}]},{"name":"mars","value":30,"children":[{"name":"crater","value":20},{"name":"valley","value":20}]},{"name":"venus","value":40,"itemStyle":{"color":"blue"}}]}]},"dispose":true},"evals":[],"jsHooks":[]}

# tibble hierarchical data representation
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
  e_sunburst() |>
  e_theme("westeros")

{"x":{"theme":"westeros","tl":false,"draw":true,"renderer":"canvas","events":[],"buttons":[],"settings":{"crosstalk_key":null,"crosstalk_group":null},"opts":{"series":[{"type":"sunburst","data":[{"name":"earth","value":30,"itemStyle":{},"children":[{"name":"land","value":10,"children":[{"name":"forest","value":3},{"name":"river","value":7}]},{"name":"ocean","value":20,"children":[{"name":"fish","value":10,"children":[{"name":"shark","value":2},{"name":"tuna","value":6}]},{"name":"kelp","value":5,"children":{}}]}]},{"name":"mars","value":40,"itemStyle":{"color":"red"},"children":[{"name":"crater","value":20},{"name":"valley","value":20}]},{"name":"venus","value":30,"itemStyle":{"color":"blue"},"children":{}}]}]},"dispose":true},"evals":[],"jsHooks":[]}
# with styles
myStyles <- c(list(color = "green"), list(color = "magenta")) # custom styles defined
myNames <- list(c("land", "river"), "crater") # names to style
myLevels <- list(2, c(3, 4)) # hierarchical levels to style

df |>
  e_charts() |>
  e_sunburst(myStyles, myNames, myLevels)

{"x":{"theme":"","tl":false,"draw":true,"renderer":"canvas","events":[],"buttons":[],"settings":{"crosstalk_key":null,"crosstalk_group":null},"opts":{"series":[{"type":"sunburst","data":[{"name":"earth","value":30,"itemStyle":{},"children":[{"name":"land","value":10,"children":[{"name":"forest","value":3,"itemStyle":{"color":"magenta"}},{"name":"river","value":7,"itemStyle":{"color":"green"}}],"itemStyle":{"color":"green"}},{"name":"ocean","value":20,"children":[{"name":"fish","value":10,"children":[{"name":"shark","value":2,"itemStyle":{"color":"magenta"}},{"name":"tuna","value":6,"itemStyle":{"color":"magenta"}}],"itemStyle":{"color":"magenta"}},{"name":"kelp","value":5,"children":{},"itemStyle":{"color":"magenta"}}],"itemStyle":{"color":"green"}}]},{"name":"mars","value":40,"itemStyle":{"color":"red"},"children":[{"name":"crater","value":20,"itemStyle":{"color":"magenta"}},{"name":"valley","value":20,"itemStyle":{"color":"green"}}]},{"name":"venus","value":30,"itemStyle":{"color":"blue"},"children":{}}]}]},"dispose":true},"evals":[],"jsHooks":[]}
```
