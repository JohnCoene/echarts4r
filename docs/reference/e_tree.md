# Tree

Build a tree.

## Usage

``` r
e_tree(e, rm_x = TRUE, rm_y = TRUE, ...)

e_tree_(e, rm_x = TRUE, rm_y = TRUE, ...)
```

## Arguments

- e:

  An `echarts4r` object as returned by
  [`e_charts`](https://echarts4r.john-coene.com/reference/init.md) or a
  proxy as returned by
  [`echarts4rProxy`](https://echarts4r.john-coene.com/reference/echarts4r-shiny.md).

- rm_x, rm_y:

  Whether to remove x and y axis, defaults to `TRUE`.

- ...:

  Any other option to pass, check See Also section.

## See also

[Additional
arguments](https://echarts.apache.org/en/option.html#series-tree)

## Examples

``` r
library(dplyr)
df <- tibble(
  name = "earth",
  # 1st level
  children = list(
    tibble(
      name = c("land", "ocean"),
      # 2nd level
      children = list(
        tibble(name = c("forest", "river")),
        # 3rd level
        tibble(
          name = c("fish", "kelp"),
          children = list(
            tibble(
              name = c("shark", "tuna")
            ),
            # 4th level
            NULL # kelp
          )
        )
      )
    )
  )
)

df |>
  e_charts() |>
  e_tree(initialTreeDepth = 3, label = list(offset = c(0, -11)))

{"x":{"theme":"","tl":false,"draw":true,"renderer":"canvas","events":[],"buttons":[],"settings":{"crosstalk_key":null,"crosstalk_group":null},"opts":{"series":[{"type":"tree","data":[{"name":"earth","children":[{"name":"land","children":[{"name":"forest"},{"name":"river"}]},{"name":"ocean","children":[{"name":"fish","children":[{"name":"shark"},{"name":"tuna"}]},{"name":"kelp","children":{}}]}]}],"initialTreeDepth":3,"label":{"offset":[0,-11]}}]},"dispose":true},"evals":[],"jsHooks":[]}
```
