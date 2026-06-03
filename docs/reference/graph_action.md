# Nodes Adjacency

Actions related to
[`e_graph`](https://echarts4r.john-coene.com/reference/graph.md).

## Usage

``` r
e_focus_adjacency(e, ..., btn = NULL)

e_unfocus_adjacency(e, ..., btn = NULL)
```

## Arguments

- e:

  An `echarts4r` object as returned by
  [`e_charts`](https://echarts4r.john-coene.com/reference/init.md) or a
  proxy as returned by
  [`echarts4rProxy`](https://echarts4r.john-coene.com/reference/echarts4r-shiny.md).

- ...:

  Any options, see [official
  documentation](https://echarts.apache.org/en/api.html#action.graph)

- btn:

  A [`e_button`](https://echarts4r.john-coene.com/reference/e_button.md)
  id.

## Examples

``` r
value <- rnorm(10, 10, 2)

nodes <- data.frame(
  name = sample(LETTERS, 10),
  value = value,
  size = value,
  grp = rep(c("grp1", "grp2"), 5),
  stringsAsFactors = FALSE
)

edges <- data.frame(
  source = sample(nodes$name, 20, replace = TRUE),
  target = sample(nodes$name, 20, replace = TRUE),
  stringsAsFactors = FALSE
)

e_charts() |>
  e_graph() |>
  e_graph_nodes(nodes, name, value, size, grp) |>
  e_graph_edges(edges, source, target) |>
  e_focus_adjacency(
    seriesIndex = 0,
    dataIndex = 4
  )

{"x":{"theme":"","tl":false,"draw":true,"renderer":"canvas","events":[{"data":{"type":"focusNodeAdjacency","seriesIndex":0,"dataIndex":4}}],"buttons":[],"settings":{"crosstalk_key":null,"crosstalk_group":null},"opts":{"series":[{"name":null,"type":"graph","layout":"force","categories":[{"name":"grp1"},{"name":"grp2"}],"data":[{"name":"W","value":"13.150551","symbolSize":"13.150551","category":"grp1"},{"name":"C","value":"10.676011","symbolSize":"10.676011","category":"grp2"},{"name":"S","value":" 9.454903","symbolSize":" 9.454903","category":"grp1"},{"name":"P","value":" 9.520610","symbolSize":" 9.520610","category":"grp2"},{"name":"F","value":"12.796634","symbolSize":"12.796634","category":"grp1"},{"name":"I","value":"11.168639","symbolSize":"11.168639","category":"grp2"},{"name":"K","value":" 8.110658","symbolSize":" 8.110658","category":"grp1"},{"name":"O","value":"11.619870","symbolSize":"11.619870","category":"grp2"},{"name":"D","value":" 9.451658","symbolSize":" 9.451658","category":"grp1"},{"name":"L","value":" 8.447189","symbolSize":" 8.447189","category":"grp2"}],"links":[{"source":"S","target":"C"},{"source":"K","target":"O"},{"source":"O","target":"K"},{"source":"L","target":"O"},{"source":"I","target":"P"},{"source":"K","target":"S"},{"source":"D","target":"K"},{"source":"L","target":"I"},{"source":"P","target":"L"},{"source":"P","target":"D"},{"source":"F","target":"K"},{"source":"I","target":"D"},{"source":"K","target":"S"},{"source":"P","target":"D"},{"source":"F","target":"O"},{"source":"O","target":"F"},{"source":"C","target":"P"},{"source":"K","target":"L"},{"source":"S","target":"D"},{"source":"S","target":"I"}]}],"legend":{"data":["grp1","grp2"]}},"dispose":true},"evals":[],"jsHooks":[]}
```
