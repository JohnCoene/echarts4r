# Pie

Draw pie and donut charts.

## Usage

``` r
e_pie(
  e,
  serie,
  name = NULL,
  legend = TRUE,
  coord_system = "",
  rm_x = TRUE,
  rm_y = TRUE,
  ...
)

e_pie_(
  e,
  serie,
  name = NULL,
  legend = TRUE,
  coord_system = "",
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

- serie:

  Column name of serie to plot.

- name:

  name of the serie.

- legend:

  Whether to add serie to legend.

- coord_system:

  Coordinate system to plot against.

- rm_x, rm_y:

  Whether to remove x and y axis, defaults to `TRUE`.

- ...:

  Any other option to pass, check See Also section.

## See also

[Additional
arguments](https://echarts.apache.org/en/option.html#series-pie)

## Examples

``` r
mtcars |>
  head() |>
  tibble::rownames_to_column("model") |>
  e_charts(model) |>
  e_pie(carb)

{"x":{"theme":"","tl":false,"draw":true,"renderer":"canvas","events":[],"buttons":[],"settings":{"crosstalk_key":null,"crosstalk_group":null},"opts":{"legend":{"data":["Mazda RX4","Mazda RX4 Wag","Datsun 710","Hornet 4 Drive","Hornet Sportabout","Valiant"]},"series":[{"name":"carb","type":"pie","data":[{"value":4,"name":"Mazda RX4"},{"value":4,"name":"Mazda RX4 Wag"},{"value":1,"name":"Datsun 710"},{"value":1,"name":"Hornet 4 Drive"},{"value":2,"name":"Hornet Sportabout"},{"value":1,"name":"Valiant"}]}]},"dispose":true},"evals":[],"jsHooks":[]}
# timeline
df <- data.frame(
  grp = c("A", "A", "A", "B", "B", "B"),
  labels = rep(LETTERS[1:3], 2),
  values = runif(6, 1, 5)
)

df |>
  group_by(grp) |>
  e_charts(labels, timeline = TRUE) |>
  e_pie(values)

{"x":{"theme":"","tl":true,"draw":true,"renderer":"canvas","events":[],"buttons":[],"settings":{"crosstalk_key":null,"crosstalk_group":null},"opts":{"baseOption":{"timeline":{"data":["A","B"],"axisType":"category"},"series":[{"name":null,"type":"pie"}]},"options":[{"series":[{"data":[{"value":4.505369887687266,"name":"A"},{"value":2.907830149866641,"name":"B"},{"value":2.280707543715835,"name":"C"}]}]},{"series":[{"data":[{"value":4.744582180865109,"name":"A"},{"value":1.021226427517831,"name":"B"},{"value":1.269911715760827,"name":"C"}]}]}]},"dispose":true},"evals":[],"jsHooks":[]}
```
