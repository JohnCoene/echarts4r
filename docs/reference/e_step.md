# Step

Add step serie.

## Usage

``` r
e_step(
  e,
  serie,
  bind,
  step = c("start", "middle", "end"),
  fill = FALSE,
  name = NULL,
  legend = TRUE,
  y_index = 0,
  x_index = 0,
  coord_system = "cartesian2d",
  ...
)

e_step_(
  e,
  serie,
  bind = NULL,
  step = c("start", "middle", "end"),
  fill = FALSE,
  name = NULL,
  legend = TRUE,
  y_index = 0,
  x_index = 0,
  coord_system = "cartesian2d",
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

- bind:

  Binding between datasets, namely for use of
  [`e_brush`](https://echarts4r.john-coene.com/reference/e_brush.md).

- step:

  Step type, one of `start`, `middle` or `end`.

- fill:

  Set to fill as area.

- name:

  name of the serie.

- legend:

  Whether to add serie to legend.

- x_index, y_index:

  Indexes of x and y axis.

- coord_system:

  Coordinate system to plot against.

- ...:

  Any other option to pass, check See Also section.

## See also

[Additional
arguments](https://echarts.apache.org/en/option.html#series-line)

## Examples

``` r
USArrests |>
  tibble::rownames_to_column("State") |>
  e_charts(State) |>
  e_step(Murder, name = "Start", step = "start", fill = TRUE) |>
  e_step(Rape, name = "Middle", step = "middle") |>
  e_step(Assault, name = "End", step = "end") |>
  e_tooltip(trigger = "axis")

{"x":{"theme":"","tl":false,"draw":true,"renderer":"canvas","events":[],"buttons":[],"settings":{"crosstalk_key":null,"crosstalk_group":null},"opts":{"yAxis":[{"show":true}],"xAxis":[{"data":["Alabama","Alaska","Arizona","Arkansas","California","Colorado","Connecticut","Delaware","Florida","Georgia","Hawaii","Idaho","Illinois","Indiana","Iowa","Kansas","Kentucky","Louisiana","Maine","Maryland","Massachusetts","Michigan","Minnesota","Mississippi","Missouri","Montana","Nebraska","Nevada","New Hampshire","New Jersey","New Mexico","New York","North Carolina","North Dakota","Ohio","Oklahoma","Oregon","Pennsylvania","Rhode Island","South Carolina","South Dakota","Tennessee","Texas","Utah","Vermont","Virginia","Washington","West Virginia","Wisconsin","Wyoming"],"type":"category","boundaryGap":true}],"legend":{"data":["Start","Middle","End"]},"series":[{"data":[{"value":["Alabama","13.2"]},{"value":["Alaska","10.0"]},{"value":["Arizona"," 8.1"]},{"value":["Arkansas"," 8.8"]},{"value":["California"," 9.0"]},{"value":["Colorado"," 7.9"]},{"value":["Connecticut"," 3.3"]},{"value":["Delaware"," 5.9"]},{"value":["Florida","15.4"]},{"value":["Georgia","17.4"]},{"value":["Hawaii"," 5.3"]},{"value":["Idaho"," 2.6"]},{"value":["Illinois","10.4"]},{"value":["Indiana"," 7.2"]},{"value":["Iowa"," 2.2"]},{"value":["Kansas"," 6.0"]},{"value":["Kentucky"," 9.7"]},{"value":["Louisiana","15.4"]},{"value":["Maine"," 2.1"]},{"value":["Maryland","11.3"]},{"value":["Massachusetts"," 4.4"]},{"value":["Michigan","12.1"]},{"value":["Minnesota"," 2.7"]},{"value":["Mississippi","16.1"]},{"value":["Missouri"," 9.0"]},{"value":["Montana"," 6.0"]},{"value":["Nebraska"," 4.3"]},{"value":["Nevada","12.2"]},{"value":["New Hampshire"," 2.1"]},{"value":["New Jersey"," 7.4"]},{"value":["New Mexico","11.4"]},{"value":["New York","11.1"]},{"value":["North Carolina","13.0"]},{"value":["North Dakota"," 0.8"]},{"value":["Ohio"," 7.3"]},{"value":["Oklahoma"," 6.6"]},{"value":["Oregon"," 4.9"]},{"value":["Pennsylvania"," 6.3"]},{"value":["Rhode Island"," 3.4"]},{"value":["South Carolina","14.4"]},{"value":["South Dakota"," 3.8"]},{"value":["Tennessee","13.2"]},{"value":["Texas","12.7"]},{"value":["Utah"," 3.2"]},{"value":["Vermont"," 2.2"]},{"value":["Virginia"," 8.5"]},{"value":["Washington"," 4.0"]},{"value":["West Virginia"," 5.7"]},{"value":["Wisconsin"," 2.6"]},{"value":["Wyoming"," 6.8"]}],"yAxisIndex":0,"xAxisIndex":0,"name":"Start","type":"line","coordinateSystem":"cartesian2d","step":"start"},{"data":[{"value":["Alabama","21.2"]},{"value":["Alaska","44.5"]},{"value":["Arizona","31.0"]},{"value":["Arkansas","19.5"]},{"value":["California","40.6"]},{"value":["Colorado","38.7"]},{"value":["Connecticut","11.1"]},{"value":["Delaware","15.8"]},{"value":["Florida","31.9"]},{"value":["Georgia","25.8"]},{"value":["Hawaii","20.2"]},{"value":["Idaho","14.2"]},{"value":["Illinois","24.0"]},{"value":["Indiana","21.0"]},{"value":["Iowa","11.3"]},{"value":["Kansas","18.0"]},{"value":["Kentucky","16.3"]},{"value":["Louisiana","22.2"]},{"value":["Maine"," 7.8"]},{"value":["Maryland","27.8"]},{"value":["Massachusetts","16.3"]},{"value":["Michigan","35.1"]},{"value":["Minnesota","14.9"]},{"value":["Mississippi","17.1"]},{"value":["Missouri","28.2"]},{"value":["Montana","16.4"]},{"value":["Nebraska","16.5"]},{"value":["Nevada","46.0"]},{"value":["New Hampshire"," 9.5"]},{"value":["New Jersey","18.8"]},{"value":["New Mexico","32.1"]},{"value":["New York","26.1"]},{"value":["North Carolina","16.1"]},{"value":["North Dakota"," 7.3"]},{"value":["Ohio","21.4"]},{"value":["Oklahoma","20.0"]},{"value":["Oregon","29.3"]},{"value":["Pennsylvania","14.9"]},{"value":["Rhode Island"," 8.3"]},{"value":["South Carolina","22.5"]},{"value":["South Dakota","12.8"]},{"value":["Tennessee","26.9"]},{"value":["Texas","25.5"]},{"value":["Utah","22.9"]},{"value":["Vermont","11.2"]},{"value":["Virginia","20.7"]},{"value":["Washington","26.2"]},{"value":["West Virginia"," 9.3"]},{"value":["Wisconsin","10.8"]},{"value":["Wyoming","15.6"]}],"yAxisIndex":0,"xAxisIndex":0,"name":"Middle","type":"line","coordinateSystem":"cartesian2d","step":"middle"},{"data":[{"value":["Alabama","236"]},{"value":["Alaska","263"]},{"value":["Arizona","294"]},{"value":["Arkansas","190"]},{"value":["California","276"]},{"value":["Colorado","204"]},{"value":["Connecticut","110"]},{"value":["Delaware","238"]},{"value":["Florida","335"]},{"value":["Georgia","211"]},{"value":["Hawaii"," 46"]},{"value":["Idaho","120"]},{"value":["Illinois","249"]},{"value":["Indiana","113"]},{"value":["Iowa"," 56"]},{"value":["Kansas","115"]},{"value":["Kentucky","109"]},{"value":["Louisiana","249"]},{"value":["Maine"," 83"]},{"value":["Maryland","300"]},{"value":["Massachusetts","149"]},{"value":["Michigan","255"]},{"value":["Minnesota"," 72"]},{"value":["Mississippi","259"]},{"value":["Missouri","178"]},{"value":["Montana","109"]},{"value":["Nebraska","102"]},{"value":["Nevada","252"]},{"value":["New Hampshire"," 57"]},{"value":["New Jersey","159"]},{"value":["New Mexico","285"]},{"value":["New York","254"]},{"value":["North Carolina","337"]},{"value":["North Dakota"," 45"]},{"value":["Ohio","120"]},{"value":["Oklahoma","151"]},{"value":["Oregon","159"]},{"value":["Pennsylvania","106"]},{"value":["Rhode Island","174"]},{"value":["South Carolina","279"]},{"value":["South Dakota"," 86"]},{"value":["Tennessee","188"]},{"value":["Texas","201"]},{"value":["Utah","120"]},{"value":["Vermont"," 48"]},{"value":["Virginia","156"]},{"value":["Washington","145"]},{"value":["West Virginia"," 81"]},{"value":["Wisconsin"," 53"]},{"value":["Wyoming","161"]}],"yAxisIndex":0,"xAxisIndex":0,"name":"End","type":"line","coordinateSystem":"cartesian2d","step":"end"}],"tooltip":{"trigger":"axis"}},"dispose":true},"evals":[],"jsHooks":[]}
# timeline
iris |>
  group_by(Species) |>
  e_charts(Sepal.Length, timeline = TRUE) |>
  e_step(Sepal.Width) |>
  e_tooltip(trigger = "axis")

{"x":{"theme":"","tl":true,"draw":true,"renderer":"canvas","events":[],"buttons":[],"settings":{"crosstalk_key":null,"crosstalk_group":null},"opts":{"baseOption":{"yAxis":[{"show":true}],"timeline":{"data":["setosa","versicolor","virginica"],"axisType":"category"},"xAxis":[{"type":"value"}],"legend":{"data":["Sepal.Width"]},"series":[{"name":"Sepal.Width","type":"line","yAxisIndex":0,"xAxisIndex":0,"coordinateSystem":"cartesian2d","areaStyle":[],"step":"start"}],"tooltip":{"trigger":"axis"}},"options":[{"series":[{"data":[{"value":[4.3,3]},{"value":[4.4,2.9]},{"value":[4.4,3]},{"value":[4.4,3.2]},{"value":[4.5,2.3]},{"value":[4.6,3.1]},{"value":[4.6,3.4]},{"value":[4.6,3.6]},{"value":[4.6,3.2]},{"value":[4.7,3.2]},{"value":[4.7,3.2]},{"value":[4.8,3.4]},{"value":[4.8,3]},{"value":[4.8,3.4]},{"value":[4.8,3.1]},{"value":[4.8,3]},{"value":[4.9,3]},{"value":[4.9,3.1]},{"value":[4.9,3.1]},{"value":[4.9,3.6]},{"value":[5,3.6]},{"value":[5,3.4]},{"value":[5,3]},{"value":[5,3.4]},{"value":[5,3.2]},{"value":[5,3.5]},{"value":[5,3.5]},{"value":[5,3.3]},{"value":[5.1,3.5]},{"value":[5.1,3.5]},{"value":[5.1,3.8]},{"value":[5.1,3.7]},{"value":[5.1,3.3]},{"value":[5.1,3.4]},{"value":[5.1,3.8]},{"value":[5.1,3.8]},{"value":[5.2,3.5]},{"value":[5.2,3.4]},{"value":[5.2,4.1]},{"value":[5.3,3.7]},{"value":[5.4,3.9]},{"value":[5.4,3.7]},{"value":[5.4,3.9]},{"value":[5.4,3.4]},{"value":[5.4,3.4]},{"value":[5.5,4.2]},{"value":[5.5,3.5]},{"value":[5.7,4.4]},{"value":[5.7,3.8]},{"value":[5.8,4]}],"yAxisIndex":0,"xAxisIndex":0}]},{"series":[{"data":[{"value":[4.9,2.4]},{"value":[5,2]},{"value":[5,2.3]},{"value":[5.1,2.5]},{"value":[5.2,2.7]},{"value":[5.4,3]},{"value":[5.5,2.3]},{"value":[5.5,2.4]},{"value":[5.5,2.4]},{"value":[5.5,2.5]},{"value":[5.5,2.6]},{"value":[5.6,2.9]},{"value":[5.6,3]},{"value":[5.6,2.5]},{"value":[5.6,3]},{"value":[5.6,2.7]},{"value":[5.7,2.8]},{"value":[5.7,2.6]},{"value":[5.7,3]},{"value":[5.7,2.9]},{"value":[5.7,2.8]},{"value":[5.8,2.7]},{"value":[5.8,2.7]},{"value":[5.8,2.6]},{"value":[5.9,3]},{"value":[5.9,3.2]},{"value":[6,2.2]},{"value":[6,2.9]},{"value":[6,2.7]},{"value":[6,3.4]},{"value":[6.1,2.9]},{"value":[6.1,2.8]},{"value":[6.1,2.8]},{"value":[6.1,3]},{"value":[6.2,2.2]},{"value":[6.2,2.9]},{"value":[6.3,3.3]},{"value":[6.3,2.5]},{"value":[6.3,2.3]},{"value":[6.4,3.2]},{"value":[6.4,2.9]},{"value":[6.5,2.8]},{"value":[6.6,2.9]},{"value":[6.6,3]},{"value":[6.7,3.1]},{"value":[6.7,3]},{"value":[6.7,3.1]},{"value":[6.8,2.8]},{"value":[6.9,3.1]},{"value":[7,3.2]}],"yAxisIndex":0,"xAxisIndex":0}]},{"series":[{"data":[{"value":[4.9,2.5]},{"value":[5.6,2.8]},{"value":[5.7,2.5]},{"value":[5.8,2.7]},{"value":[5.8,2.8]},{"value":[5.8,2.7]},{"value":[5.9,3]},{"value":[6,2.2]},{"value":[6,3]},{"value":[6.1,3]},{"value":[6.1,2.6]},{"value":[6.2,2.8]},{"value":[6.2,3.4]},{"value":[6.3,3.3]},{"value":[6.3,2.9]},{"value":[6.3,2.7]},{"value":[6.3,2.8]},{"value":[6.3,3.4]},{"value":[6.3,2.5]},{"value":[6.4,2.7]},{"value":[6.4,3.2]},{"value":[6.4,2.8]},{"value":[6.4,2.8]},{"value":[6.4,3.1]},{"value":[6.5,3]},{"value":[6.5,3.2]},{"value":[6.5,3]},{"value":[6.5,3]},{"value":[6.7,2.5]},{"value":[6.7,3.3]},{"value":[6.7,3.1]},{"value":[6.7,3.3]},{"value":[6.7,3]},{"value":[6.8,3]},{"value":[6.8,3.2]},{"value":[6.9,3.2]},{"value":[6.9,3.1]},{"value":[6.9,3.1]},{"value":[7.1,3]},{"value":[7.2,3.6]},{"value":[7.2,3.2]},{"value":[7.2,3]},{"value":[7.3,2.9]},{"value":[7.4,2.8]},{"value":[7.6,3]},{"value":[7.7,3.8]},{"value":[7.7,2.6]},{"value":[7.7,2.8]},{"value":[7.7,3]},{"value":[7.9,3.8]}],"yAxisIndex":0,"xAxisIndex":0}]}]},"dispose":true},"evals":[],"jsHooks":[]}
```
