# Legend

Legend

## Usage

``` r
e_legend_select(e, name, btn = NULL)

e_legend_unselect(e, name, btn = NULL)

e_legend_toggle_select(e, name, btn = NULL)

e_legend_scroll(e, scroll_index = NULL, legend_id = NULL, btn = NULL)
```

## Arguments

- e:

  An `echarts4r` object as returned by
  [`e_charts`](https://echarts4r.john-coene.com/reference/init.md) or a
  proxy as returned by
  [`echarts4rProxy`](https://echarts4r.john-coene.com/reference/echarts4r-shiny.md).

- name:

  Legend name.

- btn:

  A [`e_button`](https://echarts4r.john-coene.com/reference/e_button.md)
  id.

- scroll_index:

  Controle the scrolling of legend when `type = "scroll"` in `e_legend`.

- legend_id:

  Id of legend.

## Examples

``` r
e <- CO2 |>
  group_by(Type) |>
  e_charts(conc) |>
  e_scatter(uptake)

e |>
  e_legend_unselect("Quebec")

{"x":{"theme":"","tl":false,"draw":true,"renderer":"canvas","events":[{"data":{"type":"legendUnSelect","name":"Quebec"}}],"buttons":[],"settings":{"crosstalk_key":null,"crosstalk_group":null},"opts":{"yAxis":[{"show":true}],"xAxis":[{"type":"value"}],"legend":{"data":["Quebec","Mississippi"]},"series":[{"data":[{"value":[95,16]},{"value":[95,13.6]},{"value":[95,16.2]},{"value":[95,14.2]},{"value":[95,9.300000000000001]},{"value":[95,15.1]},{"value":[175,30.4]},{"value":[175,27.3]},{"value":[175,32.4]},{"value":[175,24.1]},{"value":[175,27.3]},{"value":[175,21]},{"value":[250,34.8]},{"value":[250,37.1]},{"value":[250,40.3]},{"value":[250,30.3]},{"value":[250,35]},{"value":[250,38.1]},{"value":[350,37.2]},{"value":[350,41.8]},{"value":[350,42.1]},{"value":[350,34.6]},{"value":[350,38.8]},{"value":[350,34]},{"value":[500,35.3]},{"value":[500,40.6]},{"value":[500,42.9]},{"value":[500,32.5]},{"value":[500,38.6]},{"value":[500,38.9]},{"value":[675,39.2]},{"value":[675,41.4]},{"value":[675,43.9]},{"value":[675,35.4]},{"value":[675,37.5]},{"value":[675,39.6]},{"value":[1000,39.7]},{"value":[1000,44.3]},{"value":[1000,45.5]},{"value":[1000,38.7]},{"value":[1000,42.4]},{"value":[1000,41.4]}],"name":"Quebec","type":"scatter","symbol":null,"coordinateSystem":"cartesian2d","yAxisIndex":0,"xAxisIndex":0,"symbolSize":3},{"data":[{"value":[95,10.6]},{"value":[95,12]},{"value":[95,11.3]},{"value":[95,10.5]},{"value":[95,7.7]},{"value":[95,10.6]},{"value":[175,19.2]},{"value":[175,22]},{"value":[175,19.4]},{"value":[175,14.9]},{"value":[175,11.4]},{"value":[175,18]},{"value":[250,26.2]},{"value":[250,30.6]},{"value":[250,25.8]},{"value":[250,18.1]},{"value":[250,12.3]},{"value":[250,17.9]},{"value":[350,30]},{"value":[350,31.8]},{"value":[350,27.9]},{"value":[350,18.9]},{"value":[350,13]},{"value":[350,17.9]},{"value":[500,30.9]},{"value":[500,32.4]},{"value":[500,28.5]},{"value":[500,19.5]},{"value":[500,12.5]},{"value":[500,17.9]},{"value":[675,32.4]},{"value":[675,31.1]},{"value":[675,28.1]},{"value":[675,22.2]},{"value":[675,13.7]},{"value":[675,18.9]},{"value":[1000,35.5]},{"value":[1000,31.5]},{"value":[1000,27.8]},{"value":[1000,21.9]},{"value":[1000,14.4]},{"value":[1000,19.9]}],"name":"Mississippi","type":"scatter","symbol":null,"coordinateSystem":"cartesian2d","yAxisIndex":0,"xAxisIndex":0,"symbolSize":3}]},"dispose":true},"evals":[],"jsHooks":[]}
e |>
  e_legend_unselect("Quebec", btn = "btn") |>
  e_button("btn", "Quebec")
Quebec

{"x":{"theme":"","tl":false,"draw":true,"renderer":"canvas","events":[],"buttons":{"btn":[{"id":"btn","data":{"type":"legendUnSelect","name":"Quebec"}}]},"settings":{"crosstalk_key":null,"crosstalk_group":null},"opts":{"yAxis":[{"show":true}],"xAxis":[{"type":"value"}],"legend":{"data":["Quebec","Mississippi"]},"series":[{"data":[{"value":[95,16]},{"value":[95,13.6]},{"value":[95,16.2]},{"value":[95,14.2]},{"value":[95,9.300000000000001]},{"value":[95,15.1]},{"value":[175,30.4]},{"value":[175,27.3]},{"value":[175,32.4]},{"value":[175,24.1]},{"value":[175,27.3]},{"value":[175,21]},{"value":[250,34.8]},{"value":[250,37.1]},{"value":[250,40.3]},{"value":[250,30.3]},{"value":[250,35]},{"value":[250,38.1]},{"value":[350,37.2]},{"value":[350,41.8]},{"value":[350,42.1]},{"value":[350,34.6]},{"value":[350,38.8]},{"value":[350,34]},{"value":[500,35.3]},{"value":[500,40.6]},{"value":[500,42.9]},{"value":[500,32.5]},{"value":[500,38.6]},{"value":[500,38.9]},{"value":[675,39.2]},{"value":[675,41.4]},{"value":[675,43.9]},{"value":[675,35.4]},{"value":[675,37.5]},{"value":[675,39.6]},{"value":[1000,39.7]},{"value":[1000,44.3]},{"value":[1000,45.5]},{"value":[1000,38.7]},{"value":[1000,42.4]},{"value":[1000,41.4]}],"name":"Quebec","type":"scatter","symbol":null,"coordinateSystem":"cartesian2d","yAxisIndex":0,"xAxisIndex":0,"symbolSize":3},{"data":[{"value":[95,10.6]},{"value":[95,12]},{"value":[95,11.3]},{"value":[95,10.5]},{"value":[95,7.7]},{"value":[95,10.6]},{"value":[175,19.2]},{"value":[175,22]},{"value":[175,19.4]},{"value":[175,14.9]},{"value":[175,11.4]},{"value":[175,18]},{"value":[250,26.2]},{"value":[250,30.6]},{"value":[250,25.8]},{"value":[250,18.1]},{"value":[250,12.3]},{"value":[250,17.9]},{"value":[350,30]},{"value":[350,31.8]},{"value":[350,27.9]},{"value":[350,18.9]},{"value":[350,13]},{"value":[350,17.9]},{"value":[500,30.9]},{"value":[500,32.4]},{"value":[500,28.5]},{"value":[500,19.5]},{"value":[500,12.5]},{"value":[500,17.9]},{"value":[675,32.4]},{"value":[675,31.1]},{"value":[675,28.1]},{"value":[675,22.2]},{"value":[675,13.7]},{"value":[675,18.9]},{"value":[1000,35.5]},{"value":[1000,31.5]},{"value":[1000,27.8]},{"value":[1000,21.9]},{"value":[1000,14.4]},{"value":[1000,19.9]}],"name":"Mississippi","type":"scatter","symbol":null,"coordinateSystem":"cartesian2d","yAxisIndex":0,"xAxisIndex":0,"symbolSize":3}]},"dispose":true},"evals":[],"jsHooks":[]}
```
