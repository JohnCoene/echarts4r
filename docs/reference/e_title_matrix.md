# Generate Chart Titles for Matrix

helper function for creating titles for every plot in a geofacet style
matrix. This generates the title using the name of the series.

## Usage

``` r
e_title_matrix(e, ...)
```

## Arguments

- e:

  An `echarts4r` object as returned by
  [`e_charts`](https://echarts4r.john-coene.com/reference/init.md) or a
  proxy as returned by
  [`echarts4rProxy`](https://echarts4r.john-coene.com/reference/echarts4r-shiny.md).

- ...:

  Any other option to pass, check See Also section.

## See also

[Additional arguments](https://echarts.apache.org/en/option.html#title)

## Examples

``` r
df <- data.frame(group = rep(letters[1:6], each = 20),
date = seq(from = as.Date("2025-01-01"),
           to = as.Date("2025-01-20"), by = "day"),
temp = sample(c(10:20), size = 60, replace = TRUE))
grid <- data.frame(name = unique(df$group), row = c(1:6), col = c(1:6))

df |>
  group_by(group) |>
  e_chart(date) |>
  e_line(temp, symbol = "none") |>
  e_x_axis(splitNumber = 2) |>
  e_y_axis(splitNumber = 2) |>
  e_geoFacet(legend = FALSE,
             grid = grid,
             margin_trbl = c("t"="25%"),
             left = "5%",
             width = "90%") |>
  e_title(text = "Group Temps") |>
  e_title_matrix(textStyle = list(fontSize = 10),
                 left = "center", top = "top")

{"x":{"theme":"","tl":false,"draw":true,"renderer":"canvas","events":[],"buttons":[],"settings":{"crosstalk_key":null,"crosstalk_group":null},"opts":{"matrix":{"x":{"data":[null,null,null,null,null,null],"show":false},"y":{"data":[null,null,null,null,null,null],"show":false},"left":"5%","width":"90%"},"yAxis":[{"show":true,"splitNumber":2,"id":"chart1","gridId":"chart1"},{"show":true,"splitNumber":2,"id":"chart2","gridId":"chart2"},{"show":true,"splitNumber":2,"id":"chart3","gridId":"chart3"},{"show":true,"splitNumber":2,"id":"chart4","gridId":"chart4"},{"show":true,"splitNumber":2,"id":"chart5","gridId":"chart5"},{"show":true,"splitNumber":2,"id":"chart6","gridId":"chart6"}],"xAxis":[{"data":["2025-01-01","2025-01-02","2025-01-03","2025-01-04","2025-01-05","2025-01-06","2025-01-07","2025-01-08","2025-01-09","2025-01-10","2025-01-11","2025-01-12","2025-01-13","2025-01-14","2025-01-15","2025-01-16","2025-01-17","2025-01-18","2025-01-19","2025-01-20"],"type":"time","boundaryGap":true,"splitNumber":2,"id":"chart1","gridId":"chart1"},{"data":["2025-01-01","2025-01-02","2025-01-03","2025-01-04","2025-01-05","2025-01-06","2025-01-07","2025-01-08","2025-01-09","2025-01-10","2025-01-11","2025-01-12","2025-01-13","2025-01-14","2025-01-15","2025-01-16","2025-01-17","2025-01-18","2025-01-19","2025-01-20"],"type":"time","boundaryGap":true,"splitNumber":2,"id":"chart2","gridId":"chart2"},{"data":["2025-01-01","2025-01-02","2025-01-03","2025-01-04","2025-01-05","2025-01-06","2025-01-07","2025-01-08","2025-01-09","2025-01-10","2025-01-11","2025-01-12","2025-01-13","2025-01-14","2025-01-15","2025-01-16","2025-01-17","2025-01-18","2025-01-19","2025-01-20"],"type":"time","boundaryGap":true,"splitNumber":2,"id":"chart3","gridId":"chart3"},{"data":["2025-01-01","2025-01-02","2025-01-03","2025-01-04","2025-01-05","2025-01-06","2025-01-07","2025-01-08","2025-01-09","2025-01-10","2025-01-11","2025-01-12","2025-01-13","2025-01-14","2025-01-15","2025-01-16","2025-01-17","2025-01-18","2025-01-19","2025-01-20"],"type":"time","boundaryGap":true,"splitNumber":2,"id":"chart4","gridId":"chart4"},{"data":["2025-01-01","2025-01-02","2025-01-03","2025-01-04","2025-01-05","2025-01-06","2025-01-07","2025-01-08","2025-01-09","2025-01-10","2025-01-11","2025-01-12","2025-01-13","2025-01-14","2025-01-15","2025-01-16","2025-01-17","2025-01-18","2025-01-19","2025-01-20"],"type":"time","boundaryGap":true,"splitNumber":2,"id":"chart5","gridId":"chart5"},{"data":["2025-01-01","2025-01-02","2025-01-03","2025-01-04","2025-01-05","2025-01-06","2025-01-07","2025-01-08","2025-01-09","2025-01-10","2025-01-11","2025-01-12","2025-01-13","2025-01-14","2025-01-15","2025-01-16","2025-01-17","2025-01-18","2025-01-19","2025-01-20"],"type":"time","boundaryGap":true,"splitNumber":2,"id":"chart6","gridId":"chart6"}],"series":[{"data":[{"value":["2025-01-01","11"]},{"value":["2025-01-02","15"]},{"value":["2025-01-03","13"]},{"value":["2025-01-04","16"]},{"value":["2025-01-05","14"]},{"value":["2025-01-06","15"]},{"value":["2025-01-07","18"]},{"value":["2025-01-08","14"]},{"value":["2025-01-09","17"]},{"value":["2025-01-10","16"]},{"value":["2025-01-11","18"]},{"value":["2025-01-12","19"]},{"value":["2025-01-13","19"]},{"value":["2025-01-14","19"]},{"value":["2025-01-15","13"]},{"value":["2025-01-16","13"]},{"value":["2025-01-17","11"]},{"value":["2025-01-18","18"]},{"value":["2025-01-19","11"]},{"value":["2025-01-20","11"]}],"yAxisIndex":0,"xAxisIndex":0,"name":"a","type":"line","coordinateSystem":"cartesian2d","symbol":"none","xAxisId":"chart1","yAxisId":"chart1"},{"data":[{"value":["2025-01-01","14"]},{"value":["2025-01-02","19"]},{"value":["2025-01-03","18"]},{"value":["2025-01-04","19"]},{"value":["2025-01-05","11"]},{"value":["2025-01-06","19"]},{"value":["2025-01-07","10"]},{"value":["2025-01-08","19"]},{"value":["2025-01-09","11"]},{"value":["2025-01-10","19"]},{"value":["2025-01-11","10"]},{"value":["2025-01-12","17"]},{"value":["2025-01-13","15"]},{"value":["2025-01-14","19"]},{"value":["2025-01-15","20"]},{"value":["2025-01-16","12"]},{"value":["2025-01-17","19"]},{"value":["2025-01-18","10"]},{"value":["2025-01-19","10"]},{"value":["2025-01-20","20"]}],"yAxisIndex":1,"xAxisIndex":1,"name":"b","type":"line","coordinateSystem":"cartesian2d","symbol":"none","xAxisId":"chart2","yAxisId":"chart2"},{"data":[{"value":["2025-01-01","15"]},{"value":["2025-01-02","19"]},{"value":["2025-01-03","10"]},{"value":["2025-01-04","17"]},{"value":["2025-01-05","15"]},{"value":["2025-01-06","19"]},{"value":["2025-01-07","11"]},{"value":["2025-01-08","16"]},{"value":["2025-01-09","18"]},{"value":["2025-01-10","16"]},{"value":["2025-01-11","17"]},{"value":["2025-01-12","12"]},{"value":["2025-01-13","20"]},{"value":["2025-01-14","14"]},{"value":["2025-01-15","20"]},{"value":["2025-01-16","20"]},{"value":["2025-01-17","18"]},{"value":["2025-01-18","10"]},{"value":["2025-01-19","14"]},{"value":["2025-01-20","18"]}],"yAxisIndex":2,"xAxisIndex":2,"name":"c","type":"line","coordinateSystem":"cartesian2d","symbol":"none","xAxisId":"chart3","yAxisId":"chart3"},{"data":[{"value":["2025-01-01","11"]},{"value":["2025-01-02","15"]},{"value":["2025-01-03","13"]},{"value":["2025-01-04","16"]},{"value":["2025-01-05","14"]},{"value":["2025-01-06","15"]},{"value":["2025-01-07","18"]},{"value":["2025-01-08","14"]},{"value":["2025-01-09","17"]},{"value":["2025-01-10","16"]},{"value":["2025-01-11","18"]},{"value":["2025-01-12","19"]},{"value":["2025-01-13","19"]},{"value":["2025-01-14","19"]},{"value":["2025-01-15","13"]},{"value":["2025-01-16","13"]},{"value":["2025-01-17","11"]},{"value":["2025-01-18","18"]},{"value":["2025-01-19","11"]},{"value":["2025-01-20","11"]}],"yAxisIndex":3,"xAxisIndex":3,"name":"d","type":"line","coordinateSystem":"cartesian2d","symbol":"none","xAxisId":"chart4","yAxisId":"chart4"},{"data":[{"value":["2025-01-01","14"]},{"value":["2025-01-02","19"]},{"value":["2025-01-03","18"]},{"value":["2025-01-04","19"]},{"value":["2025-01-05","11"]},{"value":["2025-01-06","19"]},{"value":["2025-01-07","10"]},{"value":["2025-01-08","19"]},{"value":["2025-01-09","11"]},{"value":["2025-01-10","19"]},{"value":["2025-01-11","10"]},{"value":["2025-01-12","17"]},{"value":["2025-01-13","15"]},{"value":["2025-01-14","19"]},{"value":["2025-01-15","20"]},{"value":["2025-01-16","12"]},{"value":["2025-01-17","19"]},{"value":["2025-01-18","10"]},{"value":["2025-01-19","10"]},{"value":["2025-01-20","20"]}],"yAxisIndex":4,"xAxisIndex":4,"name":"e","type":"line","coordinateSystem":"cartesian2d","symbol":"none","xAxisId":"chart5","yAxisId":"chart5"},{"data":[{"value":["2025-01-01","15"]},{"value":["2025-01-02","19"]},{"value":["2025-01-03","10"]},{"value":["2025-01-04","17"]},{"value":["2025-01-05","15"]},{"value":["2025-01-06","19"]},{"value":["2025-01-07","11"]},{"value":["2025-01-08","16"]},{"value":["2025-01-09","18"]},{"value":["2025-01-10","16"]},{"value":["2025-01-11","17"]},{"value":["2025-01-12","12"]},{"value":["2025-01-13","20"]},{"value":["2025-01-14","14"]},{"value":["2025-01-15","20"]},{"value":["2025-01-16","20"]},{"value":["2025-01-17","18"]},{"value":["2025-01-18","10"]},{"value":["2025-01-19","14"]},{"value":["2025-01-20","18"]}],"yAxisIndex":5,"xAxisIndex":5,"name":"f","type":"line","coordinateSystem":"cartesian2d","symbol":"none","xAxisId":"chart6","yAxisId":"chart6"}],"grid":[{"id":"chart1","coordinateSystem":"matrix","coord":[0,0],"left":"5","top":"25%","right":"5","bottom":"5"},{"id":"chart2","coordinateSystem":"matrix","coord":[1,1],"left":"5","top":"25%","right":"5","bottom":"5"},{"id":"chart3","coordinateSystem":"matrix","coord":[2,2],"left":"5","top":"25%","right":"5","bottom":"5"},{"id":"chart4","coordinateSystem":"matrix","coord":[3,3],"left":"5","top":"25%","right":"5","bottom":"5"},{"id":"chart5","coordinateSystem":"matrix","coord":[4,4],"left":"5","top":"25%","right":"5","bottom":"5"},{"id":"chart6","coordinateSystem":"matrix","coord":[5,5],"left":"5","top":"25%","right":"5","bottom":"5"}],"title":[{"text":"Group Temps"},{"text":"a","coordinateSystem":"matrix","coord":[0,0],"textStyle":{"fontSize":10},"left":"center","top":"top"},{"text":"b","coordinateSystem":"matrix","coord":[1,1],"textStyle":{"fontSize":10},"left":"center","top":"top"},{"text":"c","coordinateSystem":"matrix","coord":[2,2],"textStyle":{"fontSize":10},"left":"center","top":"top"},{"text":"d","coordinateSystem":"matrix","coord":[3,3],"textStyle":{"fontSize":10},"left":"center","top":"top"},{"text":"e","coordinateSystem":"matrix","coord":[4,4],"textStyle":{"fontSize":10},"left":"center","top":"top"},{"text":"f","coordinateSystem":"matrix","coord":[5,5],"textStyle":{"fontSize":10},"left":"center","top":"top"}]},"dispose":true},"evals":[],"jsHooks":[]}
```
