# Visual Map

Visual Map

## Usage

``` r
e_visual_map(
  e,
  serie,
  calculable = TRUE,
  type = c("continuous", "piecewise"),
  scale = NULL,
  ...
)

e_visual_map_(
  e,
  serie = NULL,
  calculable = TRUE,
  type = c("continuous", "piecewise"),
  scale = NULL,
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

  Column name of serie to scale against.

- calculable:

  Whether show handles, which can be dragged to adjust "selected range".

- type:

  One of `continuous` or `piecewise`.

- scale:

  A function that takes a vector of `numeric` and returns a vector of
  `numeric` of the same length.

- ...:

  Any other option to pass, check See Also section.

## Scaling function

defaults to `e_scale` which is a basic function that rescales `size`
between 1 and 20 for that makes for decent sized points on the chart.

## See also

[Additional
arguments](https://echarts.apache.org/en/option.html#visualMap)

## Examples

``` r
# scaled data
mtcars |>
  e_charts(mpg) |>
  e_scatter(wt, qsec, scale = e_scale) |>
  e_visual_map(qsec, scale = e_scale)

{"x":{"theme":"","tl":false,"draw":true,"renderer":"canvas","events":[],"buttons":[],"settings":{"crosstalk_key":null,"crosstalk_group":null},"opts":{"yAxis":[{"show":true}],"xAxis":[{"type":"value"}],"legend":{"data":["wt"]},"series":[{"data":[{"value":[10.4,5.25,17.98,8.871428571428574]},{"value":[10.4,5.424,17.82,8.509523809523811]},{"value":[13.3,3.84,15.41,3.058333333333334]},{"value":[14.3,3.57,15.84,4.030952380952382]},{"value":[14.7,5.345,17.42,7.60476190476191]},{"value":[15,3.57,14.6,1.226190476190475]},{"value":[15.2,3.78,18,8.916666666666668]},{"value":[15.2,3.435,17.3,7.333333333333336]},{"value":[15.5,3.52,16.87,6.360714285714288]},{"value":[15.8,3.17,14.5,1]},{"value":[16.4,4.07,17.4,7.559523809523808]},{"value":[17.3,3.73,17.6,8.011904761904766]},{"value":[17.8,3.44,18.9,10.95238095238095]},{"value":[18.1,3.46,20.22,13.93809523809524]},{"value":[18.7,3.44,17.02,6.7]},{"value":[19.2,3.44,18.3,9.595238095238098]},{"value":[19.2,3.845,17.05,6.767857142857146]},{"value":[19.7,2.77,15.5,3.261904761904762]},{"value":[21,2.62,16.46,5.433333333333336]},{"value":[21,2.875,17.02,6.7]},{"value":[21.4,3.215,19.44,12.17380952380953]},{"value":[21.4,2.78,18.6,10.27380952380953]},{"value":[21.5,2.465,20.01,13.46309523809524]},{"value":[22.8,2.32,18.61,10.29642857142857]},{"value":[22.8,3.15,22.9,20]},{"value":[24.4,3.19,20,13.44047619047619]},{"value":[26,2.14,16.7,5.976190476190475]},{"value":[27.3,1.935,18.9,10.95238095238095]},{"value":[30.4,1.615,18.52,10.09285714285714]},{"value":[30.4,1.513,16.9,6.428571428571426]},{"value":[32.4,2.2,19.47,12.24166666666667]},{"value":[33.9,1.835,19.9,13.21428571428571]}],"name":"wt","type":"scatter","symbol":null,"coordinateSystem":"cartesian2d","yAxisIndex":0,"xAxisIndex":0,"symbolSize":"function(data){ return data[3];}"}],"visualMap":[{"calculable":true,"type":"continuous","min":1,"max":20}]},"dispose":true},"evals":["opts.series.0.symbolSize"],"jsHooks":[]}
# dimension
# color according to y axis
mtcars |>
  e_charts(mpg) |>
  e_scatter(wt) |>
  e_visual_map(wt, dimension = 1)

{"x":{"theme":"","tl":false,"draw":true,"renderer":"canvas","events":[],"buttons":[],"settings":{"crosstalk_key":null,"crosstalk_group":null},"opts":{"yAxis":[{"show":true}],"xAxis":[{"type":"value"}],"legend":{"data":["wt"]},"series":[{"data":[{"value":[10.4,5.25]},{"value":[10.4,5.424]},{"value":[13.3,3.84]},{"value":[14.3,3.57]},{"value":[14.7,5.345]},{"value":[15,3.57]},{"value":[15.2,3.78]},{"value":[15.2,3.435]},{"value":[15.5,3.52]},{"value":[15.8,3.17]},{"value":[16.4,4.07]},{"value":[17.3,3.73]},{"value":[17.8,3.44]},{"value":[18.1,3.46]},{"value":[18.7,3.44]},{"value":[19.2,3.44]},{"value":[19.2,3.845]},{"value":[19.7,2.77]},{"value":[21,2.62]},{"value":[21,2.875]},{"value":[21.4,3.215]},{"value":[21.4,2.78]},{"value":[21.5,2.465]},{"value":[22.8,2.32]},{"value":[22.8,3.15]},{"value":[24.4,3.19]},{"value":[26,2.14]},{"value":[27.3,1.935]},{"value":[30.4,1.615]},{"value":[30.4,1.513]},{"value":[32.4,2.2]},{"value":[33.9,1.835]}],"name":"wt","type":"scatter","symbol":null,"coordinateSystem":"cartesian2d","yAxisIndex":0,"xAxisIndex":0,"symbolSize":3}],"visualMap":[{"dimension":1,"calculable":true,"type":"continuous","min":1.513,"max":5.424}]},"dispose":true},"evals":[],"jsHooks":[]}
# color according to x axis
mtcars |>
  e_charts(mpg) |>
  e_scatter(wt) |>
  e_visual_map(mpg, dimension = 0)

{"x":{"theme":"","tl":false,"draw":true,"renderer":"canvas","events":[],"buttons":[],"settings":{"crosstalk_key":null,"crosstalk_group":null},"opts":{"yAxis":[{"show":true}],"xAxis":[{"type":"value"}],"legend":{"data":["wt"]},"series":[{"data":[{"value":[10.4,5.25]},{"value":[10.4,5.424]},{"value":[13.3,3.84]},{"value":[14.3,3.57]},{"value":[14.7,5.345]},{"value":[15,3.57]},{"value":[15.2,3.78]},{"value":[15.2,3.435]},{"value":[15.5,3.52]},{"value":[15.8,3.17]},{"value":[16.4,4.07]},{"value":[17.3,3.73]},{"value":[17.8,3.44]},{"value":[18.1,3.46]},{"value":[18.7,3.44]},{"value":[19.2,3.44]},{"value":[19.2,3.845]},{"value":[19.7,2.77]},{"value":[21,2.62]},{"value":[21,2.875]},{"value":[21.4,3.215]},{"value":[21.4,2.78]},{"value":[21.5,2.465]},{"value":[22.8,2.32]},{"value":[22.8,3.15]},{"value":[24.4,3.19]},{"value":[26,2.14]},{"value":[27.3,1.935]},{"value":[30.4,1.615]},{"value":[30.4,1.513]},{"value":[32.4,2.2]},{"value":[33.9,1.835]}],"name":"wt","type":"scatter","symbol":null,"coordinateSystem":"cartesian2d","yAxisIndex":0,"xAxisIndex":0,"symbolSize":3}],"visualMap":[{"dimension":0,"calculable":true,"type":"continuous","min":10.4,"max":33.9}]},"dispose":true},"evals":[],"jsHooks":[]}
v <- LETTERS[1:10]
matrix <- data.frame(
  x = sample(v, 300, replace = TRUE),
  y = sample(v, 300, replace = TRUE),
  z = rnorm(300, 10, 1),
  color = rnorm(300, 10, 1),
  size = rnorm(300, 10, 1),
  stringsAsFactors = FALSE
) |>
  dplyr::group_by(x, y) |>
  dplyr::summarise(
    z = sum(z),
    color = sum(color),
    size = sum(size)
  ) |>
  dplyr::ungroup()
#> `summarise()` has regrouped the output.
#> ℹ Summaries were computed grouped by x and y.
#> ℹ Output is grouped by x.
#> ℹ Use `summarise(.groups = "drop_last")` to silence this message.
#> ℹ Use `summarise(.by = c(x, y))` for per-operation grouping
#>   (`?dplyr::dplyr_by`) instead.

matrix |>
  e_charts(x) |>
  e_scatter_3d(y, z, color, size) |>
  e_visual_map(
    z,
    # scale to z
    inRange = list(symbolSize = c(1, 30)),
    # scale size
    dimension = 3 # third dimension 0 = x, y = 1, z = 2, size = 3
  ) |>
  e_visual_map(
    z,
    # scale to z
    inRange = list(color = c("#bf444c", "#d88273", "#f6efa6")),
    # scale colors
    dimension = 4,
    # third dimension 0 = x, y = 1, z = 2, size = 3, color = 4
    bottom = 300 # padding to avoid visual maps overlap
  )

{"x":{"theme":"","tl":false,"draw":true,"renderer":"canvas","events":[],"buttons":[],"settings":{"crosstalk_key":null,"crosstalk_group":null},"opts":{"zAxis3D":[{"show":true}],"grid3D":[{"show":true}],"xAxis3D":[{"type":"category","data":["A","B","C","D","E","F","G","H","I","J"]}],"yAxis3D":[{"type":"category","data":["B","C","D","E","F","G","H","I","J","A"]}],"series":[{"type":"scatter3D","coordinateSystem":"cartesian3D","data":[{"value":["A","B","40.086996","41.609015","39.532733"]},{"value":["A","C","20.154925","18.428357","21.765365"]},{"value":["A","D","50.619902","47.356773","52.628365"]},{"value":["A","E","19.442270","18.947244","16.661291"]},{"value":["A","F","38.588849","37.962319","37.760019"]},{"value":["A","G","32.174035","26.990147","29.530053"]},{"value":["A","H","32.533236","30.178480","28.006205"]},{"value":["A","I","29.180301","28.543031","26.832806"]},{"value":["A","J"," 8.664670","12.294797","10.316449"]},{"value":["B","A","56.526170","63.168060","60.625123"]},{"value":["B","B","18.810660","22.366256","19.784567"]},{"value":["B","C","42.732448","37.824110","41.442150"]},{"value":["B","D"," 9.687277"," 8.666817","10.567491"]},{"value":["B","E"," 8.646423"," 8.639314","10.587295"]},{"value":["B","F","31.221987","30.853801","29.424431"]},{"value":["B","G","10.836076","11.315839","10.310768"]},{"value":["B","H","33.731729","29.357689","30.377233"]},{"value":["B","J","29.210776","28.845436","28.804179"]},{"value":["C","A","30.334737","29.940989","29.663357"]},{"value":["C","B","40.559353","38.093314","38.486623"]},{"value":["C","C","20.897259","19.668436","22.229616"]},{"value":["C","D","29.017584","30.295374","30.376007"]},{"value":["C","E","27.117615","27.407906","30.748949"]},{"value":["C","F","18.230929","20.415302","18.937144"]},{"value":["C","G","44.436552","40.093043","40.936582"]},{"value":["C","H"," 9.733040","10.315401"," 9.079776"]},{"value":["C","I","73.725286","71.153237","66.841245"]},{"value":["C","J","81.828839","83.987200","79.750370"]},{"value":["D","A"," 9.084415","11.009596"," 9.194728"]},{"value":["D","B","22.691882","19.779507","19.192384"]},{"value":["D","C","31.612511","30.789098","30.468307"]},{"value":["D","D","38.750803","39.595212","42.462802"]},{"value":["D","E","20.046178","20.292513","22.278841"]},{"value":["D","F","36.987220","38.931922","40.552500"]},{"value":["D","G","10.235235","10.636092","10.720933"]},{"value":["D","I","19.554692","19.276324","19.763678"]},{"value":["D","J","62.293338","61.761872","59.740678"]},{"value":["E","A","50.931892","48.156361","51.729412"]},{"value":["E","B"," 9.579862","11.257425","11.540403"]},{"value":["E","D","50.503968","47.718830","51.631563"]},{"value":["E","E","29.069410","31.616258","31.059040"]},{"value":["E","F","37.028025","37.056734","38.048980"]},{"value":["E","G"," 9.931185"," 9.921243","11.009417"]},{"value":["E","H"," 8.790963","11.160756","10.609250"]},{"value":["E","I","71.866320","71.420386","69.318443"]},{"value":["E","J","31.324504","29.803982","28.579136"]},{"value":["F","A","31.893566","29.371122","28.801386"]},{"value":["F","B"," 9.444218","10.627847"," 9.996029"]},{"value":["F","C","11.253602"," 9.827144"," 9.096636"]},{"value":["F","E","20.184100","19.428988","20.826663"]},{"value":["F","F","29.672911","28.318515","27.702886"]},{"value":["F","G","18.781930","20.103186","19.659805"]},{"value":["F","H","40.735132","36.505618","42.161799"]},{"value":["F","I","47.638494","49.774054","50.451598"]},{"value":["F","J","10.586560"," 9.941884"," 9.906127"]},{"value":["G","B"," 8.808991","12.252591","10.301049"]},{"value":["G","C","21.244609","20.018221","16.316365"]},{"value":["G","D"," 9.629518"," 9.210818","10.835387"]},{"value":["G","E","71.676699","69.285251","68.376810"]},{"value":["G","F","40.107159","40.288662","41.250042"]},{"value":["G","G","67.614079","72.435140","71.932153"]},{"value":["G","I","39.924009","42.649589","41.525047"]},{"value":["G","J","22.079982","17.465078","22.557987"]},{"value":["H","A","38.957816","39.673168","37.952472"]},{"value":["H","B","40.614751","38.843744","41.179299"]},{"value":["H","C","69.277274","68.344785","71.139750"]},{"value":["H","D","51.696264","48.414445","47.179866"]},{"value":["H","E","20.836304","18.665207","19.099718"]},{"value":["H","F","60.253522","59.863568","63.494747"]},{"value":["H","G","16.381252","20.333800","20.706610"]},{"value":["H","H","10.825694"," 8.036266"," 8.105275"]},{"value":["H","I"," 8.974002","10.331295","12.075021"]},{"value":["H","J","19.434331","20.726691","18.073634"]},{"value":["I","A","30.669114","29.945358","30.316053"]},{"value":["I","B","47.406640","49.174391","48.433163"]},{"value":["I","C","40.761250","39.660634","39.254995"]},{"value":["I","D","39.920276","44.058161","39.586483"]},{"value":["I","E","28.287540","30.532156","30.077261"]},{"value":["I","F","29.043152","28.542008","29.214469"]},{"value":["I","G","41.946952","43.203743","39.062269"]},{"value":["I","H","20.437420","21.242237","21.284361"]},{"value":["I","I"," 9.506702"," 9.217343","11.565765"]},{"value":["I","J","38.573663","38.464102","39.679619"]},{"value":["J","A","44.379017","49.739740","51.111696"]},{"value":["J","B","19.187315","19.618606","20.294107"]},{"value":["J","C","81.720851","76.000641","81.459580"]},{"value":["J","D","61.888874","57.129639","60.111228"]},{"value":["J","E","21.679434","20.893930","20.074446"]},{"value":["J","F","39.602083","38.804349","33.693719"]},{"value":["J","G","18.443413","20.946037","22.272719"]},{"value":["J","H","40.586631","43.435259","44.079311"]},{"value":["J","I","17.695628","21.269523","22.035949"]},{"value":["J","J","48.055262","49.306661","49.022202"]}]}],"visualMap":[{"inRange":{"symbolSize":[1,30]},"dimension":3,"calculable":true,"type":"continuous","min":8.646423350454032,"max":81.82883945742326},{"inRange":{"color":["#bf444c","#d88273","#f6efa6"]},"dimension":4,"bottom":300,"calculable":true,"type":"continuous","min":8.646423350454032,"max":81.82883945742326}]},"dispose":true},"evals":[],"jsHooks":[]}
```
