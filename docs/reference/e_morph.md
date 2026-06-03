# Morphing

\_\_This is experimental\_\_

## Usage

``` r
e_morph(e, ..., callback, default = 1L)
```

## Arguments

- e, ...:

  Graphs (from \`e_graph\`).

- callback:

  JavaScript callback function as a character string (vector of length
  1). This function has access to the \`chart\` object, as well as
  \`opts\` an array containing the options of the charts passed to \`e\`
  and \`...\`.

- default:

  Default chart to show.

## Details

Morph between graphs.

## Examples

``` r
mtcars2 <- mtcars |>
  head() |>
  tibble::rownames_to_column("model")

e1 <- mtcars2 |>
  e_charts(model) |>
  e_bar(
    carb,
    universalTransition = TRUE,
    animationDurationUpdate = 1000L
  )

e2 <- mtcars2 |>
  e_charts(model) |>
  e_pie(
    carb,
    universalTransition = TRUE,
    animationDurationUpdate = 1000L
  )

cb <- "() => {
  let x = 0;
  setInterval(() => {
    x++
    chart.setOption(opts[x % 2], true);
  }, 3000);
}"

e_morph(e1, e2, callback = cb)
#> Warning: This is experimental

{"x":{"theme":"","tl":false,"draw":true,"renderer":"canvas","events":[],"buttons":[],"settings":{"crosstalk_key":null,"crosstalk_group":null},"opts":[[{"yAxis":[{"show":true}],"xAxis":[{"data":["Mazda RX4","Mazda RX4 Wag","Datsun 710","Hornet 4 Drive","Hornet Sportabout","Valiant"],"type":"category","boundaryGap":true}],"legend":{"data":["carb"]},"series":[{"data":[{"value":["Mazda RX4","4"]},{"value":["Mazda RX4 Wag","4"]},{"value":["Datsun 710","1"]},{"value":["Hornet 4 Drive","1"]},{"value":["Hornet Sportabout","2"]},{"value":["Valiant","1"]}],"name":"carb","type":"bar","yAxisIndex":0,"xAxisIndex":0,"coordinateSystem":"cartesian2d","universalTransition":true,"animationDurationUpdate":1000}]},{"legend":{"data":["Mazda RX4","Mazda RX4 Wag","Datsun 710","Hornet 4 Drive","Hornet Sportabout","Valiant"]},"series":[{"name":"carb","type":"pie","universalTransition":true,"animationDurationUpdate":1000,"data":[{"value":4,"name":"Mazda RX4"},{"value":4,"name":"Mazda RX4 Wag"},{"value":1,"name":"Datsun 710"},{"value":1,"name":"Hornet 4 Drive"},{"value":2,"name":"Hornet Sportabout"},{"value":1,"name":"Valiant"}]}]}]],"dispose":true,"morphed":{"callback":"() => {\n  let x = 0;\n  setInterval(() => {\n    x++\n    chart.setOption(opts[x % 2], true);\n  }, 3000);\n}","default":0}},"evals":[],"jsHooks":[]}
```
