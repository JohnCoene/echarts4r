# Single Axis

Setup single axis.

## Usage

``` r
e_single_axis(e, index = 0, ...)
```

## Arguments

- e:

  An `echarts4r` object as returned by
  [`e_charts`](https://echarts4r.john-coene.com/reference/init.md) or a
  proxy as returned by
  [`echarts4rProxy`](https://echarts4r.john-coene.com/reference/echarts4r-shiny.md).

- index:

  Index of axis to customise.

- ...:

  Any other option to pass, check See Also section.

## Examples

``` r
df <- data.frame(
  axis = LETTERS[1:10],
  value = runif(10, 3, 20),
  size = runif(10, 3, 20)
)

df |>
  e_charts(axis) |>
  e_single_axis() |> # add the single axis
  e_scatter(
    value,
    size,
    coord_system = "singleAxis"
  )

{"x":{"theme":"","tl":false,"draw":true,"renderer":"canvas","events":[],"buttons":[],"settings":{"crosstalk_key":null,"crosstalk_group":null},"opts":{"singleAxis":{"type":"category","data":["A","B","C","D","E","F","G","H","I","J"]},"legend":{"data":["value"]},"series":[{"data":[{"value":["A"," 6.111576","16.523868","16.527402"]},{"value":["B","14.473041","19.430997","20.000000"]},{"value":["C"," 6.673005"," 3.524905"," 1.000000"]},{"value":["D","10.966482","15.530949","15.341350"]},{"value":["E","13.889921"," 5.131111"," 2.918631"]},{"value":["F"," 8.127688"," 9.490324"," 8.125758"]},{"value":["G"," 9.766691","11.984781","11.105414"]},{"value":["H","19.705053","14.045482","13.566944"]},{"value":["I"," 7.993271"," 5.274085"," 3.089415"]},{"value":["J","10.268712"," 8.897619"," 7.417765"]}],"name":"value","type":"scatter","symbol":null,"coordinateSystem":"singleAxis","singleAxisIndex":0,"symbolSize":"function(data){ return data[3];}"}]},"dispose":true},"evals":["opts.series.0.symbolSize"],"jsHooks":[]}
```
