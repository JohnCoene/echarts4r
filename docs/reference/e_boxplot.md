# Boxplot

Draw boxplot.

## Usage

``` r
e_boxplot(e, serie, name = NULL, outliers = TRUE, ...)

e_boxplot_(e, serie, name = NULL, outliers = TRUE, ...)
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

- outliers:

  Whether to plot outliers.

- ...:

  Any other option to pass, check See Also section.

## See also

[Additional
arguments](https://echarts.apache.org/en/option.html#series-boxplot)

## Examples

``` r
df <- data.frame(
  x = c(1:10, 25),
  y = c(1:10, -6)
)

df |>
  e_charts() |>
  e_boxplot(y, outliers = TRUE) |>
  e_boxplot(x, outliers = TRUE)

{"x":{"theme":"","tl":false,"draw":true,"renderer":"canvas","events":[],"buttons":[],"settings":{"crosstalk_key":null,"crosstalk_group":null},"opts":{"yAxis":[{"show":true}],"series":[{"name":"y","type":"boxplot","data":[[1,2.5,5,7.5,10],[1,3.5,6,8.5,10]]},{"type":"scatter","data":[[0,-6],[1,25]]}],"xAxis":[{"data":["y","x"],"type":"category"}]},"dispose":true},"evals":[],"jsHooks":[]}
```
