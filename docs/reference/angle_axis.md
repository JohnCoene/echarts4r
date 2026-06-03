# Angle axis

Customise angle axis.

## Usage

``` r
e_angle_axis(e, serie, show = TRUE, ...)

e_angle_axis_(e, serie = NULL, show = TRUE, ...)
```

## Arguments

- e:

  An `echarts4r` object as returned by
  [`e_charts`](https://echarts4r.john-coene.com/reference/init.md) or a
  proxy as returned by
  [`echarts4rProxy`](https://echarts4r.john-coene.com/reference/echarts4r-shiny.md).

- serie:

  Serie to use as axis labels.

- show:

  Whether to display the axis.

- ...:

  Any other option to pass, check See Also section.

## See also

[Additional
arguments](https://echarts.apache.org/en/option.html#angleAxis)

## Examples

``` r
df <- data.frame(x = 1:100, y = seq(1, 200, by = 2))

df |>
  e_charts(x) |>
  e_polar(FALSE) |>
  e_angle_axis(FALSE) |>
  e_radius_axis(FALSE) |>
  e_line(y, coord_system = "polar", smooth = TRUE) |>
  e_legend(show = FALSE)

{"x":{"theme":"","tl":false,"draw":true,"renderer":"canvas","events":[],"buttons":[],"settings":{"crosstalk_key":null,"crosstalk_group":null},"opts":{"polar":{"show":false},"angleAxis":{"show":true,"data":[]},"radiusAxis":{"show":true,"data":[]},"legend":{"data":["y"],"show":false,"type":"plain"},"series":[{"data":[1,3,5,7,9,11,13,15,17,19,21,23,25,27,29,31,33,35,37,39,41,43,45,47,49,51,53,55,57,59,61,63,65,67,69,71,73,75,77,79,81,83,85,87,89,91,93,95,97,99,101,103,105,107,109,111,113,115,117,119,121,123,125,127,129,131,133,135,137,139,141,143,145,147,149,151,153,155,157,159,161,163,165,167,169,171,173,175,177,179,181,183,185,187,189,191,193,195,197,199],"name":"y","type":"line","coordinateSystem":"polar","smooth":true}]},"dispose":true},"evals":[],"jsHooks":[]}
df <- data.frame(x = LETTERS[1:5], y = runif(5))

df |>
  e_charts(x) |>
  e_polar() |>
  e_angle_axis(x) |>
  e_radius_axis() |>
  e_line(y, coord_system = "polar", smooth = TRUE)

{"x":{"theme":"","tl":false,"draw":true,"renderer":"canvas","events":[],"buttons":[],"settings":{"crosstalk_key":null,"crosstalk_group":null},"opts":{"polar":{"show":true},"angleAxis":{"show":true,"data":["A","B","C","D","E"]},"radiusAxis":{"show":true},"legend":{"data":["y"]},"series":[{"data":[0.08075013756752014,0.8343330372590572,0.6007608862128109,0.1572084415238351,0.007399441208690405],"name":"y","type":"line","coordinateSystem":"polar","smooth":true}]},"dispose":true},"evals":[],"jsHooks":[]}
```
