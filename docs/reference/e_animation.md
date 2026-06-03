# Animation

Customise animations.

## Usage

``` r
e_animation(
  e,
  show = TRUE,
  threshold = NULL,
  duration = NULL,
  easing = NULL,
  delay = NULL,
  duration.update = NULL,
  easing.update = NULL,
  delay.update = NULL
)
```

## Arguments

- e:

  An `echarts4r` object as returned by
  [`e_charts`](https://echarts4r.john-coene.com/reference/init.md) or a
  proxy as returned by
  [`echarts4rProxy`](https://echarts4r.john-coene.com/reference/echarts4r-shiny.md).

- show:

  Set to show animation.

- threshold:

  Whether to set graphic number threshold to animation. Animation will
  be disabled when graphic number is larger than threshold.

- duration:

  Duration of the first animation.

- easing:

  Easing method used for the first animation.

- delay:

  Delay before updating the first animation.

- duration.update:

  Time for animation to complete.

- easing.update:

  Easing method used for animation.

- delay.update:

  Delay before updating animation.

## See also

[Additional
arguments](https://echarts.apache.org/en/option.html#animation)

## Examples

``` r
mtcars |>
  e_charts(mpg) |>
  e_area(drat) |>
  e_animation(duration = 10000)

{"x":{"theme":"","tl":false,"draw":true,"renderer":"canvas","events":[],"buttons":[],"settings":{"crosstalk_key":null,"crosstalk_group":null},"opts":{"yAxis":[{"show":true}],"xAxis":[{"type":"value"}],"legend":{"data":["drat"]},"series":[{"data":[{"value":[10.4,2.93]},{"value":[10.4,3]},{"value":[13.3,3.73]},{"value":[14.3,3.21]},{"value":[14.7,3.23]},{"value":[15,3.54]},{"value":[15.2,3.07]},{"value":[15.2,3.15]},{"value":[15.5,2.76]},{"value":[15.8,4.22]},{"value":[16.4,3.07]},{"value":[17.3,3.07]},{"value":[17.8,3.92]},{"value":[18.1,2.76]},{"value":[18.7,3.15]},{"value":[19.2,3.92]},{"value":[19.2,3.08]},{"value":[19.7,3.62]},{"value":[21,3.9]},{"value":[21,3.9]},{"value":[21.4,3.08]},{"value":[21.4,4.11]},{"value":[21.5,3.7]},{"value":[22.8,3.85]},{"value":[22.8,3.92]},{"value":[24.4,3.69]},{"value":[26,4.43]},{"value":[27.3,4.08]},{"value":[30.4,4.93]},{"value":[30.4,3.77]},{"value":[32.4,4.08]},{"value":[33.9,4.22]}],"yAxisIndex":0,"xAxisIndex":0,"name":"drat","type":"line","coordinateSystem":"cartesian2d","areaStyle":[]}],"animation":true,"animationDuration":10000},"dispose":true},"evals":[],"jsHooks":[]}
```
