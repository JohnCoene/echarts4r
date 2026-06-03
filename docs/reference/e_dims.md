# Dimensions

Sets the dimensions of the chart \_internally.\_ This will only affect
the dimensions of the chart within its parent container. Use the
\`height\` and \`width\` arguments of \[e_charts\] if you want to change
the dimensions of said parent (recommended).

## Usage

``` r
e_dims(e, height = "auto", width = "auto")
```

## Arguments

- e:

  An `echarts4r` object as returned by
  [`e_charts`](https://echarts4r.john-coene.com/reference/init.md) or a
  proxy as returned by
  [`echarts4rProxy`](https://echarts4r.john-coene.com/reference/echarts4r-shiny.md).

- height, width:

  Dimensions in pixels, percentage or string.
