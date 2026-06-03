# Toolbox

The toolbox allows you to add neat little interactive functions to your
plots.

## Features

- saveAsImage
- brush
- restore
- dataView
- dataZoom
- magicType

## Save plot

``` r
echart <- mtcars |> 
  e_charts(qsec) |> 
  e_line(mpg, smooth = TRUE)

echart |> e_toolbox_feature(feature = "saveAsImage") # hit the download button!
```

## data Zoom

``` r
echart |> e_toolbox_feature(feature = "dataZoom")
```

## data view

``` r
echart |> e_toolbox_feature(feature = "dataView")
```

See the [official
documentation](https://echarts.apache.org/en/option.html#toolbox.feature)
for the full list.
