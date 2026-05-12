# Axis Jitter

helper function for generating jitter between points in a scatter plot.
This is only applicable to e_scatter().

## Usage

``` r
e_jitter(e, axis = "x", jitter = 20, jitterOverlap = FALSE, jitterMargin = 5)
```

## Arguments

- e:

  An `echarts4r` object as returned by
  [`e_charts`](https://echarts4r.john-coene.com/reference/init.md) or a
  proxy as returned by
  [`echarts4rProxy`](https://echarts4r.john-coene.com/reference/echarts4r-shiny.md).

- axis:

  Axis to apply formatter to. Supports x and y axis

- jitter:

  Pixel units indicating the amount of random noise to add to each data
  point position.

- jitterOverlap:

  Boolean allowing overlap between data points. If false, overlap will
  not be allowed. For some cases, scatters may still overlap if there is
  no reasonable way to avoid.

- jitterMargin:

  When you have jitter and jiterOverlap is FALSE, this is the minimum
  distance in pixels between two data points.

## See also

[Additional
arguments](https://echarts.apache.org/en/option.html#yAxis.jitter)

## Examples

``` r
df <- data.frame(
value = c(rnorm(50, mean = 5, sd = 1),
         rnorm(50, mean = 10, sd = 1),
         rnorm(50, mean = 15, sd = 1)),
         group = rep(c("Group A", "Group B", "Group C"), each = 50)
          )

df |> e_charts(group) |> e_scatter(value) |> e_jitter()

{"x":{"theme":"","tl":false,"draw":true,"renderer":"canvas","events":[],"buttons":[],"settings":{"crosstalk_key":null,"crosstalk_group":null},"opts":{"yAxis":[{"show":true}],"xAxis":[{"data":["Group A","Group B","Group C"],"type":"category","boundaryGap":true,"jitter":20,"jitterOverlap":false,"jitterMargin":5}],"legend":{"data":["value"]},"series":[{"data":[{"value":["Group A"," 5.720257"]},{"value":["Group A"," 5.988348"]},{"value":["Group A"," 5.114347"]},{"value":["Group A"," 5.698961"]},{"value":["Group A"," 3.295801"]},{"value":["Group A"," 5.055242"]},{"value":["Group A"," 5.595056"]},{"value":["Group A"," 6.484381"]},{"value":["Group A"," 4.593034"]},{"value":["Group A"," 5.174645"]},{"value":["Group A"," 5.071263"]},{"value":["Group A"," 4.149965"]},{"value":["Group A"," 3.271764"]},{"value":["Group A"," 4.073747"]},{"value":["Group A"," 6.522847"]},{"value":["Group A"," 6.286652"]},{"value":["Group A"," 5.491891"]},{"value":["Group A"," 3.788819"]},{"value":["Group A"," 5.488408"]},{"value":["Group A"," 4.707115"]},{"value":["Group A"," 5.879755"]},{"value":["Group A"," 4.392486"]},{"value":["Group A"," 4.699731"]},{"value":["Group A"," 3.683310"]},{"value":["Group A"," 4.552575"]},{"value":["Group A"," 6.123191"]},{"value":["Group A"," 5.853159"]},{"value":["Group A"," 4.175604"]},{"value":["Group A"," 5.308861"]},{"value":["Group A"," 6.649227"]},{"value":["Group A"," 2.290169"]},{"value":["Group A"," 4.711550"]},{"value":["Group A"," 4.190035"]},{"value":["Group A"," 5.065194"]},{"value":["Group A"," 5.383443"]},{"value":["Group A"," 5.650497"]},{"value":["Group A"," 5.302304"]},{"value":["Group A"," 4.430129"]},{"value":["Group A"," 5.107883"]},{"value":["Group A"," 4.255243"]},{"value":["Group A"," 5.109542"]},{"value":["Group A"," 5.224545"]},{"value":["Group A"," 4.418709"]},{"value":["Group A"," 4.146277"]},{"value":["Group A"," 5.970431"]},{"value":["Group A"," 6.515451"]},{"value":["Group A"," 4.563586"]},{"value":["Group A"," 6.086142"]},{"value":["Group A"," 4.826469"]},{"value":["Group A"," 5.501093"]},{"value":["Group B","11.075801"]},{"value":["Group B"," 9.496339"]},{"value":["Group B"," 9.141692"]},{"value":["Group B","10.713874"]},{"value":["Group B","10.430984"]},{"value":["Group B","10.238657"]},{"value":["Group B"," 9.218170"]},{"value":["Group B","10.623876"]},{"value":["Group B","10.997475"]},{"value":["Group B","10.865982"]},{"value":["Group B"," 9.286001"]},{"value":["Group B"," 9.582191"]},{"value":["Group B","10.741300"]},{"value":["Group B","10.817401"]},{"value":["Group B","10.035256"]},{"value":["Group B"," 9.980792"]},{"value":["Group B"," 9.753274"]},{"value":["Group B"," 9.085367"]},{"value":["Group B"," 8.365296"]},{"value":["Group B","10.483507"]},{"value":["Group B"," 8.867878"]},{"value":["Group B","11.467987"]},{"value":["Group B"," 8.914459"]},{"value":["Group B","11.936473"]},{"value":["Group B"," 8.737384"]},{"value":["Group B","10.820631"]},{"value":["Group B"," 9.541324"]},{"value":["Group B","10.640518"]},{"value":["Group B","10.803171"]},{"value":["Group B","10.924872"]},{"value":["Group B","10.973124"]},{"value":["Group B","10.884231"]},{"value":["Group B","11.001913"]},{"value":["Group B"," 9.728075"]},{"value":["Group B","10.007279"]},{"value":["Group B"," 9.655127"]},{"value":["Group B","12.678972"]},{"value":["Group B","11.178099"]},{"value":["Group B"," 8.507570"]},{"value":["Group B"," 8.947804"]},{"value":["Group B"," 9.958333"]},{"value":["Group B","10.091641"]},{"value":["Group B"," 8.843153"]},{"value":["Group B","10.231481"]},{"value":["Group B","11.080537"]},{"value":["Group B"," 9.711918"]},{"value":["Group B","10.739446"]},{"value":["Group B","10.515363"]},{"value":["Group B","10.718761"]},{"value":["Group B"," 9.202638"]},{"value":["Group C","15.577435"]},{"value":["Group C","14.870181"]},{"value":["Group C","14.564488"]},{"value":["Group C","15.196704"]},{"value":["Group C","14.848295"]},{"value":["Group C","15.507201"]},{"value":["Group C","15.207262"]},{"value":["Group C","16.185447"]},{"value":["Group C","13.867294"]},{"value":["Group C","16.369587"]},{"value":["Group C","15.860264"]},{"value":["Group C","14.610182"]},{"value":["Group C","15.676682"]},{"value":["Group C","17.005637"]},{"value":["Group C","16.231579"]},{"value":["Group C","14.628291"]},{"value":["Group C","16.217233"]},{"value":["Group C","14.406835"]},{"value":["Group C","13.683997"]},{"value":["Group C","14.876741"]},{"value":["Group C","15.023648"]},{"value":["Group C","13.028808"]},{"value":["Group C","15.671837"]},{"value":["Group C","15.441225"]},{"value":["Group C","15.125086"]},{"value":["Group C","16.698746"]},{"value":["Group C","16.545995"]},{"value":["Group C","16.003872"]},{"value":["Group C","15.951927"]},{"value":["Group C","14.791081"]},{"value":["Group C","17.195649"]},{"value":["Group C","16.236540"]},{"value":["Group C","14.623718"]},{"value":["Group C","14.629521"]},{"value":["Group C","15.117668"]},{"value":["Group C","15.917012"]},{"value":["Group C","14.014625"]},{"value":["Group C","15.672330"]},{"value":["Group C","15.044896"]},{"value":["Group C","15.099403"]},{"value":["Group C","15.088771"]},{"value":["Group C","14.322110"]},{"value":["Group C","15.378883"]},{"value":["Group C","15.753935"]},{"value":["Group C","14.983148"]},{"value":["Group C","15.618046"]},{"value":["Group C","16.241696"]},{"value":["Group C","13.974727"]},{"value":["Group C","13.630258"]},{"value":["Group C","14.841296"]}],"name":"value","type":"scatter","symbol":null,"coordinateSystem":"cartesian2d","yAxisIndex":0,"xAxisIndex":0,"symbolSize":3}]},"dispose":true},"evals":[],"jsHooks":[]}
```
