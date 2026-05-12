# Select Visual Map

Selects data range of visual mapping.

## Usage

``` r
e_visual_map_range(e, ..., btn = NULL)
```

## Arguments

- e:

  An `echarts4r` object as returned by
  [`e_charts`](https://echarts4r.john-coene.com/reference/init.md) or a
  proxy as returned by
  [`echarts4rProxy`](https://echarts4r.john-coene.com/reference/echarts4r-shiny.md).

- ...:

  Any options, see [official
  documentation](https://echarts.apache.org/en/api.html#action.visualMap)

- btn:

  A [`e_button`](https://echarts4r.john-coene.com/reference/e_button.md)
  id.

## Examples

``` r
data("state")

as.data.frame(state.x77) |>
  e_charts(Population) |>
  e_scatter(Income, Frost) |>
  e_visual_map(Frost, scale = e_scale) |>
  e_legend(FALSE) |>
  e_visual_map_range(
    selected = list(60, 120)
  )

{"x":{"theme":"","tl":false,"draw":true,"renderer":"canvas","events":[{"data":{"type":"selectDataRange","selected":[60,120]}}],"buttons":[],"settings":{"crosstalk_key":null,"crosstalk_group":null},"opts":{"yAxis":[{"show":true}],"xAxis":[{"type":"value"}],"legend":{"data":["Income"],"show":false,"type":"plain"},"series":[{"data":[{"value":[365,6315,152,16.36170212765957]},{"value":[376,4566,173,18.48404255319149]},{"value":[472,3907,168,17.97872340425532]},{"value":[579,4809,103,11.40957446808511]},{"value":[590,5149,188,20]},{"value":[637,5087,186,19.79787234042553]},{"value":[681,4167,172,18.38297872340425]},{"value":[746,4347,155,16.66489361702128]},{"value":[812,4281,174,18.58510638297872]},{"value":[813,4119,126,13.73404255319149]},{"value":[868,4963,0,1]},{"value":[931,4558,127,13.83510638297872]},{"value":[1058,3694,161,17.27127659574468]},{"value":[1144,3601,120,13.12765957446809]},{"value":[1203,4022,137,14.84574468085106]},{"value":[1544,4508,139,15.04787234042553]},{"value":[1799,3617,100,11.1063829787234]},{"value":[2110,3378,65,7.569148936170214]},{"value":[2212,4530,15,2.51595744680851]},{"value":[2280,4669,114,12.52127659574468]},{"value":[2284,4660,44,5.446808510638298]},{"value":[2341,3098,50,6.053191489361702]},{"value":[2541,4884,166,17.77659574468085]},{"value":[2715,3983,82,9.287234042553191]},{"value":[2816,3635,65,7.569148936170214]},{"value":[2861,4628,140,15.14893617021277]},{"value":[3100,5348,139,15.04787234042553]},{"value":[3387,3712,95,10.60106382978723]},{"value":[3559,4864,32,4.23404255319149]},{"value":[3615,3624,20,3.021276595744681]},{"value":[3806,3545,12,2.212765957446808]},{"value":[3921,4675,160,17.17021276595745]},{"value":[4122,5299,101,11.20744680851064]},{"value":[4173,3821,70,8.074468085106382]},{"value":[4589,4468,149,16.05851063829787]},{"value":[4767,4254,108,11.91489361702128]},{"value":[4931,4091,60,7.063829787234043]},{"value":[4981,4701,85,9.590425531914894]},{"value":[5313,4458,122,13.32978723404255]},{"value":[5441,3875,80,9.085106382978724]},{"value":[5814,4755,103,11.40957446808511]},{"value":[7333,5237,115,12.62234042553191]},{"value":[8277,4815,11,2.111702127659575]},{"value":[9111,4751,125,13.63297872340426]},{"value":[10735,4561,124,13.53191489361702]},{"value":[11197,5107,127,13.83510638297872]},{"value":[11860,4449,126,13.73404255319149]},{"value":[12237,4188,35,4.537234042553191]},{"value":[18076,4903,82,9.287234042553191]},{"value":[21198,5114,20,3.021276595744681]}],"name":"Income","type":"scatter","symbol":null,"coordinateSystem":"cartesian2d","yAxisIndex":0,"xAxisIndex":0,"symbolSize":"function(data){ return data[3];}"}],"visualMap":[{"calculable":true,"type":"continuous","min":1,"max":20}]},"dispose":true},"evals":["opts.series.0.symbolSize"],"jsHooks":[]}
```
