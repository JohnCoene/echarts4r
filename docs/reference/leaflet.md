# Leaflet

Leaflet extension.

## Usage

``` r
e_leaflet(e, roam = TRUE, ...)

e_leaflet_tile(
  e,
  template = "https://{s}.tile.openstreetmap.fr/hot/{z}/{x}/{y}.png",
  options = NULL,
  ...
)
```

## Arguments

- e:

  An `echarts4r` object as returned by
  [`e_charts`](https://echarts4r.john-coene.com/reference/init.md) or a
  proxy as returned by
  [`echarts4rProxy`](https://echarts4r.john-coene.com/reference/echarts4r-shiny.md).

- roam:

  Whether to allow the user to roam.

- ...:

  Any other option to pass, check See Also section.

- template:

  `urlTemplate`, should not be changed.

- options:

  List of options, including `attribution` and `label`.

## Examples

``` r
population |>
  dplyr::filter(value > 8) |>
  e_charts(lon) |>
  e_leaflet() |>
  e_leaflet_tile(options = list(maxZoom = 3)) |>
  e_scatter(lat, size = value, coord_system = "leaflet")

{"x":{"theme":"","tl":false,"draw":true,"renderer":"canvas","events":[],"buttons":[],"settings":{"crosstalk_key":null,"crosstalk_group":null},"opts":{"leaflet":{"roam":true,"tiles":[{"urlTemplate":"https://{s}.tile.openstreetmap.fr/hot/{z}/{x}/{y}.png","options":{"maxZoom":3}}]},"legend":{"data":["lat"]},"series":[{"data":[{"value":[-118.5,34,8.056997308601568,1]},{"value":[-99.5,19.5,8.185405962985284,3.163729131189309]},{"value":[-74,41,8.686159293163033,11.60159161986485]},{"value":[-58.5,-34.5,8.237082384460775,4.034494260524062]},{"value":[-47,-23.5,8.170666578529319,2.91536553236311]},{"value":[-43.5,-23,8.507385250425152,8.589188709570884]},{"value":[-38.5,-13,8.148301063538913,2.538499062178131]},{"value":[-35,-8,8.191379945380648,3.264392749230468]},{"value":[-18,15,9.184571180082219,20]},{"value":[-8,34,8.155390839042687,2.657964170388572]},{"value":[2,49,8.248372126605375,4.224730223038625]},{"value":[2,41.5,8.738655329947909,12.48616754357163]},{"value":[29.5,31.5,8.203468261870789,3.468084958780414]},{"value":[31,30.5,8.310464181370122,5.271002285888958]},{"value":[34,32,8.540851122566872,9.153099941264937]},{"value":[37.5,56,8.490520620000401,8.305013999447905]},{"value":[51,36,8.14331395586726,2.454464616047363]},{"value":[67,25,8.285008895449879,4.842072133534929]},{"value":[72,10.5,8.394211667001009,6.682175661960011]},{"value":[72.5,19,8.238589782897661,4.059894432544024]},{"value":[72.5,11,8.394211667001009,6.682175661960011]},{"value":[72.5,7.5,8.133381937202904,2.287106752056623]},{"value":[73,11.5,8.160089584185952,2.737139610667989]},{"value":[73.5,11,8.898515868889511,15.1798715364657]},{"value":[73.5,4.5,8.677166884546285,11.45006650205251]},{"value":[77,29,8.515431640796576,8.724773100912188]},{"value":[80,13.5,8.069905588454235,1.217508868734794]},{"value":[88,23,8.50457390205986,8.541816541510268]},{"value":[90,24,8.408070050397843,6.915694095829069]},{"value":[103.5,1.5,8.146651113097302,2.510696840803996]},{"value":[106.5,-6,8.696142401569334,11.76981036323698]},{"value":[114,22.5,8.984129621612503,16.62249214242283]},{"value":[114.5,22.5,8.164254117559519,2.807313402469202]},{"value":[121,31.5,8.38160253710989,6.469707570963295]},{"value":[121,25.5,8.150410189900416,2.574038552655986]},{"value":[121.5,25.5,8.157771621809401,2.698081162908246]},{"value":[122,38,8.445568287740937,7.547552041050187]},{"value":[126.5,37.5,8.378413766212967,6.415975705961867]},{"value":[135.5,35,8.081320420886648,1.409852644784742]},{"value":[139.5,36,8.975402540779262,16.47543788724233]}],"name":"lat","type":"scatter","symbol":null,"coordinateSystem":"leaflet","symbolSize":"function(data){ return data[3];}"}]},"dispose":true},"evals":["opts.series.0.symbolSize"],"jsHooks":[]}
```
