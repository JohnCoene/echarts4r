# Modularity

Graph modularity extension will do community detection and partian a
graph's vertices in several subsets. Each subset will be assigned a
different color.

## Usage

``` r
e_modularity(e, modularity = TRUE)
```

## Arguments

- e:

  An `echarts4r` object as returned by
  [`e_charts`](https://echarts4r.john-coene.com/reference/init.md) or a
  proxy as returned by
  [`echarts4rProxy`](https://echarts4r.john-coene.com/reference/echarts4r-shiny.md).

- modularity:

  Either set to `TRUE`, or a `list`.

## Note

Does not work in RStudio viewer, open in browser.

## Modularity

- `resolution` Resolution

- `sort` Whether to sort to comunities

## See also

[Official
documentation](https://github.com/ecomfe/echarts-graph-modularity)

## Examples

``` r
nodes <- data.frame(
  name = paste0(LETTERS, 1:100),
  value = rnorm(100, 10, 2),
  stringsAsFactors = FALSE
)

edges <- data.frame(
  source = sample(nodes$name, 200, replace = TRUE),
  target = sample(nodes$name, 200, replace = TRUE),
  stringsAsFactors = FALSE
)

e_charts() |>
  e_graph() |>
  e_graph_nodes(nodes, name, value) |>
  e_graph_edges(edges, source, target) |>
  e_modularity(
    list(
      resolution = 5,
      sort = TRUE
    )
  )

{"x":{"theme":"","tl":false,"draw":true,"renderer":"canvas","events":[],"buttons":[],"settings":{"crosstalk_key":null,"crosstalk_group":null},"opts":{"series":[{"name":null,"type":"graph","layout":"force","data":[{"name":"A1","value":"12.384995"},{"name":"B2","value":" 6.834300"},{"name":"C3","value":" 9.246847"},{"name":"D4","value":" 8.248616"},{"name":"E5","value":" 9.937302"},{"name":"F6","value":"12.699953"},{"name":"G7","value":" 6.690727"},{"name":"H8","value":" 7.864292"},{"name":"I9","value":" 6.677534"},{"name":"J10","value":" 9.751655"},{"name":"K11","value":" 9.358762"},{"name":"L12","value":" 6.564255"},{"name":"M13","value":"10.261367"},{"name":"N14","value":" 8.695872"},{"name":"O15","value":"11.555366"},{"name":"P16","value":"12.481485"},{"name":"Q17","value":" 9.954030"},{"name":"R18","value":" 9.196520"},{"name":"S19","value":"10.270266"},{"name":"T20","value":" 9.297612"},{"name":"U21","value":"11.075317"},{"name":"V22","value":"13.546521"},{"name":"W23","value":"10.850943"},{"name":"X24","value":" 7.783873"},{"name":"Y25","value":" 9.490461"},{"name":"Z26","value":" 8.476990"},{"name":"A27","value":" 9.747706"},{"name":"B28","value":" 8.475001"},{"name":"C29","value":"10.335235"},{"name":"D30","value":"11.867750"},{"name":"E31","value":" 9.168583"},{"name":"F32","value":"12.636742"},{"name":"G33","value":" 4.189378"},{"name":"H34","value":" 7.960487"},{"name":"I35","value":"10.566451"},{"name":"J36","value":" 7.819119"},{"name":"K37","value":"15.686900"},{"name":"L38","value":" 6.620298"},{"name":"M39","value":"13.567702"},{"name":"N40","value":"10.306250"},{"name":"O41","value":" 8.714113"},{"name":"P42","value":" 9.223580"},{"name":"Q43","value":" 9.281707"},{"name":"R44","value":"10.857062"},{"name":"S45","value":" 9.111237"},{"name":"T46","value":" 4.972464"},{"name":"U47","value":"11.133742"},{"name":"V48","value":"12.161729"},{"name":"W49","value":" 9.602685"},{"name":"X50","value":"10.969930"},{"name":"Y51","value":" 8.305074"},{"name":"Z52","value":" 7.973274"},{"name":"A53","value":" 9.467478"},{"name":"B54","value":" 9.545749"},{"name":"C55","value":" 8.623125"},{"name":"D56","value":" 9.599845"},{"name":"E57","value":"11.585286"},{"name":"F58","value":" 9.562585"},{"name":"G59","value":" 8.917363"},{"name":"H60","value":" 6.397274"},{"name":"I61","value":"10.552981"},{"name":"J62","value":" 8.057582"},{"name":"K63","value":" 9.580804"},{"name":"L64","value":" 8.275854"},{"name":"M65","value":" 9.774032"},{"name":"N66","value":" 8.097882"},{"name":"O67","value":" 8.405927"},{"name":"P68","value":" 9.390643"},{"name":"Q69","value":"12.161240"},{"name":"R70","value":" 6.780188"},{"name":"S71","value":" 7.485094"},{"name":"T72","value":"10.269760"},{"name":"U73","value":" 9.919925"},{"name":"V74","value":"10.146094"},{"name":"W75","value":" 9.157124"},{"name":"X76","value":"11.442335"},{"name":"Y77","value":"11.291359"},{"name":"Z78","value":" 9.994804"},{"name":"A79","value":" 9.533761"},{"name":"B80","value":"13.874890"},{"name":"C81","value":"10.924533"},{"name":"D82","value":"10.492292"},{"name":"E83","value":"11.605491"},{"name":"F84","value":"10.663743"},{"name":"G85","value":" 9.279050"},{"name":"H86","value":"10.303126"},{"name":"I87","value":" 6.661270"},{"name":"J88","value":" 9.913019"},{"name":"K89","value":" 9.954196"},{"name":"L90","value":"14.583547"},{"name":"M91","value":" 9.933982"},{"name":"N92","value":" 5.939944"},{"name":"O93","value":"10.130221"},{"name":"P94","value":" 7.548837"},{"name":"Q95","value":" 8.933618"},{"name":"R96","value":" 9.412870"},{"name":"S97","value":"10.097074"},{"name":"T98","value":"11.859476"},{"name":"U99","value":"10.696288"},{"name":"V100","value":"11.099345"}],"links":[{"source":"Z26","target":"A1"},{"source":"P16","target":"U73"},{"source":"N40","target":"X50"},{"source":"O15","target":"O15"},{"source":"N14","target":"A79"},{"source":"I61","target":"Q69"},{"source":"V48","target":"X24"},{"source":"R18","target":"I35"},{"source":"U99","target":"O41"},{"source":"V48","target":"Y51"},{"source":"U47","target":"B80"},{"source":"B54","target":"J10"},{"source":"P94","target":"S71"},{"source":"H34","target":"X24"},{"source":"D30","target":"E57"},{"source":"J62","target":"J88"},{"source":"L64","target":"G85"},{"source":"R96","target":"J10"},{"source":"S45","target":"U99"},{"source":"W49","target":"U47"},{"source":"Q69","target":"F84"},{"source":"F32","target":"R70"},{"source":"L90","target":"R96"},{"source":"T98","target":"R70"},{"source":"L12","target":"S71"},{"source":"B80","target":"X50"},{"source":"S97","target":"L12"},{"source":"E5","target":"C81"},{"source":"T20","target":"U21"},{"source":"V100","target":"N92"},{"source":"A1","target":"Q69"},{"source":"H34","target":"O15"},{"source":"T46","target":"P42"},{"source":"O93","target":"U47"},{"source":"J10","target":"E31"},{"source":"Y51","target":"O67"},{"source":"N92","target":"L64"},{"source":"I9","target":"V74"},{"source":"K63","target":"D4"},{"source":"V48","target":"P68"},{"source":"D56","target":"U99"},{"source":"D82","target":"K89"},{"source":"U21","target":"O93"},{"source":"I61","target":"Z52"},{"source":"P68","target":"S71"},{"source":"G85","target":"F58"},{"source":"E57","target":"B54"},{"source":"J10","target":"I35"},{"source":"B2","target":"J88"},{"source":"J88","target":"Z78"},{"source":"W23","target":"K11"},{"source":"V48","target":"C3"},{"source":"X24","target":"K89"},{"source":"D30","target":"L90"},{"source":"P42","target":"A27"},{"source":"B80","target":"W49"},{"source":"T72","target":"E5"},{"source":"L64","target":"V100"},{"source":"Q69","target":"Q69"},{"source":"M65","target":"R70"},{"source":"G85","target":"L64"},{"source":"S19","target":"W49"},{"source":"T20","target":"X76"},{"source":"L64","target":"G59"},{"source":"U73","target":"K89"},{"source":"H86","target":"O93"},{"source":"K11","target":"F32"},{"source":"H86","target":"Y77"},{"source":"J36","target":"U21"},{"source":"H86","target":"P94"},{"source":"Q17","target":"K37"},{"source":"D30","target":"Y51"},{"source":"K89","target":"H86"},{"source":"G59","target":"L64"},{"source":"L90","target":"L64"},{"source":"G7","target":"E57"},{"source":"S45","target":"M13"},{"source":"V100","target":"I35"},{"source":"J88","target":"I61"},{"source":"U99","target":"D56"},{"source":"S19","target":"D30"},{"source":"U99","target":"C29"},{"source":"X24","target":"W49"},{"source":"O93","target":"O15"},{"source":"D30","target":"S45"},{"source":"Z26","target":"V74"},{"source":"N40","target":"M13"},{"source":"I35","target":"R44"},{"source":"I61","target":"L38"},{"source":"A1","target":"R44"},{"source":"B80","target":"I9"},{"source":"Z26","target":"C55"},{"source":"C81","target":"Z26"},{"source":"C81","target":"H8"},{"source":"I9","target":"K89"},{"source":"H8","target":"A27"},{"source":"R70","target":"D30"},{"source":"W49","target":"M65"},{"source":"L12","target":"B54"},{"source":"E83","target":"L64"},{"source":"B80","target":"K89"},{"source":"H86","target":"M91"},{"source":"U99","target":"A27"},{"source":"U99","target":"F6"},{"source":"J36","target":"W49"},{"source":"V100","target":"G59"},{"source":"A1","target":"S19"},{"source":"V22","target":"T72"},{"source":"G85","target":"R96"},{"source":"V100","target":"J10"},{"source":"K63","target":"L38"},{"source":"I87","target":"E83"},{"source":"L38","target":"G59"},{"source":"H86","target":"L12"},{"source":"X24","target":"V48"},{"source":"Z78","target":"S45"},{"source":"Q17","target":"B80"},{"source":"F6","target":"C81"},{"source":"M91","target":"H34"},{"source":"E31","target":"K11"},{"source":"T72","target":"Y51"},{"source":"X24","target":"H86"},{"source":"G85","target":"C55"},{"source":"Q95","target":"I61"},{"source":"D82","target":"V74"},{"source":"T46","target":"U73"},{"source":"F6","target":"S19"},{"source":"H34","target":"E57"},{"source":"D4","target":"L64"},{"source":"J62","target":"Y51"},{"source":"U47","target":"D4"},{"source":"S19","target":"L38"},{"source":"Z78","target":"K11"},{"source":"M39","target":"H86"},{"source":"T20","target":"B2"},{"source":"G59","target":"M13"},{"source":"O15","target":"D30"},{"source":"C29","target":"X76"},{"source":"V48","target":"I35"},{"source":"V48","target":"B54"},{"source":"S19","target":"E31"},{"source":"U73","target":"C29"},{"source":"R18","target":"H34"},{"source":"H86","target":"E31"},{"source":"F32","target":"E57"},{"source":"F58","target":"B54"},{"source":"I87","target":"V74"},{"source":"X24","target":"C3"},{"source":"G59","target":"G59"},{"source":"H60","target":"S45"},{"source":"A1","target":"I87"},{"source":"G85","target":"L90"},{"source":"N14","target":"I35"},{"source":"W23","target":"X50"},{"source":"T98","target":"J62"},{"source":"K89","target":"Z26"},{"source":"K37","target":"L12"},{"source":"Q43","target":"N14"},{"source":"X50","target":"M13"},{"source":"E5","target":"R70"},{"source":"W23","target":"F6"},{"source":"T46","target":"Y25"},{"source":"Q95","target":"O41"},{"source":"K89","target":"L38"},{"source":"D82","target":"O67"},{"source":"I87","target":"T72"},{"source":"Z52","target":"L38"},{"source":"Z78","target":"K63"},{"source":"S97","target":"V22"},{"source":"N40","target":"F58"},{"source":"U47","target":"O67"},{"source":"G59","target":"X24"},{"source":"D56","target":"P42"},{"source":"I35","target":"N92"},{"source":"Q95","target":"H60"},{"source":"K63","target":"N92"},{"source":"O15","target":"V22"},{"source":"Q43","target":"S97"},{"source":"J36","target":"U73"},{"source":"C81","target":"S45"},{"source":"D4","target":"U99"},{"source":"E5","target":"U21"},{"source":"W49","target":"K89"},{"source":"O67","target":"O41"},{"source":"Z78","target":"A79"},{"source":"B28","target":"I61"},{"source":"H86","target":"A27"},{"source":"N66","target":"F58"},{"source":"M39","target":"S71"},{"source":"H86","target":"M65"},{"source":"J10","target":"Q17"},{"source":"A79","target":"W75"},{"source":"C55","target":"D4"},{"source":"A53","target":"V22"},{"source":"R44","target":"U73"},{"source":"U99","target":"P16"},{"source":"I35","target":"R70"},{"source":"I61","target":"R18"},{"source":"P16","target":"F58"},{"source":"M91","target":"I61"}],"modularity":{"modularity":{"resolution":5,"sort":true}}}]},"dispose":true},"evals":[],"jsHooks":[]}
```
