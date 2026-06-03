# Correlation

Correlation

## Usage

``` r
e_correlations(e, order = NULL, visual_map = TRUE, ...)
```

## Arguments

- e:

  An `echarts4r` object as returned by
  [`e_charts`](https://echarts4r.john-coene.com/reference/init.md) or a
  proxy as returned by
  [`echarts4rProxy`](https://echarts4r.john-coene.com/reference/echarts4r-shiny.md).

- order:

  Ordering method, passed to
  [corrMatOrder](https://rdrr.io/pkg/corrplot/man/corrMatOrder.html).

- visual_map:

  Whether to add the visual map.

- ...:

  Any argument to pass to
  [`e_heatmap`](https://echarts4r.john-coene.com/reference/e_heatmap.md)
  and
  [`e_visual_map`](https://echarts4r.john-coene.com/reference/e_visual_map.md).

## Examples

``` r
cor(mtcars) |>
  e_charts() |>
  e_correlations(
    order = "hclust",
    visual_map = FALSE
  ) |>
  e_visual_map(
    min = -1,
    max = 1
  )

{"x":{"theme":"","tl":false,"draw":true,"renderer":"canvas","events":[],"buttons":[],"settings":{"crosstalk_key":null,"crosstalk_group":null},"opts":{"yAxis":[{"data":["carb","wt","hp","cyl","disp","qsec","vs","mpg","drat","am","gear"]}],"xAxis":[{"data":["carb","wt","hp","cyl","disp","qsec","vs","mpg","drat","am","gear"]}],"series":[{"data":[{"value":["carb","carb"," 1.00000000"]},{"value":["wt","carb"," 0.42760594"]},{"value":["hp","carb"," 0.74981247"]},{"value":["cyl","carb"," 0.52698829"]},{"value":["disp","carb"," 0.39497686"]},{"value":["qsec","carb","-0.65624923"]},{"value":["vs","carb","-0.56960714"]},{"value":["mpg","carb","-0.55092507"]},{"value":["drat","carb","-0.09078980"]},{"value":["am","carb"," 0.05753435"]},{"value":["gear","carb"," 0.27407284"]},{"value":["carb","wt"," 0.42760594"]},{"value":["wt","wt"," 1.00000000"]},{"value":["hp","wt"," 0.65874789"]},{"value":["cyl","wt"," 0.78249579"]},{"value":["disp","wt"," 0.88797992"]},{"value":["qsec","wt","-0.17471588"]},{"value":["vs","wt","-0.55491568"]},{"value":["mpg","wt","-0.86765938"]},{"value":["drat","wt","-0.71244065"]},{"value":["am","wt","-0.69249526"]},{"value":["gear","wt","-0.58328700"]},{"value":["carb","hp"," 0.74981247"]},{"value":["wt","hp"," 0.65874789"]},{"value":["hp","hp"," 1.00000000"]},{"value":["cyl","hp"," 0.83244745"]},{"value":["disp","hp"," 0.79094859"]},{"value":["qsec","hp","-0.70822339"]},{"value":["vs","hp","-0.72309674"]},{"value":["mpg","hp","-0.77616837"]},{"value":["drat","hp","-0.44875912"]},{"value":["am","hp","-0.24320426"]},{"value":["gear","hp","-0.12570426"]},{"value":["carb","cyl"," 0.52698829"]},{"value":["wt","cyl"," 0.78249579"]},{"value":["hp","cyl"," 0.83244745"]},{"value":["cyl","cyl"," 1.00000000"]},{"value":["disp","cyl"," 0.90203287"]},{"value":["qsec","cyl","-0.59124207"]},{"value":["vs","cyl","-0.81081180"]},{"value":["mpg","cyl","-0.85216196"]},{"value":["drat","cyl","-0.69993811"]},{"value":["am","cyl","-0.52260705"]},{"value":["gear","cyl","-0.49268660"]},{"value":["carb","disp"," 0.39497686"]},{"value":["wt","disp"," 0.88797992"]},{"value":["hp","disp"," 0.79094859"]},{"value":["cyl","disp"," 0.90203287"]},{"value":["disp","disp"," 1.00000000"]},{"value":["qsec","disp","-0.43369788"]},{"value":["vs","disp","-0.71041589"]},{"value":["mpg","disp","-0.84755138"]},{"value":["drat","disp","-0.71021393"]},{"value":["am","disp","-0.59122704"]},{"value":["gear","disp","-0.55556920"]},{"value":["carb","qsec","-0.65624923"]},{"value":["wt","qsec","-0.17471588"]},{"value":["hp","qsec","-0.70822339"]},{"value":["cyl","qsec","-0.59124207"]},{"value":["disp","qsec","-0.43369788"]},{"value":["qsec","qsec"," 1.00000000"]},{"value":["vs","qsec"," 0.74453544"]},{"value":["mpg","qsec"," 0.41868403"]},{"value":["drat","qsec"," 0.09120476"]},{"value":["am","qsec","-0.22986086"]},{"value":["gear","qsec","-0.21268223"]},{"value":["carb","vs","-0.56960714"]},{"value":["wt","vs","-0.55491568"]},{"value":["hp","vs","-0.72309674"]},{"value":["cyl","vs","-0.81081180"]},{"value":["disp","vs","-0.71041589"]},{"value":["qsec","vs"," 0.74453544"]},{"value":["vs","vs"," 1.00000000"]},{"value":["mpg","vs"," 0.66403892"]},{"value":["drat","vs"," 0.44027846"]},{"value":["am","vs"," 0.16834512"]},{"value":["gear","vs"," 0.20602335"]},{"value":["carb","mpg","-0.55092507"]},{"value":["wt","mpg","-0.86765938"]},{"value":["hp","mpg","-0.77616837"]},{"value":["cyl","mpg","-0.85216196"]},{"value":["disp","mpg","-0.84755138"]},{"value":["qsec","mpg"," 0.41868403"]},{"value":["vs","mpg"," 0.66403892"]},{"value":["mpg","mpg"," 1.00000000"]},{"value":["drat","mpg"," 0.68117191"]},{"value":["am","mpg"," 0.59983243"]},{"value":["gear","mpg"," 0.48028476"]},{"value":["carb","drat","-0.09078980"]},{"value":["wt","drat","-0.71244065"]},{"value":["hp","drat","-0.44875912"]},{"value":["cyl","drat","-0.69993811"]},{"value":["disp","drat","-0.71021393"]},{"value":["qsec","drat"," 0.09120476"]},{"value":["vs","drat"," 0.44027846"]},{"value":["mpg","drat"," 0.68117191"]},{"value":["drat","drat"," 1.00000000"]},{"value":["am","drat"," 0.71271113"]},{"value":["gear","drat"," 0.69961013"]},{"value":["carb","am"," 0.05753435"]},{"value":["wt","am","-0.69249526"]},{"value":["hp","am","-0.24320426"]},{"value":["cyl","am","-0.52260705"]},{"value":["disp","am","-0.59122704"]},{"value":["qsec","am","-0.22986086"]},{"value":["vs","am"," 0.16834512"]},{"value":["mpg","am"," 0.59983243"]},{"value":["drat","am"," 0.71271113"]},{"value":["am","am"," 1.00000000"]},{"value":["gear","am"," 0.79405876"]},{"value":["carb","gear"," 0.27407284"]},{"value":["wt","gear","-0.58328700"]},{"value":["hp","gear","-0.12570426"]},{"value":["cyl","gear","-0.49268660"]},{"value":["disp","gear","-0.55556920"]},{"value":["qsec","gear","-0.21268223"]},{"value":["vs","gear"," 0.20602335"]},{"value":["mpg","gear"," 0.48028476"]},{"value":["drat","gear"," 0.69961013"]},{"value":["am","gear"," 0.79405876"]},{"value":["gear","gear"," 1.00000000"]}],"name":null,"type":"heatmap","coordinateSystem":"cartesian2d"}],"visualMap":[{"min":-1,"max":1,"calculable":true,"type":"continuous"}]},"dispose":true},"evals":[],"jsHooks":[]}
```
