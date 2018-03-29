# echarts4r

[![Travis-CI Build Status](https://travis-ci.org/JohnCoene/echarts4r.svg?branch=master)](https://travis-ci.org/JohnCoene/echarts4r)

ECharts 4 for R.

[echarts](http://john-coene.com/img/echarts4r.png)

* [EN](https://ecomfe.github.io/)
* [ZH](http://echarts.baidu.com/index.html)

## Installation

You can install echarts4r from github with:

```{r gh-installation, eval = FALSE}
# install.packages("devtools")
devtools::install_github("JohnCoene/echarts4r")
```

## Features

* Bar
* Line
* Area
* Step
* Scatter
* Effect Scatter
* Candlestick
* Funnel
* Sankey
* Graph
* Heatmap
* Parallel
* Pie
* Tree
* Treemap
* River
* Sunburst
* Boxplot
* Calendar
* Gauge
* Line 3D
* Bar 3D
* Scatter 3D
* Flow GL
* Graph GL
* Map 3D
* Geo 3D
* Globe

## Examples

```r
library(echarts4r)

#' ol' mtcars example
mtcars %>%
  e_charts(mpg) %>% 
  e_line(drat)

# with negative
USArrests %>% 
  dplyr::mutate(
    State = row.names(.),
    Rape = -Rape
  ) %>% 
  e_charts(State) %>% 
  e_area(Murder) %>%
  e_bar(Rape, name = "Sick basterd", x.index = 1) # second y axis

# Sankey
sankey <- data.frame(
  source = c("a", "b", "c", "d", "c"),
  target = c("b", "c", "d", "e", "e"),
  value = ceiling(rnorm(5, 10, 1)),
  stringsAsFactors = FALSE
)
 
sankey %>%
  e_charts() %>% 
  e_sankey(source, target, value) 

# Use graphGL for larger networks
nodes <- data.frame(
  name = paste0(LETTERS, 1:1000),
  value = rnorm(1000, 10, 2),
  size = rnorm(1000, 10, 2),
  grp = rep(c("grp1", "grp2"), 500),
  stringsAsFactors = FALSE
)
 
edges <- data.frame(
  source = sample(nodes$name, 2000, replace = TRUE),
  target = sample(nodes$name, 2000, replace = TRUE),
  stringsAsFactors = FALSE
)
 
e_charts() %>%
  e_graph_gl() %>% 
  e_graph_nodes(nodes, name, value, size, grp) %>% 
  e_graph_edges(edges, source, target)
```
