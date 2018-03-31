# echarts4r

[![Travis-CI Build Status](https://travis-ci.org/JohnCoene/echarts4r.svg?branch=master)](https://travis-ci.org/JohnCoene/echarts4r)

![echarts](http://john-coene.com/img/echarts4r.png)

ECharts 4 for R.

* [EN](https://ecomfe.github.io/echarts-doc/public/en/index.html)
* [ZH](http://echarts.baidu.com/index.html)

## Installation

You can install echarts4r from github with:

```r
# install.packages("devtools")
devtools::install_github("JohnCoene/echarts4r")
```

## Features

#### Chart types

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
* River (streamgraph)
* Sunburst
* Boxplot
* Calendar
* Gauge
* Polar 
* wordcloud
* liquidFill
* Line 3D
* Bar 3D
* Scatter 3D
* Flow GL
* Graph GL
* Map 3D
* Geo 3D
* Globe 3D

#### Other

* Proxies (`*_p`)
* Themes
* Animations

## Examples

Some examples

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
  e_bar(Rape, name = "Sick basterd", x.index = 1) %>% # second y axis 
  e_mark_line("Sick basterd", data = list(type = "average")) %>% 
  e_mark_point("Murder", data = list(type = "max"))

# Sankey
sankey <- data.frame(
  source = c("a", "b", "c", "d", "c"),
  target = c("b", "c", "d", "e", "e"),
  value = ceiling(rnorm(5, 10, 1)),
  stringsAsFactors = FALSE
)
 
sankey %>%
  e_charts() %>% 
  e_sankey(source, target, value) %>% 
  e_title("Sankey") %>% 
  e_theme("dark")

# Graph
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
  
# globe
url <- paste0("https://ecomfe.github.io/echarts-examples/",
              "public/data-gl/asset/data/population.json")
data <- jsonlite::fromJSON(url)
data <- as.data.frame(data)
names(data) <- c("lon", "lat", "value")

data %>% 
  e_charts(lon) %>% 
  e_globe(
    environment = e_stars_texture(),
    base.texture = e_globe_texture()
  ) %>% 
  e_bar_3d(lat, value, coord.system = "globe") %>% 
  e_visual_map()
  
# Flow GL
# coordinates
vectors <- expand.grid(0:9, 0:9)
names(vectors) <- c("x", "y")
vectors$sx <- rnorm(100)
vectors$sy <- rnorm(100)
vectors$color <- log10(runif(100, 1, 10))

vectors %>% 
  e_charts(x) %>% 
  e_flow_gl(y, sx, sy, color) %>% 
  e_visual_map(
    min = 0, max = 1, # log 10
    dimension = 4, # x = 0, y = 1, sx = 3, sy = 4
    show = FALSE, # hide
    inRange = list(
      color = c('#313695', '#4575b4', '#74add1', '#abd9e9', '#e0f3f8',
                '#ffffbf', '#fee090', '#fdae61', '#f46d43', '#d73027', '#a50026')
    )
  ) %>% 
  e_x_axis(
    splitLine = list(show = FALSE)
  ) %>% 
  e_y_axis(
    splitLine = list(show = FALSE)
  ) 
```
