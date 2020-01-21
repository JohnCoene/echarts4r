
<div align="center">

<img src="https://raw.githubusercontent.com/JohnCoene/echarts4r/master/docs/logo.png" height = "200px" />

<br />
<br />

[![Travis-CI Build Status](https://travis-ci.org/JohnCoene/echarts4r.svg?branch=master)](https://travis-ci.org/JohnCoene/echarts4r) [![AppVeyor Build Status](https://ci.appveyor.com/api/projects/status/github/JohnCoene/echarts4r?branch=master&svg=true)](https://ci.appveyor.com/project/JohnCoene/echarts4r) [![lifecycle](https://img.shields.io/badge/lifecycle-maturing-blue.svg)](https://www.tidyverse.org/lifecycle/#maturing) [![version](https://img.shields.io/github/tag/JohnCoene/echarts4r.svg)](https://github.com/JohnCoene/echarts4r/releases) [![GitHub closed issues](https://img.shields.io/github/issues-closed/JohnCoene/echarts4r.svg)](https://github.com/JohnCoene/echarts4r/issues) [![echarts4r](https://cranlogs.r-pkg.org/badges/echarts4r)](https://cranlogs.r-pkg.org/badges/echarts4r) [![code-size](https://img.shields.io/github/languages/code-size/JohnCoene/echarts4r.svg)](https://github.com/JohnCoene/echarts4r)

ECharts 4 for R.

[Website](https://echarts4r.john-coene.com) |
[Shiny Demo](https://shiny.john-coene.com/echarts4rShiny/) | [Get
Started](https://echarts4r.john-coene.com/articles/get_started.html)

</div>

```r
library(echarts4r)

e_chart(cars, speed) %>% 
  e_scatter(dist, symbolSize = 10)
```

![](./man/figures/ex.png)

Get it with `install.packages("echarts4r")`.
