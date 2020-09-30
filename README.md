
<div align="center">

[![](echarts4r-banner.png)](https://xscode.com/johncoene/echarts4r)

[![Travis-CI Build Status](https://travis-ci.org/JohnCoene/echarts4r.svg?branch=master)](https://travis-ci.org/JohnCoene/echarts4r) [![AppVeyor Build Status](https://ci.appveyor.com/api/projects/status/github/JohnCoene/echarts4r?branch=master&svg=true)](https://ci.appveyor.com/project/JohnCoene/echarts4r) [![lifecycle](https://img.shields.io/badge/lifecycle-maturing-blue.svg)](https://www.tidyverse.org/lifecycle/#maturing) [![version](https://img.shields.io/github/tag/JohnCoene/echarts4r.svg)](https://github.com/JohnCoene/echarts4r/releases) [![GitHub closed issues](https://img.shields.io/github/issues-closed/JohnCoene/echarts4r.svg)](https://github.com/JohnCoene/echarts4r/issues) [![echarts4r](https://cranlogs.r-pkg.org/badges/echarts4r)](https://cranlogs.r-pkg.org/badges/echarts4r) [![code-size](https://img.shields.io/github/languages/code-size/JohnCoene/echarts4r.svg)](https://github.com/JohnCoene/echarts4r)

[Apache ECharts (incubating)](https://echarts.apache.org/) version 4 for R.

[Website](https://echarts4r.john-coene.com) |
[Shiny Demo](https://shiny.john-coene.com/echarts4rShiny/) | [Get
Started](https://echarts4r.john-coene.com/articles/get_started.html)

</div>

## Example

Basic example.

```r
library(echarts4r)

e_chart(cars, speed) %>% 
  e_scatter(dist, symbolSize = 10)
```

![](./man/figures/ex.png)

## Get it

Stable version from CRAN.

```r
install.packages("echarts4r")
```

The development version from Github.

```r
# install.packages("remotes")
remotes::install_github("JohnCoene/echarts4r")
```

## Code of Conduct

Please note that the echarts4r project is released with a [Contributor Code of Conduct](https:/github.com/JohnCoene/echarts4r/CODE_OF_CONDUCT.html). By contributing to this project, you agree to abide by its terms.
