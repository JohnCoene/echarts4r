
<div align="center">

<img src="https://raw.githubusercontent.com/JohnCoene/echarts4r/master/docs/logo.png" height = "200px" />

<br />
<br />

[![R-CMD-check](https://github.com/JohnCoene/echarts4r/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/JohnCoene/echarts4r/actions/workflows/R-CMD-check.yaml) [![version](https://img.shields.io/github/tag/JohnCoene/echarts4r.svg)](https://github.com/JohnCoene/echarts4r/releases) [![GitHub closed issues](https://img.shields.io/github/issues-closed/JohnCoene/echarts4r.svg)](https://github.com/JohnCoene/echarts4r/issues) [![echarts4r](https://cranlogs.r-pkg.org/badges/echarts4r)](https://cranlogs.r-pkg.org/badges/echarts4r) [![code-size](https://img.shields.io/github/languages/code-size/JohnCoene/echarts4r.svg)](https://github.com/JohnCoene/echarts4r) [![Coveralls test coverage](https://coveralls.io/repos/github/JohnCoene/echarts4r/badge.svg)](https://coveralls.io/github/JohnCoene/echarts4r)

[Apache ECharts](https://echarts.apache.org/en/) version __5__ for R.

[Website](https://echarts4r.john-coene.com) |
[Shiny Demo](https://shiny.john-coene.com/echarts4rShiny/) | [Get
Started](https://echarts4r.john-coene.com/articles/get_started.html)

</div>

## Note

The Github version is currently going through the update from ECharts.js version 4 to version 5: feel free to test it but there are some issues. Please use the stable CRAN version otherwise.

## Example

Basic example.

```r
library(echarts4r)

e_chart(cars, speed) %>% 
  e_scatter(dist, symbolSize = 10)
```

![](./man/figures/ex1.png)

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

Please note that the echarts4r project is released with a [Contributor Code of Conduct](https://github.com/JohnCoene/echarts4r/blob/master/CODE_OF_CONDUCT.md). By contributing to this project, you agree to abide by its terms.


