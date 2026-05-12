# echarts4r

[![R-CMD-check](https://github.com/JohnCoene/echarts4r/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/JohnCoene/echarts4r/actions/workflows/R-CMD-check.yaml)
[![Lifecycle:
stable](https://img.shields.io/badge/lifecycle-stable-brightgreen.svg)](https://www.tidyverse.org/lifecycle/#stable)
[![GitHub closed
issues](https://img.shields.io/github/issues-closed/JohnCoene/echarts4r.svg)](https://github.com/JohnCoene/echarts4r/issues)
[![code-size](https://img.shields.io/github/languages/code-size/JohnCoene/echarts4r.svg)](https://github.com/JohnCoene/echarts4r)
[![activity](https://img.shields.io/github/last-commit/JohnCoene/echarts4r.svg)](https://github.com/JohnCoene/echarts4r)
[![Coveralls test
coverage](https://coveralls.io/repos/github/JohnCoene/echarts4r/badge.svg)](https://coveralls.io/github/JohnCoene/echarts4r)

![](reference/figures/logo.png)

Interactive visualisations for R via [Apache
ECharts](https://echarts.apache.org/)

[Get Started](https://echarts4r.john-coene.com/articles/get_started.md)
[Reference](https://echarts4r.john-coene.com/reference/)
[Timeline](https://echarts4r.john-coene.com/articles/timeline) [Shiny
Demo](http://shiny.john-coene.com/echarts4rShiny)

  
  

#### Version 6

Explore new features available on version 6 of echarts.js!

  
[Explore](https://echarts4r.john-coene.com/articles/v6)

## Introduction

Thanks to [Sharon Machlis](https://twitter.com/sharon000) there is an
amazing video and
[article](https://www.infoworld.com/article/3607068/plot-in-r-with-echarts4r.html)
introducing echarts4r.

  

You can learn how to build such R packages for interactive
visualisations with the book [JavaScript for
R](https://javascript-for-r.com/).

## Installation

The package is available on
[CRAN](https://CRAN.R-project.org/package=echarts4r). The full
installation can be obtained with:

``` r
install.packages("echarts4r")
```

However, if you only want a *lite* version you can simply do, this is
useful for a lighter version that installs faster if you do not want to
use any of the geospatial features of the package:

``` r
install.packages("echarts4r", dependencies = c("Depends", "Imports"))
```

You can also install the *unstable* development version of echarts4r
with `remotes` from Github, see
[changes](https://echarts4r.john-coene.com/news/index.md).

``` r
# install.packages("remotes")
remotes::install_github("JohnCoene/echarts4r")
```

如果您位于中国，请安装:

``` r
# install.packages("remotes")
remotes::install_git("https://gitee.com/JohnCoene/echarts4r")
```

## Companions

Companion packages to make `echarts4r` even better. You can install and
load the whole suite with:

``` r
remotes::install_github("JohnCoene/echarts4r.suite")
```

#### echarts4r.assets

Icons, and assets for globes, add visually interesting globe overlays
and background.

``` r
remotes::install_github('JohnCoene/echarts4r.assets')
```

[Website](https://echarts4r-assets.john-coene.com/)
[Github](https://github.com/JohnCoene/echarts4r.assets)

#### echarts4r.maps

A collection of 215 country maps to use with geo-spatial visualisations.

``` r
remotes::install_github('JohnCoene/echarts4r.maps')
```

[Website](https://echarts4r-maps.john-coene.com/)
[Github](https://github.com/JohnCoene/echarts4r.maps)
