# echarts4r

[![Travis-CI Build Status](https://travis-ci.org/JohnCoene/echarts4r.svg?branch=master)](https://travis-ci.org/JohnCoene/echarts4r) [![AppVeyor Build Status](https://ci.appveyor.com/api/projects/status/github/JohnCoene/echarts4r?branch=master&svg=true)](https://ci.appveyor.com/project/JohnCoene/echarts4r) [![Lifecycle: stable](https://img.shields.io/badge/lifecycle-stable-brightgreen.svg)](https://www.tidyverse.org/lifecycle/#stable) [![GitHub closed issues](https://img.shields.io/github/issues-closed/JohnCoene/echarts4r.svg)](https://github.com/JohnCoene/echarts4r/issues) [![code-size](https://img.shields.io/github/languages/code-size/JohnCoene/echarts4r.svg)](https://github.com/JohnCoene/echarts4r) [![activity](https://img.shields.io/github/last-commit/JohnCoene/echarts4r.svg)](https://github.com/JohnCoene/echarts4r) [![Coveralls test coverage](https://coveralls.io/repos/github/JohnCoene/echarts4r/badge.svg)](https://coveralls.io/github/JohnCoene/echarts4r)

<div class = "row">

<div class = "col-md-4">
<img class = "img-responsive responsive-img" src="reference/figures/logo.png">
</div>

<div class = "col-md-8">
<p>Interactive visualisations for R via <a href="https://echarts.apache.org/">Apache ECharts</a></p>
<a class = "btn btn-success" href = "articles/get_started.html" style = "margin-bottom: 5px;">Get Started</a>
<a class = "btn btn-primary" href = "reference/" style = "margin-bottom: 5px;">Reference</a>
<a class = "btn btn-info" href = "articles/timeline" style = "margin-bottom: 5px;">Timeline</a>
<a class = "btn btn-default" href = "http://shiny.john-coene.com/echarts4rShiny" style = "margin-bottom: 5px;">Shiny Demo</a>
</div>

</div>

<br/><br/>

<div class = "thumbnail" style = "text-align:center;">
<div class = "caption">
<i class = "fa fa-code-branch fa-4x" style = "color:#F72C5B;"></i>
<h4>Version 5</h4>
<p>Explore new features available on version 5 of echarts.js!</p>
<br/>
<a class = "btn btn-default" href = "articles/v5">Explore</a>
</div>
</div>

## Note

The Github version is currently going through the update from ECharts.js version 4 to version 5: feel free to test it but there are some issues. Please use the stable CRAN version otherwise.

## Introduction

Thanks to [Sharon Machlis](https://twitter.com/sharon000) there is an amazing video and [article](https://www.infoworld.com/article/3607068/plot-in-r-with-echarts4r.html) introducing echarts4r.

<iframe width="560" height="315" src="https://www.youtube.com/embed/OBJxIWEFHdo" frameborder="0" allow="accelerometer; autoplay; clipboard-write; encrypted-media; gyroscope; picture-in-picture" allowfullscreen></iframe>

## Installation

The package is available on [CRAN](https://CRAN.R-project.org/package=echarts4r). The full installation can be obtained with:

```r
install.packages("echarts4r")
```

However, if you only want a _lite_ version you can simply do, this is useful for a lighter version that installs faster if you do not want to use any of the geospatial features of the package:

```r
install.packages("echarts4r", dependencies = c("Depends", "Imports"))
```

You can also install the _unstable_ development version of echarts4r with `remotes` from Github, see [changes](news/index.html).

```r
# install.packages("remotes")
remotes::install_github("JohnCoene/echarts4r")
```

如果您位于中国，请安装:

```r
# install.packages("remotes")
remotes::install_git("https://gitee.com/JohnCoene/echarts4r")
```

## Companions

Companion packages to make `echarts4r` even better. You can install and load the whole suite with:

```r
remotes::install_github("JohnCoene/echarts4r.suite")
```

<div class = "row">

<div class = "col-md-6">

<div class = "thumbnail" style = "text-align:center;">
<div class = "caption">
<i class = "fa fa-globe fa-5x" style = "color:#293c55;"></i>
<h4>echarts4r.assets</h4>
<p>Icons, and assets for globes, add visually interesting globe overlays and background.</p>
<pre class = "sourceCode r"><code class = "sourceCode r">remotes::install_github('JohnCoene/echarts4r.assets')</code></pre>
<a class = "btn btn-default" href = "https://echarts4r-assets.john-coene.com/">Website</a>
<a class = "btn btn-default" href = "https://github.com/JohnCoene/echarts4r.assets">Github</a>
</div>
</div>

</div>

<div class = "col-md-6">
<div class = "thumbnail" style = "text-align:center;">
<div class = "caption">
<i class = "fa fa-map-o fa-5x" style = "color:#293c55;"></i>
<h4>echarts4r.maps</h4>
<p>A collection of 215 country maps to use with geo-spatial visualisations.</p>
<pre class = "sourceCode r"><code class = "sourceCode r">remotes::install_github('JohnCoene/echarts4r.maps')</code></pre>
<a class = "btn btn-default" href = "https://echarts4r-maps.john-coene.com/">Website</a>
<a class = "btn btn-default" href = "https://github.com/JohnCoene/echarts4r.maps">Github</a>
</div>
</div>
</div>

</div>
