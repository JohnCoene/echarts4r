# echarts4r

[![Travis-CI Build Status](https://travis-ci.org/JohnCoene/echarts4r.svg?branch=master)](https://travis-ci.org/JohnCoene/echarts4r) [![AppVeyor Build Status](https://ci.appveyor.com/api/projects/status/github/JohnCoene/echarts4r?branch=master&svg=true)](https://ci.appveyor.com/project/JohnCoene/echarts4r) [![lifecycle](https://img.shields.io/badge/lifecycle-maturing-blue.svg)](https://www.tidyverse.org/lifecycle/#maturing) [![GitHub closed issues](https://img.shields.io/github/issues-closed/JohnCoene/echarts4r.svg)](https://github.com/JohnCoene/echarts4r/issues) [![code-size](https://img.shields.io/github/languages/code-size/JohnCoene/echarts4r.svg)](https://github.com/JohnCoene/echarts4r) [![activity](https://img.shields.io/github/last-commit/JohnCoene/echarts4r.svg)](https://github.com/JohnCoene/echarts4r) 

<div class = "row">

<div class = "col-md-4">
<img class = "img-responsive responsive-img" src="reference/figures/logo.png">
</div>

<div class = "col-md-8">
<p>Interactive visualisations for R</p>
<a class = "btn btn-success" href = "articles/get_started.html" style = "margin-bottom: 5px;">Get Started</a>
<a class = "btn btn-primary" href = "reference/" style = "margin-bottom: 5px;">Reference</a>
<a class = "btn btn-info" href = "articles/timeline" style = "margin-bottom: 5px;">Timeline</a>
<a class = "btn btn-default" href = "http://shiny.john-coene.com/echarts4rShiny" style = "margin-bottom: 5px;">Shiny Demo</a>
</div>

</div>

<br/><br/>

<div class = "thumbnail" style = "text-align:center;">
<div class = "caption">
<i class = "fa fa-flask fa-4x" style = "color:#293c55;"></i>
<h4>New Features</h4>
<p>"Use (almost) any chart serie as chart proxy, now on CRAN!"</p>
<br/>
<a class = "btn btn-warning" href = "articles/development">Explore</a>
<a class = "btn btn-default" href = "news/">Changelog</a>
</div>
</div>

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
<pre class = "sourceCode r">
<code class = "sourceCode r">
remotes::install_github('JohnCoene/echarts4r.assets')
</code>
</pre>
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
<pre class = "sourceCode r">
<code class = "sourceCode r">
remotes::install_github('JohnCoene/echarts4r.maps')
</code>
</pre>
<a class = "btn btn-default" href = "https://echarts4r-maps.john-coene.com/">Website</a>
<a class = "btn btn-default" href = "https://github.com/JohnCoene/echarts4r.maps">Github</a>
</div>
</div>
</div>

</div>
