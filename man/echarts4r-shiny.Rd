% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/echarts4r.R
\name{echarts4r-shiny}
\alias{echarts4r-shiny}
\alias{echarts4rOutput}
\alias{renderEcharts4r}
\alias{echarts4rProxy}
\alias{echarts4r_proxy}
\title{Shiny bindings for echarts4r}
\usage{
echarts4rOutput(outputId, width = "100\%", height = "400px")

renderEcharts4r(expr, env = parent.frame(), quoted = FALSE)

echarts4rProxy(
  id,
  data,
  x,
  timeline = FALSE,
  session = shiny::getDefaultReactiveDomain(),
  reorder = TRUE
)

echarts4r_proxy(
  id,
  data,
  x,
  timeline = FALSE,
  session = shiny::getDefaultReactiveDomain(),
  reorder = TRUE
)
}
\arguments{
\item{outputId}{output variable to read from.}

\item{width, height}{Must be a valid CSS unit (like \code{'100\%'},
\code{'400px'}, \code{'auto'}) or a number, which will be coerced to a
string and have \code{'px'} appended.}

\item{expr}{An expression that generates a echarts4r}

\item{env}{The environment in which to evaluate \code{expr}.}

\item{quoted}{Is \code{expr} a quoted expression (with \code{quote()})? This
is useful if you want to save an expression in a variable.}

\item{id}{Target chart id.}

\item{data}{A \code{data.frame}.}

\item{x}{Column name containing x axis.}

\item{timeline}{Set to \code{TRUE} to build a timeline, see timeline section.}

\item{session}{Shiny session.}

\item{reorder}{Set to \code{FALSE} to not reorder numeric x axis values.}
}
\description{
Output and render functions for using echarts4r within Shiny
applications and interactive Rmd documents.
}
\section{Details}{
 The chart is created inside a parent \code{'<div>'}
  element, the dimensions of which are controlled by the \code{'width'} and
  \code{'height'} arguments. When these dimensions are small, it is possible
  that the chart \code{'grid'} resizes to a size larger than the parent,
  which might result in unexpected size given the input arguments. To disable
  this automatic readjustment, define a static \code{\link{e_grid}} like the
  following: \code{'e_grid(e = current_chart, top = 0, left = 20, right = 0,
  bottom = 20)'}.
}

\section{Callbacks}{

\itemize{
  \item{\code{id_brush}: returns data on brushed data points.}
  \item{\code{id_legend_change}: returns series name of legend selected/unselected.}
  \item{\code{id_clicked_data}: returns data of clicked data point.}
  \item{\code{id_clicked_data_value}: returns value of clicked data point.}
  \item{\code{id_clicked_row}: returns row number of clicked data point.}
  \item{\code{id_clicked_serie}: returns name of serie of clicked data point.}
  \item{\code{id_mouseover_data}: returns data on hovered data point.}
  \item{\code{id_mouseover_data_value}: returns value of hovered data point.}
  \item{\code{id_mouseover_row}: returns row o hovered data point.}
  \item{\code{id_mouseover_serie}: returns name of serie of hovered data point.}
}
}

\section{Proxies}{

The \code{echarts4rProxy} function returns a proxy for chart which allows
manipulating a drawn chart, adding data, adding or removing series, etc. without
redrawing the entire chart.

\itemize{
  \item{\code{\link{e_append1_p}} & \code{\link{e_append2_p}}}
  \item{\code{\link{e_showtip_p}} & \code{\link{e_hidetip_p}}}
  \item{\code{\link{e_highlight_p}} & \code{\link{e_downplay_p}}}
  \item{\code{\link{e_focus_adjacency}} & \code{\link{e_unfocus_adjacency}}}
  \item{\code{\link{e_dispatch_action_p}}}
  \item{\code{\link{e_execute}}}
  \item{\code{\link{e_remove_serie_p}}}
}
}

