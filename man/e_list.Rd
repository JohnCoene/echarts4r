% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/helpers.R
\name{e_list}
\alias{e_list}
\title{List}
\usage{
e_list(e, list, append = FALSE)
}
\arguments{
\item{e}{An \code{echarts4r} object as returned by \code{\link{e_charts}} or
a proxy as returned by \code{\link{echarts4rProxy}}.}

\item{list}{A \code{list} of options passed to \code{setOptions}.}

\item{append}{if \code{TRUE} the \code{list} is appended to the options,
otherwise it \emph{overwrites} everything.}
}
\description{
simply pass a list of options, similar to a \code{JSON}.
}
\examples{
N <- 20 # data points

opts <- list(
  xAxis = list(
    type = "category",
    data = LETTERS[1:N]
  ),
  yAxis = list(
    type = "value"
  ),
  series = list(
    list(
      type = "line",
      data = round(runif(N, 5, 20))
    )
  )
)

e_charts() |>
  e_list(opts)
}
