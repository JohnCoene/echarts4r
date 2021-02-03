#' Bar and Line chart
#'
#' Add bar serie.
#'
#' @param e An \code{echarts4r} object as returned by \code{\link{e_charts}} or
#' a proxy as returned by \code{\link{echarts4rProxy}}.
#' @param serie Column name of serie to plot.
#' @param bind Binding between datasets, namely for use of \code{\link{e_brush}}.
#' @param name name of the serie.
#' @param legend Whether to add serie to legend.
#' @param ... Any other option to pass, check See Also section.
#' @param x_index,y_index Indexes of x and y axis.
#' @param coord_system Coordinate system to plot against.
#'
#' @examples
#' library(dplyr)
#'
#' mtcars %>%
#'   mutate(
#'     model = row.names(.),
#'     total = mpg + qsec
#'   ) %>%
#'   arrange(desc(total)) %>%
#'   e_charts(model) %>%
#'   e_bar(mpg, stack = "grp") %>%
#'   e_bar(qsec, stack = "grp")
#' @seealso \href{https://echarts.apache.org/en/option.html#series-bar}{Additional arguments}
#'
#' @rdname e_bar
#' @export
e_bar <- function(e, serie, bind, name = NULL, legend = TRUE, y_index = 0, x_index = 0, coord_system = "cartesian2d", ...) UseMethod("e_bar")

#' @method e_bar echarts4r
#' @export
e_bar.echarts4r <- function(e, serie, bind, name = NULL, legend = TRUE, y_index = 0, x_index = 0, coord_system = "cartesian2d", ...) {
  if (missing(serie)) {
    stop("must pass serie", call. = FALSE)
  }

  if (missing(bind)) {
    bd <- NULL
  } else {
    bd <- deparse(substitute(bind))
  }

  sr <- deparse(substitute(serie))

  e_bar_(e, sr, bd, name, legend, y_index, x_index, coord_system, ...)
}

#' @method e_bar echarts4rProxy
#' @export
e_bar.echarts4rProxy <- function(e, serie, bind, name = NULL, legend = TRUE, y_index = 0, x_index = 0, coord_system = "cartesian2d", ...) {
  if (missing(serie)) {
    stop("must pass serie", call. = FALSE)
  }

  if (missing(bind)) {
    bd <- NULL
  } else {
    bd <- deparse(substitute(bind))
  }

  sr <- deparse(substitute(serie))

  e$chart <- e_bar_(e$chart, sr, bd, name, legend, y_index, x_index, coord_system, ...)
  return(e)
}

#' Line
#'
#' Add line serie.
#'
#' @inheritParams e_bar
#' @param coord_system Coordinate system to plot against.
#'
#' @examples
#' iris %>%
#'   group_by(Species) %>%
#'   e_charts(Sepal.Length) %>%
#'   e_line(Sepal.Width) %>%
#'   e_tooltip(trigger = "axis")
#'
#' # timeline
#' iris %>%
#'   group_by(Species) %>%
#'   e_charts(Sepal.Length, timeline = TRUE) %>%
#'   e_line(Sepal.Width) %>%
#'   e_tooltip(trigger = "axis")
#' @seealso \href{https://echarts.apache.org/en/option.html#series-line}{Additional arguments}
#'
#' @rdname e_line
#' @export
e_line <- function(
  e,
  serie,
  bind,
  name = NULL,
  legend = TRUE,
  y_index = 0,
  x_index = 0,
  coord_system = "cartesian2d",
  ...
) {
  UseMethod("e_line")
}

#' @export
#' @method e_line echarts4r
e_line.echarts4r <- function(
  e,
  serie,
  bind,
  name = NULL,
  legend = TRUE,
  y_index = 0,
  x_index = 0,
  coord_system = "cartesian2d",
  ...
) {
  if (missing(serie)) {
    stop("must pass serie", call. = FALSE)
  }

  serie <- deparse(substitute(serie))

  if (missing(bind)) {
    bd <- NULL
  } else {
    bd <- deparse(substitute(bind))
  }

  e_line_(e, serie, bd, name, legend, y_index, x_index, coord_system, ...)
}

#' @export
#' @method e_line echarts4rProxy
e_line.echarts4rProxy <- function(
  e,
  serie,
  bind,
  name = NULL,
  legend = TRUE,
  y_index = 0,
  x_index = 0,
  coord_system = "cartesian2d",
  ...
) {
  if (missing(serie)) {
    stop("must pass serie", call. = FALSE)
  }

  serie <- deparse(substitute(serie))

  if (missing(bind)) {
    bd <- NULL
  } else {
    bd <- deparse(substitute(bind))
  }

  e$chart <- e_line_(e$chart, serie, bd, name, legend, y_index, x_index, coord_system, ...)
  return(e)
}

#' Area
#'
#' Add area serie.
#'
#' @inheritParams e_bar
#'
#' @examples
#' CO2 %>%
#'   group_by(Plant) %>%
#'   e_charts(conc) %>%
#'   e_area(uptake) %>%
#'   e_tooltip(trigger = "axis")
#'
#' # timeline
#' iris %>%
#'   group_by(Species) %>%
#'   e_charts(Sepal.Length, timeline = TRUE) %>%
#'   e_area(Sepal.Width) %>%
#'   e_tooltip(trigger = "axis")
#' @seealso \href{https://echarts.apache.org/en/option.html#series-line}{Additional arguments}
#'
#' @rdname e_area
#' @export
e_area <- function(
  e,
  serie,
  bind,
  name = NULL,
  legend = TRUE,
  y_index = 0,
  x_index = 0,
  coord_system = "cartesian2d",
  ...
) {
  UseMethod("e_area")
}

#' @export
#' @method e_area echarts4r
e_area.echarts4r <- function(
  e,
  serie,
  bind,
  name = NULL,
  legend = TRUE,
  y_index = 0,
  x_index = 0,
  coord_system = "cartesian2d",
  ...
) {
  if (missing(serie)) {
    stop("must pass serie", call. = FALSE)
  }

  serie <- deparse(substitute(serie))

  if (missing(bind)) {
    bd <- NULL
  } else {
    bd <- deparse(substitute(bind))
  }

  e_area_(e, serie, bd, name, legend, y_index, x_index, coord_system, ...)
}

#' @export
#' @method e_area echarts4rProxy
e_area.echarts4rProxy <- function(
  e,
  serie,
  bind,
  name = NULL,
  legend = TRUE,
  y_index = 0,
  x_index = 0,
  coord_system = "cartesian2d",
  ...
) {
  if (missing(serie)) {
    stop("must pass serie", call. = FALSE)
  }

  serie <- deparse(substitute(serie))

  if (missing(bind)) {
    bd <- NULL
  } else {
    bd <- deparse(substitute(bind))
  }

  e$chart <- e_area_(e$chart, serie, bd, name, legend, y_index, x_index, coord_system, ...)
  return(e)
}

#' Step
#'
#' Add step serie.
#'
#' @inheritParams  e_bar
#' @param step Step type, one of \code{start}, \code{middle} or \code{end}.
#' @param fill Set to fill as area.
#'
#' @examples
#' USArrests %>%
#'   dplyr::mutate(State = row.names(.)) %>%
#'   e_charts(State) %>%
#'   e_step(Murder, name = "Start", step = "start", fill = TRUE) %>%
#'   e_step(Rape, name = "Middle", step = "middle") %>%
#'   e_step(Assault, name = "End", step = "end") %>%
#'   e_tooltip(trigger = "axis")
#'
#' # timeline
#' iris %>%
#'   group_by(Species) %>%
#'   e_charts(Sepal.Length, timeline = TRUE) %>%
#'   e_step(Sepal.Width) %>%
#'   e_tooltip(trigger = "axis")
#' @seealso \href{https://echarts.apache.org/en/option.html#series-line}{Additional arguments}
#'
#' @rdname e_step
#' @export
e_step <- function(
  e,
  serie,
  bind,
  step = c("start", "middle", "end"),
  fill = FALSE,
  name = NULL,
  legend = TRUE,
  y_index = 0,
  x_index = 0,
  coord_system = "cartesian2d",
  ...
) {
  UseMethod("e_step")
}

#' @export
#' @method e_step echarts4r
e_step.echarts4r <- function(
  e,
  serie,
  bind,
  step = c("start", "middle", "end"),
  fill = FALSE,
  name = NULL,
  legend = TRUE,
  y_index = 0,
  x_index = 0,
  coord_system = "cartesian2d",
  ...
) {
  if (missing(serie)) {
    stop("must pass serie", call. = FALSE)
  }

  serie <- deparse(substitute(serie))

  if (missing(bind)) {
    bd <- NULL
  } else {
    bd <- deparse(substitute(bind))
  }

  e_step_(e, serie, bd, step, fill, name, legend, y_index, x_index, coord_system = "cartesian2d", ...)
}

#' @export
#' @method e_step echarts4rProxy
e_step.echarts4rProxy <- function(
  e,
  serie,
  bind,
  step = c("start", "middle", "end"),
  fill = FALSE,
  name = NULL,
  legend = TRUE,
  y_index = 0,
  x_index = 0,
  coord_system = "cartesian2d",
  ...
) {
  if (missing(serie)) {
    stop("must pass serie", call. = FALSE)
  }

  serie <- deparse(substitute(serie))

  if (missing(bind)) {
    bd <- NULL
  } else {
    bd <- deparse(substitute(bind))
  }

  e$chart <- e_step_(e$chart, serie, bd, step, fill, name, legend, y_index, x_index, coord_system = "cartesian2d", ...)
  return(e)
}

#' Scatter
#'
#' Add scatter serie.
#'
#' @inheritParams e_bar
#' @param size Column name containing size of points.
#' @param symbol The symbol to use, default to \code{NULL}, can also be \code{circle}, \code{rect},
#' \code{roundRect}, \code{triangle}, \code{diamond}, \code{pin}, \code{arrow}, or \code{none}.
#' @param symbol_size Size of points, either an integer or a vector of length 2,
#' if \code{size} is \emph{not} \code{NULL} or missing it is applied as a multiplier to \code{scale}.
#' @param scale A function that takes a vector of \code{numeric} and returns a vector of \code{numeric}
#' of the same length. You can disable the scaling by setting it to \code{NULL}.
#' @param coord_system Coordinate system to plot against, see examples.
#' @param rm_x,rm_y Whether to remove x and y axis, only applies if \code{coord_system} is not
#' set to \code{cartesian2d}.
#' @param x A vector of integers or numeric.
#' @param jitter_factor,jitter_amount Jitter points, passed to \code{jitter}.
#' @param scale_js the JavaScript scaling function.
#'
#' @section Scaling function: defaults to \code{e_scale} which is a basic function that rescales \code{size}
#' between 1 and 20 for that makes for decent sized points on the chart.
#'
#' @examples
#' # scaling
#' e_scale(c(1, 1000))
#'
#' mtcars %>%
#'   e_charts(mpg) %>%
#'   e_scatter(wt, qsec)
#'
#' # custom function
#' my_scale <- function(x) scales::rescale(x, to = c(2, 50))
#'
#' echart <- mtcars %>%
#'   e_charts(mpg) %>%
#'   e_scatter(wt, qsec, scale = my_scale)
#'
#' echart
#'
#' # rescale color too
#' echart %>%
#'   e_visual_map(wt, scale = my_scale)
#'
#' # or
#' echart %>%
#'   e_visual_map(min = 2, max = 50)
#'
#' # disable scaling
#' mtcars %>%
#'   e_charts(qsec) %>%
#'   e_scatter(wt, mpg, scale = NULL)
#'
#' # jitter point
#' mtcars %>%
#'   e_charts(cyl) %>%
#'   e_scatter(wt, symbol_size = 5) %>%
#'   e_scatter(wt, jitter_factor = 2, legend = FALSE)
#'
#' # examples
#' USArrests %>%
#'   e_charts(Assault) %>%
#'   e_scatter(Murder, Rape) %>%
#'   e_effect_scatter(Rape, Murder, y_index = 1) %>%
#'   e_grid(index = c(0, 1)) %>%
#'   e_tooltip()
#'
#' iris %>%
#'   e_charts_("Sepal.Length") %>%
#'   e_scatter_(
#'     "Sepal.Width",
#'     symbol_size = c(8, 2),
#'     symbol = "rect"
#'   ) %>%
#'   e_x_axis(min = 4)
#'
#' quakes %>%
#'   e_charts(long) %>%
#'   e_geo(
#'     roam = TRUE,
#'     boundingCoords = list(
#'       c(185, -10),
#'       c(165, -40)
#'     )
#'   ) %>%
#'   e_scatter(lat, mag, coord_system = "geo") %>%
#'   e_visual_map(min = 4, max = 6.5)
#'
#' # timeline
#' iris %>%
#'   group_by(Species) %>%
#'   e_charts(Petal.Width, timeline = TRUE) %>%
#'   e_scatter(Sepal.Width, Sepal.Length) %>%
#'   e_tooltip(trigger = "axis")
#' @seealso \href{https://echarts.apache.org/en/option.html#series-scatter}{Additional arguments scatter},
#'  \href{https://echarts.apache.org/en/option.html#series-effectScatter}{Additional arguments for effect scatter}
#'
#' @rdname scatter
#' @export
e_scatter <- function(
  e,
  serie,
  size,
  bind,
  symbol = NULL,
  symbol_size = 1,
  scale = e_scale,
  scale_js = "function(data){ return data[3];}",
  name = NULL,
  coord_system = "cartesian2d",
  jitter_factor = 0,
  jitter_amount = NULL,
  legend = TRUE,
  y_index = 0,
  x_index = 0,
  rm_x = TRUE,
  rm_y = TRUE,
  ...
) {
  UseMethod("e_scatter")
}

#' @export
#' @method e_scatter echarts4r
e_scatter.echarts4r <- function(
  e,
  serie,
  size,
  bind,
  symbol = NULL,
  symbol_size = 1,
  scale = e_scale,
  scale_js = "function(data){ return data[3];}",
  name = NULL,
  coord_system = "cartesian2d",
  jitter_factor = 0,
  jitter_amount = NULL,
  legend = TRUE,
  y_index = 0,
  x_index = 0,
  rm_x = TRUE,
  rm_y = TRUE,
  ...
) {
  if (missing(serie)) {
    stop("must pass serie", call. = FALSE)
  }

  serie <- deparse(substitute(serie))

  if (missing(size)) {
    size <- NULL
  } else {
    size <- deparse(substitute(size))
  }

  if (missing(bind)) {
    bd <- NULL
  } else {
    bd <- deparse(substitute(bind))
  }

  e_scatter_(
    e = e,
    serie = serie,
    size = size,
    bind = bd,
    symbol = symbol,
    symbol_size = symbol_size,
    scale = scale,
    scale_js = scale_js,
    name = name,
    coord_system = coord_system,
    jitter_factor = jitter_factor,
    jitter_amount = jitter_amount,
    legend = legend,
    y_index = y_index,
    x_index = x_index,
    rm_x = rm_x,
    rm_y = rm_y,
    ...
  )
}

#' @export
#' @method e_scatter echarts4rProxy
e_scatter.echarts4rProxy <- function(
  e,
  serie,
  size,
  bind,
  symbol = NULL,
  symbol_size = 1,
  scale = e_scale,
  scale_js = "function(data){ return data[3];}",
  name = NULL,
  coord_system = "cartesian2d",
  jitter_factor = 0,
  jitter_amount = NULL,
  legend = TRUE,
  y_index = 0,
  x_index = 0,
  rm_x = TRUE,
  rm_y = TRUE,
  ...
) {
  if (missing(serie)) {
    stop("must pass serie", call. = FALSE)
  }

  serie <- deparse(substitute(serie))

  if (missing(size)) {
    size <- NULL
  } else {
    size <- deparse(substitute(size))
  }

  if (missing(bind)) {
    bd <- NULL
  } else {
    bd <- deparse(substitute(bind))
  }

  e$chart <- e_scatter_(
    e = e$chart,
    serie = serie,
    size = size,
    bind = bd,
    symbol = symbol,
    symbol_size = symbol_size,
    scale = scale,
    scale_js = scale_js,
    name = name,
    coord_system = coord_system,
    jitter_factor = jitter_factor,
    jitter_amount = jitter_amount,
    legend = legend,
    y_index = y_index,
    x_index = x_index,
    rm_x = rm_x,
    rm_y = rm_y,
    ...
  )

  return(e)
}

#' @rdname scatter
#' @export
e_effect_scatter <- function(
  e,
  serie,
  size,
  bind,
  symbol = NULL,
  symbol_size = 1,
  scale = e_scale,
  scale_js = "function(data){ return data[3];}",
  name = NULL,
  coord_system = "cartesian2d",
  legend = TRUE,
  y_index = 0,
  x_index = 0,
  rm_x = TRUE,
  rm_y = TRUE,
  ...
) {
  UseMethod("e_effect_scatter")
}

#' @export
#' @method e_effect_scatter echarts4r
e_effect_scatter.echarts4r <- function(
  e,
  serie,
  size,
  bind,
  symbol = NULL,
  symbol_size = 1,
  scale = e_scale,
  scale_js = "function(data){ return data[3];}",
  name = NULL,
  coord_system = "cartesian2d",
  legend = TRUE,
  y_index = 0,
  x_index = 0,
  rm_x = TRUE,
  rm_y = TRUE,
  ...
) {
  if (missing(serie)) {
    stop("must pass serie", call. = FALSE)
  }

  serie <- deparse(substitute(serie))

  if (missing(size)) {
    size <- NULL
  } else {
    size <- deparse(substitute(size))
  }

  if (missing(bind)) {
    bd <- NULL
  } else {
    bd <- deparse(substitute(bind))
  }

  e_effect_scatter_(
    e,
    serie = serie,
    size = size,
    bind = bd,
    symbol = symbol,
    symbol_size = symbol_size,
    scale = scale,
    scale_js = scale_js,
    name = name,
    coord_system,
    legend,
    y_index,
    x_index,
    rm_x,
    rm_y,
    ...
  )
}

#' @export
#' @method e_effect_scatter echarts4rProxy
e_effect_scatter.echarts4rProxy <- function(
  e,
  serie,
  size,
  bind,
  symbol = NULL,
  symbol_size = 1,
  scale = e_scale,
  scale_js = "function(data){ return data[3];}",
  name = NULL,
  coord_system = "cartesian2d",
  legend = TRUE,
  y_index = 0,
  x_index = 0,
  rm_x = TRUE,
  rm_y = TRUE,
  ...
) {
  if (missing(serie)) {
    stop("must pass serie", call. = FALSE)
  }

  serie <- deparse(substitute(serie))

  if (missing(size)) {
    size <- NULL
  } else {
    size <- deparse(substitute(size))
  }

  if (missing(bind)) {
    bd <- NULL
  } else {
    bd <- deparse(substitute(bind))
  }

  e$chart <- e_effect_scatter_(
    e$chart,
    serie = serie,
    size = size,
    bind = bd,
    symbol = symbol,
    symbol_size = symbol_size,
    scale = scale,
    scale_js = scale_js,
    name = name,
    coord_system,
    legend,
    y_index,
    x_index,
    rm_x,
    rm_y,
    ...
  )

  return(e)
}

#' Candlestick
#'
#' Add a candlestick chart.
#'
#' @inheritParams e_bar
#' @param opening,closing,low,high Stock prices.
#'
#' @examples
#' date <- c(
#'   "2017-01-01",
#'   "2017-01-02",
#'   "2017-01-03",
#'   "2017-01-04",
#'   "2017-03-05",
#'   "2017-01-06",
#'   "2017-01-07"
#' )
#'
#' stock <- data.frame(
#'   date = date,
#'   opening = c(200.60, 200.22, 198.43, 199.05, 203.54, 203.40, 208.34),
#'   closing = c(200.72, 198.85, 199.05, 203.73, 204.08, 208.11, 211.88),
#'   low = c(197.82, 198.07, 197.90, 198.10, 202.00, 201.50, 207.60),
#'   high = c(203.32, 200.67, 200.00, 203.95, 204.90, 208.44, 213.17)
#' )
#'
#' stock %>%
#'   e_charts(date) %>%
#'   e_candle(opening, closing, low, high) %>%
#'   e_y_axis(min = 190, max = 220)
#' @seealso \href{https://echarts.apache.org/en/option.html#series-candlestick}{Additional arguments}
#'
#' @rdname e_candle
#' @export
e_candle <- function(e, opening, closing, low, high, bind, name = NULL, legend = TRUE, ...) UseMethod("e_candle")

#' @export
#' @method e_candle echarts4r
e_candle.echarts4r <- function(e, opening, closing, low, high, bind, name = NULL, legend = TRUE, ...) {
  if (missing(opening) || missing(closing) || missing(low) || missing(high)) {
    stop("missing inputs", call. = FALSE)
  }

  if (!missing(bind)) {
    bind <- deparse(substitute(bind))
  } else {
    bind <- NULL
  }

  e_candle_(
    e,
    deparse(substitute(opening)),
    deparse(substitute(closing)),
    deparse(substitute(low)),
    deparse(substitute(high)),
    bind,
    name,
    legend,
    ...
  )
}

#' @export
#' @method e_candle echarts4rProxy
e_candle.echarts4rProxy <- function(e, opening, closing, low, high, bind, name = NULL, legend = TRUE, ...) {
  if (missing(opening) || missing(closing) || missing(low) || missing(high)) {
    stop("missing inputs", call. = FALSE)
  }

  if (!missing(bind)) {
    bind <- deparse(substitute(bind))
  } else {
    bind <- NULL
  }

  e$chart <- e_candle_(
    e$chart,
    deparse(substitute(opening)),
    deparse(substitute(closing)),
    deparse(substitute(low)),
    deparse(substitute(high)),
    bind,
    name,
    legend,
    ...
  )

  return(e)
}

#' Radar
#'
#' Add a radar chart
#'
#' @inheritParams e_bar
#' @param max Maximum value.
#' @param rm_x,rm_y Whether to remove x and y axis, defaults to \code{TRUE}.
#' @param radar A \code{list} of options to pass to the \code{radar}
#' rather than the serie, see \href{https://echarts.apache.org/en/option.html#radar}{official documentation}
#' alternatively, use the \code{\link{e_radar_opts}}.
#'
#' @examples
#' df <- data.frame(
#'   x = LETTERS[1:5],
#'   y = runif(5, 1, 5),
#'   z = runif(5, 3, 7)
#' )
#'
#' df %>%
#'   e_charts(x) %>%
#'   e_radar(y, max = 7) %>%
#'   e_radar(z) %>%
#'   e_tooltip(trigger = "item")
#' @rdname e_radar
#' @export
e_radar <- function(
  e,
  serie,
  max = 100,
  name = NULL,
  legend = TRUE,
  rm_x = TRUE,
  rm_y = TRUE,
  ...,
  radar = list()
) {
  UseMethod("e_radar")
}

#' @export
#' @method e_radar echarts4r
e_radar.echarts4r <- function(
  e,
  serie,
  max = 100,
  name = NULL,
  legend = TRUE,
  rm_x = TRUE,
  rm_y = TRUE,
  ...,
  radar = list()
) {
  if (missing(serie)) {
    stop("must pass serie", call. = FALSE)
  }

  serie <- deparse(substitute(serie))

  e_radar_(e, serie, max, name, legend, rm_x, rm_y, ..., radar = radar)
}

#' @export
#' @method e_radar echarts4rProxy
e_radar.echarts4rProxy <- function(
  e,
  serie,
  max = 100,
  name = NULL,
  legend = TRUE,
  rm_x = TRUE,
  rm_y = TRUE,
  ...,
  radar = list()
) {
  if (missing(serie)) {
    stop("must pass serie", call. = FALSE)
  }

  serie <- deparse(substitute(serie))

  e$chart <- e_radar_(e, serie, max, name, legend, rm_x, rm_y, ..., radar = radar)
  return(e)
}

#' Funnel
#'
#' Add a funnel.
#'
#' @param e An \code{echarts4r} object as returned by \code{\link{e_charts}}.
#' @param name name of the serie.
#' @param legend Whether to add serie to legend.
#' @param ... Any other option to pass to \code{bar} or \code{line} char types.
#' @param values,labels Values and labels of funnel.
#' @param rm_x,rm_y Whether to remove x and y axis, defaults to \code{TRUE}.
#'
#' @details No \code{bind} argument here, with a funnel \code{bind} = \code{labels}.
#'
#' @examples
#' funnel <- data.frame(
#'   stage = c("View", "Click", "Purchase"),
#'   value = c(80, 30, 20)
#' )
#'
#' funnel %>%
#'   e_charts() %>%
#'   e_funnel(value, stage)
#' @seealso \href{https://echarts.apache.org/en/option.html#series-funnel}{Additional arguments}
#'
#' @rdname e_funnel
#' @export
e_funnel <- function(e, values, labels, name = NULL, legend = TRUE, rm_x = TRUE, rm_y = TRUE, ...) UseMethod("e_funnel")

#' @export
#' @method e_funnel echarts4r
e_funnel.echarts4r <- function(e, values, labels, name = NULL, legend = TRUE, rm_x = TRUE, rm_y = TRUE, ...) {
  if (missing(values) || missing(labels)) {
    stop("missing values or labels", call. = FALSE)
  }

  e_funnel_(
    e = e,
    values = deparse(substitute(values)),
    labels = deparse(substitute(labels)),
    name = name,
    legend = legend,
    rm_x = rm_x,
    rm_y = rm_y,
    ...
  )
}

#' @export
#' @method e_funnel echarts4rProxy
e_funnel.echarts4rProxy <- function(e, values, labels, name = NULL, legend = TRUE, rm_x = TRUE, rm_y = TRUE, ...) {
  if (missing(values) || missing(labels)) {
    stop("missing values or labels", call. = FALSE)
  }

  e$chart <- e_funnel_(
    e = e$chart,
    values = deparse(substitute(values)),
    labels = deparse(substitute(labels)),
    name = name,
    legend = legend,
    rm_x = rm_x,
    rm_y = rm_y,
    ...
  )

  return(e)
}

#' Sankey
#'
#' Draw a sankey diagram.
#'
#' @inheritParams e_bar
#' @param layout Layout of sankey.
#' @param source,target Source and target columns.
#' @param value Value change from \code{source} to \code{target}.
#' @param rm_x,rm_y Whether to remove the x and y axis, defaults to \code{TRUE}.
#'
#' @examples
#' sankey <- data.frame(
#'   source = c("a", "b", "c", "d", "c"),
#'   target = c("b", "c", "d", "e", "e"),
#'   value = ceiling(rnorm(5, 10, 1)),
#'   stringsAsFactors = FALSE
#' )
#'
#' sankey %>%
#'   e_charts() %>%
#'   e_sankey(source, target, value)
#' @seealso \href{https://echarts.apache.org/en/option.html#series-sankey}{Additional arguments}
#'
#' @rdname e_sankey
#' @export
e_sankey <- function(e, source, target, value, layout = "none", rm_x = TRUE, rm_y = TRUE, ...) UseMethod("e_sankey")

#' @export
#' @method e_sankey echarts4r
e_sankey.echarts4r <- function(e, source, target, value, layout = "none", rm_x = TRUE, rm_y = TRUE, ...) {
  if (missing(source) || missing(target) || missing(value)) {
    stop("missing source, target or values", call. = FALSE)
  }

  e <- .rm_axis(e, rm_x, "x")
  e <- .rm_axis(e, rm_y, "y")

  e_sankey_(
    e,
    deparse(substitute(source)),
    deparse(substitute(target)),
    deparse(substitute(value)),
    layout,
    rm_x,
    rm_y,
    ...
  )
}

#' @export
#' @method e_sankey echarts4rProxy
e_sankey.echarts4rProxy <- function(e, source, target, value, layout = "none", rm_x = TRUE, rm_y = TRUE, ...) {
  if (missing(source) || missing(target) || missing(value)) {
    stop("missing source, target or values", call. = FALSE)
  }

  e <- .rm_axis(e, rm_x, "x")
  e <- .rm_axis(e, rm_y, "y")

  e$chart <- e_sankey_(
    e$chart,
    deparse(substitute(source)),
    deparse(substitute(target)),
    deparse(substitute(value)),
    layout,
    rm_x,
    rm_y,
    ...
  )

  return(e)
}

#' Graph
#'
#' Create a graph.
#'
#' @param e An \code{echarts4} object as returned by \code{e_charts}.
#' @param name Name of graph.
#' @param nodes Data.frame of nodes.
#' @param names Names of nodes, unique.
#' @param value values of nodes.
#' @param size Size of nodes.
#' @param symbol Symbols of nodes.
#' @param legend Whether to add serie to legend.
#' @param category Group of nodes (i.e.: group membership).
#' @param edges Data.frame of edges.
#' @param source,target Column names of source and target.
#' @param layout Layout, one of \code{force}, \code{none} or \code{circular}.
#' @param rm_x,rm_y Whether to remove the x and y axis, defaults to \code{TRUE}.
#' @param itemStyle This option is available for for GL and canvas
#' graph but is only necessary for GL.
#' @param ... Any other parameter.
#'
#' @examples
#' value <- rnorm(10, 10, 2)
#'
#' nodes <- data.frame(
#'   name = sample(LETTERS, 10),
#'   value = value,
#'   size = value,
#'   symbol = sample(c("circle", "rect", "triangle"), 10, replace = TRUE),
#'   grp = rep(c("grp1", "grp2"), 5),
#'   stringsAsFactors = FALSE
#' )
#'
#' edges <- data.frame(
#'   source = sample(nodes$name, 20, replace = TRUE),
#'   target = sample(nodes$name, 20, replace = TRUE),
#'   stringsAsFactors = FALSE
#' )
#'
#' e_charts() %>%
#'   e_graph() %>%
#'   e_graph_nodes(nodes, name, value, size, grp, symbol) %>%
#'   e_graph_edges(edges, source, target)
#'
#' # Use graphGL for larger networks
#' nodes <- data.frame(
#'   name = paste0(LETTERS, 1:1000),
#'   value = rnorm(1000, 10, 2),
#'   size = rnorm(1000, 10, 2),
#'   grp = rep(c("grp1", "grp2"), 500),
#'   stringsAsFactors = FALSE
#' )
#'
#' edges <- data.frame(
#'   source = sample(nodes$name, 2000, replace = TRUE),
#'   target = sample(nodes$name, 2000, replace = TRUE),
#'   stringsAsFactors = FALSE
#' )
#'
#' e_charts() %>%
#'   e_graph_gl() %>%
#'   e_graph_nodes(nodes, name, value, size, grp) %>%
#'   e_graph_edges(edges, source, target)
#' @seealso \href{https://echarts.apache.org/en/option.html#series-graph}{Additional arguments},
#'  \code{\link{e_modularity}}
#'
#' @rdname graph
#' @export
e_graph <- function(e, layout = "force", name = NULL, rm_x = TRUE, rm_y = TRUE, ...) UseMethod("e_graph")

#' @export
#' @method e_graph echarts4r
e_graph.echarts4r <- function(e, layout = "force", name = NULL, rm_x = TRUE, rm_y = TRUE, ...) {
  if (missing(e)) {
    stop("must pass e", call. = FALSE)
  }

  e <- .rm_axis(e, rm_x, "x")
  e <- .rm_axis(e, rm_y, "y")

  serie <- list(
    name = name,
    type = "graph",
    layout = layout,
    ...
  )

  e$x$opts$series <- append(e$x$opts$series, list(serie))

  # dependency
  path <- system.file("htmlwidgets/lib/echarts-4.8.0/plugins", package = "echarts4r")
  dep <- htmltools::htmlDependency(
    name = "echarts-graph-modularity",
    version = "1.1.0",
    src = c(file = path),
    script = "echarts-graph-modularity.min.js"
  )

  e$dependencies <- append(e$dependencies, list(dep))

  e
}

#' @export
#' @method e_graph echarts4rProxy
e_graph.echarts4rProxy <- function(e, layout = "force", name = NULL, rm_x = TRUE, rm_y = TRUE, ...) {
  e$chart <- e_graph(e$chart, layout, name, rm_x, rm_y, ...)
  return(e)
}

#' @rdname graph
#' @export
e_graph_gl <- function(e, layout = "force", name = NULL, rm_x = TRUE, rm_y = TRUE, ..., itemStyle = list(opacity = 1)) UseMethod("e_graph_gl")

#' @export
#' @method e_graph_gl echarts4r
e_graph_gl.echarts4r <- function(e, layout = "force", name = NULL, rm_x = TRUE, rm_y = TRUE, ..., itemStyle = list(opacity = 1)) {
  e <- .rm_axis(e, rm_x, "x")
  e <- .rm_axis(e, rm_y, "y")

  serie <- list(
    name = name,
    type = "graphGL",
    layout = layout,
    itemStyle = itemStyle,
    ...
  )

  # add dependencies
  path <- system.file("htmlwidgets/lib/echarts-4.8.0", package = "echarts4r")
  dep_gl <- htmltools::htmlDependency(
    name = "echarts-gl",
    version = "1.1.2",
    src = c(file = path),
    script = "echarts-gl.min.js"
  )

  path <- system.file("htmlwidgets/lib/echarts-4.8.0/plugins", package = "echarts4r")
  dep_modularity <- htmltools::htmlDependency(
    name = "echarts-graph-modularity",
    version = "1.1.0",
    src = c(file = path),
    script = "echarts-graph-modularity.min.js"
  )

  e$dependencies <- append(e$dependencies, list(dep_gl))
  e$dependencies <- append(e$dependencies, list(dep_modularity))

  e$x$opts$series <- append(e$x$opts$series, list(serie))
  e
}

#' @export
#' @method e_graph_gl echarts4rProxy
e_graph_gl.echarts4rProxy <- function(e, layout = "force", name = NULL, rm_x = TRUE, rm_y = TRUE, ...) {
  e$chart <- e_graph_gl(e$chart, layout, name, rm_x, rm_y, ...)
  return(e)
}

#' @rdname graph
#' @export
e_graph_nodes <- function(e, nodes, names, value, size, category, symbol = NULL, legend = TRUE) UseMethod("e_graph_nodes")

#' @export
#' @method e_graph_nodes echarts4r
e_graph_nodes.echarts4r <- function(e, nodes, names, value, size, category, symbol = NULL, legend = TRUE) {
  if (missing(nodes) || missing(names) || missing(value)) {
    stop("missing arguments", call. = FALSE)
  }

  value <- dplyr::enquo(value)
  symbolSize <- dplyr::enquo(size)
  symbol <- dplyr::enquo(symbol)
  names <- dplyr::enquo(names)

  if (!missing(category) && !missing(size)) {
    e$x$opts$series[[length(e$x$opts$series)]]$categories <- .build_graph_category(nodes, dplyr::enquo(category))

    if (isTRUE(legend)) {
      e$x$opts$legend$data <- append(e$x$opts$legend$data, unique(nodes[[deparse(substitute(category))]]))
    }

    nodes <- .build_graph_nodes(
      nodes,
      names,
      value,
      symbolSize,
      dplyr::enquo(category),
      symbol
    )
  } else if (missing(category) && !missing(size)) {
    nodes <- .build_graph_nodes_no_cat(
      nodes,
      names,
      value,
      symbolSize,
      symbol
    )
  } else if (missing(category) && missing(size)) {
    nodes <- .build_graph_nodes_no_size(
      nodes,
      names,
      value,
      symbol
    )
  }

  # build JSON data
  e$x$opts$series[[length(e$x$opts$series)]]$data <- nodes
  e
}

#' @export
#' @method e_graph_nodes echarts4rProxy
e_graph_nodes.echarts4rProxy <- function(e, nodes, names, value, size, category, symbol = NULL, legend = TRUE) {
  if (missing(nodes) || missing(names) || missing(value)) {
    stop("missing arguments", call. = FALSE)
  }

  value <- dplyr::enquo(value)
  symbolSize <- dplyr::enquo(size)
  symbol <- dplyr::enquo(symbol)
  names <- dplyr::enquo(names)

  if (!missing(category) && !missing(size)) {
    e$chart$x$opts$series[[length(e$chart$x$opts$series)]]$categories <- .build_graph_category(nodes, dplyr::enquo(category))

    if (isTRUE(legend)) {
      e$chart$x$opts$legend$data <- append(e$chart$x$opts$legend$data, unique(nodes[[deparse(substitute(category))]]))
    }

    nodes <- .build_graph_nodes(
      nodes,
      names,
      value,
      symbolSize,
      symbol,
      dplyr::enquo(category)
    )
  } else if (missing(category) && !missing(size)) {
    nodes <- .build_graph_nodes_no_cat(
      nodes,
      names,
      value,
      symbolSize,
      symbol
    )
  } else if (missing(category) && missing(size)) {
    nodes <- .build_graph_nodes_no_size(
      nodes,
      names,
      value,
      symbol
    )
  }

  # build JSON data
  e$chart$x$opts$series[[length(e$chart$x$opts$series)]]$data <- nodes
  e
}

#' @rdname graph
#' @export
e_graph_edges <- function(e, edges, source, target) UseMethod("e_graph_edges")

#' @method e_graph_edges echarts4r
#' @export
e_graph_edges.echarts4r <- function(e, edges, source, target) {
  if (missing(edges) || missing(source) || missing(target)) {
    stop("must pass edges, source and target", call. = FALSE)
  }

  source <- dplyr::enquo(source)
  target <- dplyr::enquo(target)

  data <- .build_graph_edges(
    edges,
    source,
    target
  )

  # build JSON data
  e$x$opts$series[[length(e$x$opts$series)]]$links <- data
  e
}


#' @method e_graph_edges echarts4rProxy
#' @export
e_graph_edges.echarts4rProxy <- function(e, edges, source, target) {
  if (missing(edges) || missing(source) || missing(target)) {
    stop("must pass edges, source and target", call. = FALSE)
  }

  source <- dplyr::enquo(source)
  target <- dplyr::enquo(target)

  data <- .build_graph_edges(
    edges,
    source,
    target
  )

  # build JSON data
  e$chart$x$opts$series[[length(e$chart$x$opts$series)]]$links <- data
  e
}

#' Heatmap
#'
#' Draw heatmap by coordinates.
#'
#' @inheritParams e_bar
#' @param y,z Coordinates and values.
#' @param coord_system Coordinate system to plot against, takes
#' \code{cartesian2d}, \code{geo} or \code{calendar}.
#' @param rm_x,rm_y Whether to remove x and y axis, only applies if \code{coord_system} is not
#' set to \code{cartesian2d}.
#' @param calendar The index of the calendar to plot against.
#'
#' @examples
#' v <- LETTERS[1:10]
#' matrix <- data.frame(
#'   x = sample(v, 300, replace = TRUE),
#'   y = sample(v, 300, replace = TRUE),
#'   z = rnorm(300, 10, 1),
#'   stringsAsFactors = FALSE
#' ) %>%
#'   dplyr::group_by(x, y) %>%
#'   dplyr::summarise(z = sum(z)) %>%
#'   dplyr::ungroup()
#'
#' matrix %>%
#'   e_charts(x) %>%
#'   e_heatmap(y, z, itemStyle = list(emphasis = list(shadowBlur = 10))) %>%
#'   e_visual_map(z)
#'
#' # calendar
#' dates <- seq.Date(as.Date("2017-01-01"), as.Date("2018-12-31"), by = "day")
#' values <- rnorm(length(dates), 20, 6)
#'
#' year <- data.frame(date = dates, values = values)
#'
#' year %>%
#'   e_charts(date) %>%
#'   e_calendar(range = "2018") %>%
#'   e_heatmap(values, coord_system = "calendar") %>%
#'   e_visual_map(max = 30)
#'
#' # calendar multiple years
#' year %>%
#'   dplyr::mutate(year = format(date, "%Y")) %>%
#'   group_by(year) %>%
#'   e_charts(date) %>%
#'   e_calendar(range = "2017", top = 40) %>%
#'   e_calendar(range = "2018", top = 260) %>%
#'   e_heatmap(values, coord_system = "calendar") %>%
#'   e_visual_map(max = 30)
#'
#' # map
#' quakes %>%
#'   e_charts(long) %>%
#'   e_geo(
#'     boundingCoords = list(
#'       c(190, -10),
#'       c(180, -40)
#'     )
#'   ) %>%
#'   e_heatmap(
#'     lat,
#'     mag,
#'     coord_system = "geo",
#'     blurSize = 5,
#'     pointSize = 3
#'   ) %>%
#'   e_visual_map(mag)
#'
#' # timeline
#' library(dplyr)
#'
#' axis <- LETTERS[1:10]
#' df <- expand.grid(axis, axis)
#'
#' bind_rows(df, df) %>%
#'   mutate(
#'     values = runif(n(), 1, 10),
#'     grp = c(
#'       rep("A", 100),
#'       rep("B", 100)
#'     )
#'   ) %>%
#'   group_by(grp) %>%
#'   e_charts(Var1, timeline = TRUE) %>%
#'   e_heatmap(Var2, values) %>%
#'   e_visual_map(values)
#' @seealso \href{https://echarts.apache.org/en/option.html#series-heatmap}{Additional arguments}
#'
#' @rdname e_heatmap
#' @export
e_heatmap <- function(
  e,
  y,
  z,
  bind,
  name = NULL,
  coord_system = "cartesian2d",
  rm_x = TRUE,
  rm_y = TRUE,
  calendar = NULL,
  ...
) {
  UseMethod("e_heatmap")
}

#' @export
#' @method e_heatmap echarts4r
e_heatmap.echarts4r <- function(
  e,
  y,
  z,
  bind,
  name = NULL,
  coord_system = "cartesian2d",
  rm_x = TRUE,
  rm_y = TRUE,
  calendar = NULL,
  ...
) {
  if (missing(y)) {
    stop("must pass y", call. = FALSE)
  }

  if (!missing(z)) {
    z <- deparse(substitute(z))
  } else {
    z <- NULL
  }

  if (!missing(bind)) {
    bind <- deparse(substitute(bind))
  } else {
    bind <- NULL
  }

  e_heatmap_(e, deparse(substitute(y)), z, bind, name, coord_system, rm_x, rm_y, calendar, ...)
}

#' @export
#' @method e_heatmap echarts4rProxy
e_heatmap.echarts4rProxy <- function(
  e,
  y,
  z,
  bind,
  name = NULL,
  coord_system = "cartesian2d",
  rm_x = TRUE,
  rm_y = TRUE,
  calendar = NULL,
  ...
) {
  if (missing(y)) {
    stop("must pass y", call. = FALSE)
  }

  if (!missing(z)) {
    z <- deparse(substitute(z))
  } else {
    z <- NULL
  }

  if (!missing(bind)) {
    bind <- deparse(substitute(bind))
  } else {
    bind <- NULL
  }

  e$chart <- e_heatmap_(e$chart, deparse(substitute(y)), z, bind, name, coord_system, rm_x, rm_y, calendar, ...)
  return(e)
}

#' Parallel
#'
#' Draw parallel coordinates.
#'
#' @inheritParams e_bar
#' @param ... Columns to select from the data passed to \code{\link{e_charts}}.
#' @param rm_x,rm_y Whether to remove x and y axis, defaults to \code{TRUE}.
#' @param opts A list of additional options to pass to the serie.
#'
#' @examples
#' df <- data.frame(
#'   price = rnorm(5, 10),
#'   amount = rnorm(5, 15),
#'   letter = LETTERS[1:5]
#' )
#'
#' df %>%
#'   e_charts() %>%
#'   e_parallel(price, amount, letter, opts = list(smooth = TRUE))
#' @seealso \href{https://echarts.apache.org/en/option.html#series-parallel}{Additional arguments}
#'
#' @rdname e_parallel
#' @export
e_parallel <- function(e, ..., name = NULL, rm_x = TRUE, rm_y = TRUE, opts = list()) UseMethod("e_parallel")

#' @export
#' @method e_parallel echarts4r
e_parallel.echarts4r <- function(e, ..., name = NULL, rm_x = TRUE, rm_y = TRUE, opts = list()) {
  e <- .rm_axis(e, rm_x, "x")
  e <- .rm_axis(e, rm_y, "y")

  e$x$data[[1]] %>%
    dplyr::select(...) -> df

  # remove names
  data <- df
  row.names(data) <- NULL
  data <- unname(data)

  data <- apply(data, 1, as.list)

  # remove white spaces (ex: in " 1")
  data <- purrr::map(data, function(x) {
    x %>%
      gsub(" ", "", .) %>%
      as.list()
  })

  serie <- list(
    name = name,
    type = "parallel",
    data = data
  )

  serie <- append(serie, opts)

  para <- list()
  for (i in 1:ncol(df)) {
    line <- list()
    line$dim <- i - 1
    line$name <- names(df)[i]
    if (inherits(df[, i], "character") || inherits(df[, i], "factor")) {
      line$type <- "category"
      line$data <- unique(df[, i])
    }

    para[[i]] <- line
  }

  e$x$opts$series <- append(e$x$opts$series, serie)
  e$x$opts$parallelAxis <- para
  e
}

#' @export
#' @method e_parallel echarts4rProxy
e_parallel.echarts4rProxy <- function(e, ..., name = NULL, rm_x = TRUE, rm_y = TRUE) {
  e$chart <- e_parallel(e$chart, ..., name = name, rm_x = rm_x, rm_y = rm_y)
  return(e)
}

#' Pie
#'
#' Draw pie and donut charts.
#'
#' @inheritParams e_bar
#' @param rm_x,rm_y Whether to remove x and y axis, defaults to \code{TRUE}.
#'
#' @examples
#' mtcars %>%
#'   head() %>%
#'   dplyr::mutate(model = row.names(.)) %>%
#'   e_charts(model) %>%
#'   e_pie(carb)
#'
#' # timeline
#' df <- data.frame(
#'   grp = c("A", "A", "A", "B", "B", "B"),
#'   labels = rep(LETTERS[1:3], 2),
#'   values = runif(6, 1, 5)
#' )
#'
#' df %>%
#'   group_by(grp) %>%
#'   e_charts(labels, timeline = TRUE) %>%
#'   e_pie(values)
#' @seealso \href{https://echarts.apache.org/en/option.html#series-pie}{Additional arguments}
#'
#' @rdname e_pie
#' @export
e_pie <- function(e, serie, name = NULL, legend = TRUE, rm_x = TRUE, rm_y = TRUE, ...) UseMethod("e_pie")

#' @export
#' @method e_pie echarts4r
e_pie.echarts4r <- function(e, serie, name = NULL, legend = TRUE, rm_x = TRUE, rm_y = TRUE, ...) {
  if (missing(serie)) {
    stop("must pass serie", call. = FALSE)
  }

  e_pie_(e, deparse(substitute(serie)), name, legend, rm_x, rm_y, ...)
}

#' @export
#' @method e_pie echarts4rProxy
e_pie.echarts4rProxy <- function(e, serie, name = NULL, legend = TRUE, rm_x = TRUE, rm_y = TRUE, ...) {
  if (missing(serie)) {
    stop("must pass serie", call. = FALSE)
  }

  e$chart <- e_pie_(e$chart, deparse(substitute(serie)), name, legend, rm_x, rm_y, ...)
  return(e)
}

#' Sunburst
#'
#' Build a sunburst.
#'
#' @inheritParams e_bar
#' @param styles  Vector of style lists, defaults to \code{NULL}.
#' @param names Names of items to style, expects a \code{list}, defaults to \code{NULL}.
#' @param levels Hierarchical levels to style, expects a \code{list}, defaults to \code{NULL}.
#' @param rm_x,rm_y Whether to remove x and y axis, defaults to \code{TRUE}.
#'
#' @details
#' Charts e_sunburst, e_treemap and e_tree require hierarchical input data.
#' Such structure could be represented thru json lists or nested tibbles (data.frame).
#' Input data may contain styles, see \code{itemStyle} in examples \code{jsonl} and \code{df} below.
#' The number of lists in the \code{styles} parameter should match the number of elements
#' in \code{names} and/or \code{levels}. If both \code{names} and \code{levels} are present,
#' name styles will take precedence over level styles.
#' Multiple names may have the same style, see \code{c('land','river')} below.
#' Multiple levels may have the same style, see \code{c(3,4)} below.
#' \code{styles} lists contain items such as \code{color}, or \code{borderColor} as specified in the
#' \href{https://echarts.apache.org/en/option.html#series-sunburst.data.itemStyle}{official documentation}.
#'
#' @examples
#'
#' # json list hierarchical data representation
#' jsonl <- jsonlite::fromJSON('[
#'   {"name": "earth", "value": 30,
#'     "children": [
#'       {"name": "land", "value":10,
#'         "children": [
#'                 {"name": "forest", "value": 3},
#'                 {"name": "river", "value": 7}
#'         ]},
#'       {"name": "ocean", "value":20,
#'         "children": [
#'           {"name": "fish", "value": 10,
#'             "children": [
#'               {"name": "shark", "value":2},
#'               {"name": "tuna", "value":6}
#'             ]},
#'           {"name": "kelp", "value": 5}
#'         ]}
#'     ]
#'   },
#'   {"name": "mars", "value": 30,
#'     "children": [
#'       {"name": "crater", "value": 20},
#'       {"name": "valley", "value": 20}
#'     ]},
#'   {"name": "venus", "value": 40, "itemStyle": {"color": "blue"} }
#' ]', simplifyDataFrame = FALSE)
#'
#' jsonl %>%
#'   e_charts() %>%
#'   e_sunburst() # demo
#'
#'
#' # tibble hierarchical data representation
#' library(dplyr)
#' df <- tibble(
#'   name = c("earth", "mars", "venus"),
#'   value = c(30, 40, 30),
#'   # 1st level
#'   itemStyle = tibble(color = c(NA, "red", "blue")),
#'   # embedded styles, optional
#'   children = list(
#'     tibble(
#'       name = c("land", "ocean"),
#'       value = c(10, 20),
#'       # 2nd level
#'       children = list(
#'         tibble(name = c("forest", "river"), value = c(3, 7)),
#'         # 3rd level
#'         tibble(
#'           name = c("fish", "kelp"),
#'           value = c(10, 5),
#'           children = list(
#'             tibble(name = c("shark", "tuna"), value = c(2, 6)),
#'             # 4th level
#'             NULL # kelp
#'           )
#'         )
#'       )
#'     ),
#'     tibble(name = c("crater", "valley"), value = c(20, 20)),
#'     NULL # venus
#'   )
#' )
#'
#' df %>%
#'   e_charts() %>%
#'   e_sunburst() %>%
#'   e_theme("westeros")
#'
#' # with styles
#' myStyles <- c(list(color = "green"), list(color = "magenta")) # custom styles defined
#' myNames <- list(c("land", "river"), "crater") # names to style
#' myLevels <- list(2, c(3, 4)) # hierarchical levels to style
#'
#' df %>%
#'   e_charts() %>%
#'   e_sunburst(myStyles, myNames, myLevels)
#' @seealso \href{https://echarts.apache.org/en/option.html#series-sunburst}{Additional arguments}
#'
#' @rdname e_sunburst
#' @export
e_sunburst <- function(e, styles = NULL, names = NULL, levels = NULL, rm_x = TRUE, rm_y = TRUE, ...) UseMethod("e_sunburst")

#' @export
#' @method e_sunburst echarts4r
e_sunburst.echarts4r <- function(e, styles = NULL, names = NULL, levels = NULL, rm_x = TRUE, rm_y = TRUE, ...) {
  e_sunburst_(e, styles, names, levels, rm_x, rm_y, ...)
}

#' @export
#' @method e_sunburst echarts4rProxy
e_sunburst.echarts4rProxy <- function(e, styles = NULL, names = NULL, levels = NULL, rm_x = TRUE, rm_y = TRUE, ...) {
  e$chart <- e_sunburst_(e, styles, names, levels, rm_x, rm_y, ...)

  return(e)
}

#' Treemap
#'
#' Build a treemap.
#'
#' @inheritParams e_bar
#' @param styles  Vector of style lists, defaults to \code{NULL}.
#' @param names Names of items to style, expects a \code{list}, defaults to \code{NULL}.
#' @param levels Hierarchical levels to style, expects a \code{list}, defaults to \code{NULL}.
#' @param rm_x,rm_y Whether to remove x and y axis, defaults to \code{TRUE}.
#'
#' @examples
#' library(dplyr)
#' df <- tibble(
#'   name = c("earth", "mars", "venus"),
#'   value = c(30, 40, 30),
#'   # 1st level
#'   itemStyle = tibble(color = c(NA, "red", "blue")),
#'   # embedded styles, optional
#'   children = list(
#'     tibble(
#'       name = c("land", "ocean"),
#'       value = c(10, 20),
#'       # 2nd level
#'       children = list(
#'         tibble(name = c("forest", "river"), value = c(3, 7)),
#'         # 3rd level
#'         tibble(
#'           name = c("fish", "kelp"),
#'           value = c(10, 5),
#'           children = list(
#'             tibble(name = c("shark", "tuna"), value = c(2, 6)),
#'             # 4th level
#'             NULL # kelp
#'           )
#'         )
#'       )
#'     ),
#'     tibble(name = c("crater", "valley"), value = c(20, 20)),
#'     NULL # venus
#'   )
#' )
#'
#' df %>%
#'   e_charts() %>%
#'   e_treemap()
#' @seealso \href{https://echarts.apache.org/en/option.html#series-treemap}{Additional arguments}
#'
#' @rdname e_treemap
#' @export
e_treemap <- function(e, styles = NULL, names = NULL, levels = NULL, rm_x = TRUE, rm_y = TRUE, ...) UseMethod("e_treemap")

#' @export
#' @method e_treemap echarts4r
e_treemap.echarts4r <- function(e, styles = NULL, names = NULL, levels = NULL, rm_x = TRUE, rm_y = TRUE, ...) {
  e_treemap_(e, styles, names, levels, rm_x, rm_y, ...)
}

#' @export
#' @method e_treemap echarts4rProxy
e_treemap.echarts4rProxy <- function(e, styles = NULL, names = NULL, levels = NULL, rm_x = TRUE, rm_y = TRUE, ...) {
  e$chart <- e_treemap_(e, styles, names, levels, rm_x, rm_y, ...)

  return(e)
}

#' River
#'
#' Build a theme river.
#'
#' @inheritParams e_bar
#' @param rm_x,rm_y Whether to remove x and y axis, defaults to \code{TRUE}.
#'
#' @examples
#' dates <- seq.Date(Sys.Date() - 30, Sys.Date(), by = "day")
#' grps <- lapply(LETTERS[1:3], rep, 31) %>% unlist()
#'
#' df <- data.frame(
#'   dates = rep(dates, 3),
#'   groups = grps,
#'   values = runif(length(grps), 1, 50)
#' )
#'
#' df %>%
#'   group_by(groups) %>%
#'   e_charts(dates) %>%
#'   e_river(values) %>%
#'   e_tooltip(trigger = "axis")
#' @seealso \href{https://echarts.apache.org/en/option.html#series-themeRiver}{Additional arguments}
#'
#' @rdname e_river
#' @export
e_river <- function(e, serie, name = NULL, legend = TRUE, rm_x = TRUE, rm_y = TRUE, ...) UseMethod("e_river")

#' @export
#' @method e_river echarts4r
e_river.echarts4r <- function(e, serie, name = NULL, legend = TRUE, rm_x = TRUE, rm_y = TRUE, ...) {
  if (missing(serie)) {
    stop("must pass serie", call. = FALSE)
  }

  e_river_(e, deparse(substitute(serie)), name, legend, rm_x, rm_y, ...)
}

#' @export
#' @method e_river echarts4rProxy
e_river.echarts4rProxy <- function(e, serie, name = NULL, legend = TRUE, rm_x = TRUE, rm_y = TRUE, ...) {
  if (missing(serie)) {
    stop("must pass serie", call. = FALSE)
  }

  e$chart <- e_river_(e$chart, deparse(substitute(serie)), name, legend, rm_x, rm_y, ...)
  return(e)
}

#' Boxplot
#'
#' Draw boxplot.
#'
#' @inheritParams e_bar
#' @param outliers Whether to plot outliers.
#
#'
#' @examples
#' df <- data.frame(
#'   x = c(1:10, 25),
#'   y = c(1:10, -6)
#' )
#'
#' df %>%
#'   e_charts() %>%
#'   e_boxplot(y, outliers = TRUE) %>%
#'   e_boxplot(x, outliers = TRUE)
#' @seealso \href{https://echarts.apache.org/en/option.html#series-boxplot}{Additional arguments}
#'
#' @rdname e_boxplot
#' @export
e_boxplot <- function(e, serie, name = NULL, outliers = TRUE, ...) UseMethod("e_boxplot")

#' @export
#' @method e_boxplot echarts4r
e_boxplot.echarts4r <- function(e, serie, name = NULL, outliers = TRUE, ...) {
  if (missing(serie)) {
    stop("must pass serie", call. = FALSE)
  }

  if (is.null(name)) { # defaults to column name
    name <- deparse(substitute(serie))
  }

  e_boxplot_(e, deparse(substitute(serie)), name, outliers, ...)
}

#' @export
#' @method e_boxplot echarts4rProxy
e_boxplot.echarts4rProxy <- function(e, serie, name = NULL, outliers = TRUE, ...) {
  if (missing(serie)) {
    stop("must pass serie", call. = FALSE)
  }

  if (is.null(name)) { # defaults to column name
    name <- deparse(substitute(serie))
  }

  e$chart <- e_boxplot_(e, deparse(substitute(serie)), name, outliers, ...)
  return(e)
}

#' Tree
#'
#' Build a tree.
#'
#' @inheritParams e_bar
#' @param rm_x,rm_y Whether to remove x and y axis, defaults to \code{TRUE}.
#'
#' @examples
#' library(dplyr)
#' df <- tibble(
#'   name = "earth",
#'   # 1st level
#'   children = list(
#'     tibble(
#'       name = c("land", "ocean"),
#'       # 2nd level
#'       children = list(
#'         tibble(name = c("forest", "river")),
#'         # 3rd level
#'         tibble(
#'           name = c("fish", "kelp"),
#'           children = list(
#'             tibble(
#'               name = c("shark", "tuna"),
#'               # 4th level
#'               NULL # kelp
#'             )
#'           )
#'         )
#'       )
#'     )
#'   )
#' )
#'
#' df %>%
#'   e_charts() %>%
#'   e_tree(initialTreeDepth = 3, label = list(offset = c(0, -11)))
#' @seealso \href{https://echarts.apache.org/en/option.html#series-tree}{Additional arguments}
#'
#' @rdname e_tree
#' @export
e_tree <- function(e, rm_x = TRUE, rm_y = TRUE, ...) UseMethod("e_tree")

#' @export
#' @method e_tree echarts4r
e_tree.echarts4r <- function(e, rm_x = TRUE, rm_y = TRUE, ...) {
  e_tree_(e, rm_x, rm_y, ...)
}

#' @export
#' @method e_tree echarts4rProxy
e_tree.echarts4rProxy <- function(e, rm_x = TRUE, rm_y = TRUE, ...) {
  e$chart <- e_tree_(e$chart, rm_x, rm_y, ...)
  return(e)
}

#' Gauge
#'
#' Plot a gauge.
#'
#' @inheritParams e_bar
#' @param value Value to gauge.
#' @param name Text on gauge.
#' @param rm_x,rm_y Whether to remove x and y axis, defaults to \code{TRUE}.
#'
#' @examples
#' e_charts() %>%
#'   e_gauge(57, "PERCENT")
#' @seealso \href{https://echarts.apache.org/en/option.html#series-gauge}{Additional arguments}
#'
#' @rdname e_gauge
#' @export
e_gauge <- function(e, value, name, rm_x = TRUE, rm_y = TRUE, ...) UseMethod("e_gauge")

#' @export
#' @method e_gauge echarts4r
e_gauge.echarts4r <- function(e, value, name, rm_x = TRUE, rm_y = TRUE, ...) {
  if (missing(e) || missing(value) || missing(name)) {
    stop("missing e, name, or value", call. = FALSE)
  }

  if (!inherits(value, "numeric")) {
    stop("must pass numeric or integer", call. = FALSE)
  }

  # remove axis
  e <- .rm_axis(e, rm_x, "x")
  e <- .rm_axis(e, rm_y, "y")

  for (i in seq_along(value)) {
    serie <- list(
      data = list(list(value = value[i], name = name[i]))
    )

    opts <- list(
      type = "gauge",
      ...
    )

    if (!e$x$tl) {
      lst <- append(serie, opts)
      e$x$opts$series <- append(e$x$opts$series, list(lst))
    } else {
      e$x$opts$options[[i]]$series <- append(e$x$opts$options[[i]]$series, list(serie))
      e$x$opts$baseOption$series <- append(e$x$opts$baseOption$series, list(opts))
    }
  }
  e
}

#' @export
#' @method e_gauge echarts4rProxy
e_gauge.echarts4rProxy <- function(e, value, name, rm_x = TRUE, rm_y = TRUE, ...) {
  e <- e$chart <- e_gauge(e$chart, value, name, rm_x, rm_y, ...)
  return(e)
}

#' @inheritParams e_bar
#' @param value Value to gauge.
#' @param name Text on gauge.
#' @param rm_x,rm_y Whether to remove x and y axis, defaults to \code{TRUE}.
#'
#' @rdname e_gauge
#' @export
e_gauge_ <- function(e, value, name, rm_x = TRUE, rm_y = TRUE, ...) {
  if (missing(e) || missing(value) || missing(name)) {
    stop("missing e, name, or value", call. = FALSE)
  }

  # remove axis
  e <- .rm_axis(e, rm_x, "x")
  e <- .rm_axis(e, rm_y, "y")

  values <- list()

  for (i in seq_along(e$x$data)) {
    values[[i]] <- .get_data(e, value, i = i) %>%
      unlist() %>%
      unname() %>%
      .[[1]]

    serie <- list(
      data = list(list(value = values[i], name = name))
    )

    opts <- list(
      type = "gauge",
      ...
    )

    if (!e$x$tl) {
      lst <- append(serie, opts)
      e$x$opts$series <- append(e$x$opts$series, list(lst))
    } else {
      e$x$opts$options[[i]]$series <- append(e$x$opts$options[[i]]$series, list(serie))
      e$x$opts$baseOption$series <- append(e$x$opts$baseOption$series, opts)
    }
  }
  e
}

#' Lines 3D
#'
#' Add 3D lines.
#'
#' @inheritParams e_bar
#' @param coord_system Coordinate system to use, such as \code{cartesian3D}, or \code{globe}.
#' @param y,z Coordinates of lines.
#' @param source_lon,source_lat,target_lon,target_lat coordinates.
#' @param source_name,target_name Names of source and target.
#' @param value Value of edges.
#' @param rm_x,rm_y Whether to remove x and y axis, defaults to \code{TRUE}.
#'
#' @examples
#' # get data
#' flights <- read.csv(
#'   paste0(
#'     "https://raw.githubusercontent.com/plotly/datasets/",
#'     "master/2011_february_aa_flight_paths.csv"
#'   )
#' )
#'
#' # Lines 3D
#' # Globe
#' # get tetures: echarts4r-assets.john-coene.com
#' flights %>%
#'   e_charts() %>%
#'   e_globe(
#'     displacementScale = 0.05
#'   ) %>%
#'   e_lines_3d(
#'     start_lon,
#'     start_lat,
#'     end_lon,
#'     end_lat,
#'     name = "flights",
#'     effect = list(show = TRUE)
#'   ) %>%
#'   e_legend(FALSE)
#'
#' # Geo 3D
#' flights %>%
#'   e_charts() %>%
#'   e_geo_3d() %>%
#'   e_lines_3d(
#'     start_lon,
#'     start_lat,
#'     end_lon,
#'     end_lat,
#'     coord_system = "geo3D"
#'   )
#'
#' # groups
#' flights$grp <- rep(LETTERS[1:2], 89)
#'
#' flights %>%
#'   group_by(grp) %>%
#'   e_charts() %>%
#'   e_geo_3d() %>%
#'   e_lines_3d(
#'     start_lon,
#'     start_lat,
#'     end_lon,
#'     end_lat,
#'     coord_system = "geo3D"
#'   )
#'
#' # line 3D
#' df <- data.frame(
#'   x = 1:100,
#'   y = runif(100, 10, 25),
#'   z = rnorm(100, 100, 50)
#' )
#'
#' df %>%
#'   e_charts(x) %>%
#'   e_line_3d(y, z) %>%
#'   e_visual_map() %>%
#'   e_title("nonsense")
#'
#' # timeline
#' df$grp <- rep(LETTERS[1:5], 20)
#'
#' df %>%
#'   group_by(grp) %>%
#'   e_charts(x) %>%
#'   e_line_3d(y, z) %>%
#'   e_visual_map() %>%
#'   e_title("nonsense")
#' @seealso \href{https://echarts.apache.org/en/option-gl.html#series-lines3D}{Additional arguments for lines 3D},
#'  \href{https://echarts.apache.org/en/option-gl.html#series-lines3D}{Additional arguments for line 3D}
#'
#' @seealso \url{https://echarts4r-assets.john-coene.com}
#'
#' @rdname line3D
#' @export
e_lines_3d <- function(
  e,
  source_lon,
  source_lat,
  target_lon,
  target_lat,
  source_name,
  target_name,
  value,
  name = NULL,
  coord_system = "globe",
  rm_x = TRUE,
  rm_y = TRUE,
  ...
) {
  UseMethod("e_lines_3d")
}

#' @export
#' @method e_lines_3d echarts4r
e_lines_3d.echarts4r <- function(
  e,
  source_lon,
  source_lat,
  target_lon,
  target_lat,
  source_name,
  target_name,
  value,
  name = NULL,
  coord_system = "globe",
  rm_x = TRUE,
  rm_y = TRUE,
  ...
) {
  if (missing(source_lat) || missing(source_lon) || missing(target_lat) || missing(target_lon)) {
    stop("missing coordinates", call. = FALSE)
  }

  if (missing(source_name)) {
    source_name <- NULL
  } else {
    source_name <- deparse(substitute(source_name))
  }

  if (missing(target_name)) {
    target_name <- NULL
  } else {
    target_name <- deparse(substitute(target_name))
  }

  if (missing(value)) {
    value <- NULL
  } else {
    value <- deparse(substitute(value))
  }

  e_lines_3d_(
    e,
    deparse(substitute(source_lon)),
    deparse(substitute(source_lat)),
    deparse(substitute(target_lon)),
    deparse(substitute(target_lat)),
    source_name,
    target_name,
    value,
    name,
    coord_system,
    rm_x,
    rm_y,
    ...
  )
}

#' @export
#' @method e_lines_3d echarts4rProxy
e_lines_3d.echarts4rProxy <- function(
  e,
  source_lon,
  source_lat,
  target_lon,
  target_lat,
  source_name,
  target_name,
  value,
  name = NULL,
  coord_system = "globe",
  rm_x = TRUE,
  rm_y = TRUE,
  ...
) {
  if (missing(source_lat) || missing(source_lon) || missing(target_lat) || missing(target_lon)) {
    stop("missing coordinates", call. = FALSE)
  }
  if (missing(source_name)) {
    source_name <- NULL
  }

  if (missing(target_name)) {
    target_name <- NULL
  }

  if (missing(value)) {
    value <- NULL
  }

  e$chart <- e_lines_3d_(
    e$chart,
    deparse(substitute(source_lon)),
    deparse(substitute(source_lat)),
    deparse(substitute(target_lon)),
    deparse(substitute(target_lat)),
    deparse(substitute(source_name)),
    deparse(substitute(target_name)),
    deparse(substitute(value)),
    name,
    coord_system,
    rm_x,
    rm_y,
    ...
  )
  return(e)
}

#' @rdname line3D
#' @export
e_line_3d <- function(e, y, z, name = NULL, coord_system = NULL, rm_x = TRUE, rm_y = TRUE, ...) UseMethod("e_line_3d")

#' @export
#' @method e_line_3d echarts4r
e_line_3d.echarts4r <- function(e, y, z, name = NULL, coord_system = NULL, rm_x = TRUE, rm_y = TRUE, ...) {
  if (missing(e)) {
    stop("must pass e", call. = FALSE)
  }

  if (missing(y) || missing(z)) {
    stop("missing coordinates", call. = FALSE)
  }

  e_line_3d_(
    e,
    deparse(substitute(y)),
    deparse(substitute(z)),
    name,
    coord_system,
    rm_x,
    rm_y,
    ...
  )
}

#' @export
#' @method e_line_3d echarts4rProxy
e_line_3d.echarts4rProxy <- function(e, y, z, name = NULL, coord_system = NULL, rm_x = TRUE, rm_y = TRUE, ...) {
  if (missing(e)) {
    stop("must pass e", call. = FALSE)
  }

  if (missing(y) || missing(z)) {
    stop("missing coordinates", call. = FALSE)
  }

  e$chart <- e_line_3d_(
    e$chart,
    deparse(substitute(y)),
    deparse(substitute(z)),
    name,
    coord_system,
    rm_x,
    rm_y,
    ...
  )
  return(e)
}

#' Bar 3D
#'
#' Add 3D bars
#'
#' @inheritParams e_bar
#' @param y,z Coordinates.
#' @param bind Binding.
#' @param coord_system Coordinate system to use, one of \code{cartesian3D}, \code{geo3D}, \code{globe}.
#' @param rm_x,rm_y Whether to remove x and y axis, defaults to \code{TRUE}.
#'
#' @examples
#' \dontrun{
#' # volcano
#' volcano %>%
#'   as.table() %>%
#'   as.data.frame() %>%
#'   dplyr::mutate(
#'     Var1 = as.integer(Var1),
#'     Var2 = as.integer(Var2)
#'   ) %>%
#'   e_charts(Var1) %>%
#'   e_bar_3d(Var2, Freq) %>%
#'   e_visual_map(Freq)
#'
#' url <- paste0(
#'   "https://echarts.apache.org/examples/",
#'   "data-gl/asset/data/population.json"
#' )
#' data <- jsonlite::fromJSON(url)
#' data <- as.data.frame(data)
#' names(data) <- c("lon", "lat", "value")
#'
#' # globe
#' data %>%
#'   e_charts(lon) %>%
#'   e_globe() %>%
#'   e_bar_3d(lat, value, coord_system = "globe") %>%
#'   e_visual_map()
#'
#' # get3d
#' data %>%
#'   e_charts(lon) %>%
#'   e_geo_3d() %>%
#'   e_bar_3d(lat, value, coord_system = "geo3D") %>%
#'   e_visual_map()
#'
#' # stacked
#' v <- LETTERS[1:10]
#' matrix <- data.frame(
#'   x = sample(v, 300, replace = TRUE),
#'   y = sample(v, 300, replace = TRUE),
#'   z1 = rnorm(300, 10, 1),
#'   z2 = rnorm(300, 10, 1),
#'   stringsAsFactors = FALSE
#' ) %>%
#'   dplyr::group_by(x, y) %>%
#'   dplyr::summarise(
#'     z1 = sum(z1),
#'     z2 = sum(z2)
#'   ) %>%
#'   dplyr::ungroup()
#'
#' trans <- list(opacity = 0.4) # transparency
#' emphasis <- list(itemStyle = list(color = "#313695"))
#'
#' matrix %>%
#'   e_charts(x) %>%
#'   e_bar_3d(y, z1, stack = "stack", name = "Serie 1", itemStyle = trans, emphasis = emphasis) %>%
#'   e_bar_3d(y, z2, stack = "stack", name = "Serie 2", itemStyle = trans, emphasis = emphasis) %>%
#'   e_legend()
#'
#' # timeline
#' matrix %>%
#'   group_by(x) %>%
#'   e_charts(y, timeline = TRUE) %>%
#'   e_bar_3d(z1, z2) %>%
#'   e_visual_map(z2)
#' }
#'
#' @seealso \href{https://echarts.apache.org/en/option-gl.html#series-bar3D}{Additional arguments}
#'
#' @rdname e_bar_3d
#' @export
e_bar_3d <- function(
  e,
  y,
  z,
  bind,
  coord_system = "cartesian3D",
  name = NULL,
  rm_x = TRUE,
  rm_y = TRUE,
  ...
) {
  UseMethod("e_bar_3d")
}

#' @export
#' @method e_bar_3d echarts4r
e_bar_3d.echarts4r <- function(
  e,
  y,
  z,
  bind,
  coord_system = "cartesian3D",
  name = NULL,
  rm_x = TRUE,
  rm_y = TRUE,
  ...
) {
  if (missing(y) || missing(z)) {
    stop("must pass y and z", call. = FALSE)
  }

  if (missing(bind)) {
    bd <- NULL
  } else {
    bd <- deparse(substitute(bind))
  }

  e_bar_3d_(
    e,
    deparse(substitute(y)),
    deparse(substitute(z)),
    bd,
    coord_system,
    name,
    rm_x,
    rm_y,
    ...
  )
}

#' @export
#' @method e_bar_3d echarts4rProxy
e_bar_3d.echarts4rProxy <- function(
  e,
  y,
  z,
  bind,
  coord_system = "cartesian3D",
  name = NULL,
  rm_x = TRUE,
  rm_y = TRUE,
  ...
) {
  if (missing(y) || missing(z)) {
    stop("must pass y and z", call. = FALSE)
  }

  if (missing(bind)) {
    bd <- NULL
  } else {
    bd <- deparse(substitute(bind))
  }

  e$chart <- e_bar_3d_(
    e$chart,
    deparse(substitute(y)),
    deparse(substitute(z)),
    bd,
    coord_system,
    name,
    rm_x,
    rm_y,
    ...
  )
  return(e)
}

#' Surface
#'
#' Add a surface plot.
#'
#' @inheritParams e_bar
#' @param y,z Coordinates.
#' @param bind Binding.
#' @param rm_x,rm_y Whether to remove x and y axis, defaults to \code{TRUE}.
#'
#' @examples
#' data("volcano")
#'
#' surface <- as.data.frame(as.table(volcano))
#' surface$Var1 <- as.numeric(surface$Var1)
#' surface$Var2 <- as.numeric(surface$Var2)
#'
#' surface %>%
#'   e_charts(Var1) %>%
#'   e_surface(Var2, Freq) %>%
#'   e_visual_map(Freq)
#' @rdname e_surface
#' @export
e_surface <- function(
  e,
  y,
  z,
  bind,
  name = NULL,
  rm_x = TRUE,
  rm_y = TRUE,
  ...
) {
  UseMethod("e_surface")
}

#' @export
#' @method e_surface echarts4r
e_surface.echarts4r <- function(
  e,
  y,
  z,
  bind,
  name = NULL,
  rm_x = TRUE,
  rm_y = TRUE,
  ...
) {
  if (missing(e)) {
    stop("must pass e", call. = FALSE)
  }

  if (missing(y) || missing(z)) {
    stop("must pass y and z", call. = FALSE)
  }

  if (missing(bind)) {
    bd <- NULL
  } else {
    bd <- deparse(substitute(bind))
  }

  e_surface_(
    e,
    y = deparse(substitute(y)),
    z = deparse(substitute(z)),
    bind = bd,
    name = name,
    rm_x = rm_x,
    rm_y = rm_y,
    ...
  )
}

#' @export
#' @method e_surface echarts4rProxy
e_surface.echarts4rProxy <- function(
  e,
  y,
  z,
  bind,
  name = NULL,
  rm_x = TRUE,
  rm_y = TRUE,
  ...
) {
  if (missing(e)) {
    stop("must pass e", call. = FALSE)
  }

  if (missing(y) || missing(z)) {
    stop("must pass y and z", call. = FALSE)
  }

  if (missing(bind)) {
    bd <- NULL
  } else {
    bd <- deparse(substitute(bind))
  }

  e$chart <- e_surface_(
    e$chart,
    deparse(substitute(y)),
    deparse(substitute(z)),
    bd,
    name,
    rm_x,
    rm_y,
    ...
  )
  return(e)
}

#' Lines
#'
#' Add lines.
#'
#' @inheritParams e_bar
#' @param source_lon,source_lat,target_lon,target_lat coordinates.
#' @param source_name,target_name Names of source and target.
#' @param value Value of edges.
#' @param coord_system Coordinate system to use, one of \code{geo}, or \code{cartesian2d}.
#' @param rm_x,rm_y Whether to remove x and y axis, defaults to \code{TRUE}.
#'
#' @examples
#' flights <- read.csv(
#'   paste0(
#'     "https://raw.githubusercontent.com/plotly/datasets/",
#'     "master/2011_february_aa_flight_paths.csv"
#'   )
#' )
#'
#' flights %>%
#'   e_charts() %>%
#'   e_geo() %>%
#'   e_lines(
#'     start_lon,
#'     start_lat,
#'     end_lon,
#'     end_lat,
#'     airport1,
#'     airport2,
#'     cnt,
#'     name = "flights",
#'     lineStyle = list(normal = list(curveness = 0.3))
#'   ) %>%
#'   e_tooltip(
#'     trigger = "item",
#'     formatter = htmlwidgets::JS("
#'       function(params){
#'         return(
#'           params.seriesName +'<br />' +
#'           params.data.source_name + ' -> ' +
#'           params.data.target_name + ':'+ params.value
#'         )
#'       }
#'    ")
#'   )
#'
#' # timeline
#' flights$grp <- rep(LETTERS[1:2], 89)
#'
#' flights %>%
#'   group_by(grp) %>%
#'   e_charts(timeline = TRUE) %>%
#'   e_geo() %>%
#'   e_lines(
#'     start_lon,
#'     start_lat,
#'     end_lon,
#'     end_lat,
#'     cnt,
#'     coord_system = "geo"
#'   )
#' @seealso \href{https://echarts.apache.org/en/option.html#series-lines}{Additional arguments}
#'
#' @rdname e_lines
#' @export
e_lines <- function(
  e,
  source_lon,
  source_lat,
  target_lon,
  target_lat,
  source_name,
  target_name,
  value,
  coord_system = "geo",
  name = NULL,
  rm_x = TRUE,
  rm_y = TRUE,
  ...
) {
  UseMethod("e_lines")
}

#' @export
#' @method e_lines echarts4r
e_lines.echarts4r <- function(
  e,
  source_lon,
  source_lat,
  target_lon,
  target_lat,
  source_name,
  target_name,
  value,
  coord_system = "geo",
  name = NULL,
  rm_x = TRUE,
  rm_y = TRUE,
  ...
) {
  if (missing(e)) {
    stop("must pass e", call. = FALSE)
  }

  if (missing(source_lat) || missing(source_lon) || missing(target_lat) || missing(target_lon)) {
    stop("missing coordinates", call. = FALSE)
  }

  if (missing(source_name)) {
    source_name <- NULL
  } else {
    source_name <- deparse(substitute(source_name))
  }

  if (missing(target_name)) {
    target_name <- NULL
  } else {
    target_name <- deparse(substitute(target_name))
  }

  if (missing(value)) {
    value <- NULL
  } else {
    value <- deparse(substitute(value))
  }

  e_lines_(
    e,
    deparse(substitute(source_lon)),
    deparse(substitute(source_lat)),
    deparse(substitute(target_lon)),
    deparse(substitute(target_lat)),
    source_name,
    target_name,
    value,
    coord_system,
    name,
    rm_x,
    rm_y,
    ...
  )
}

#' @export
#' @method e_lines echarts4rProxy
e_lines.echarts4rProxy <- function(
  e,
  source_lon,
  source_lat,
  target_lon,
  target_lat,
  source_name,
  target_name,
  value,
  coord_system = "geo",
  name = NULL,
  rm_x = TRUE,
  rm_y = TRUE,
  ...
) {
  if (missing(e)) {
    stop("must pass e", call. = FALSE)
  }

  if (missing(source_lat) || missing(source_lon) || missing(target_lat) || missing(target_lon)) {
    stop("missing coordinates", call. = FALSE)
  }

  if (missing(source_name)) {
    source_name <- NULL
  }

  if (missing(target_name)) {
    target_name <- NULL
  }

  if (missing(value)) {
    value <- NULL
  }

  e$chart <- e_lines_(
    e$chart,
    deparse(substitute(source_lon)),
    deparse(substitute(source_lat)),
    deparse(substitute(target_lon)),
    deparse(substitute(target_lat)),
    deparse(substitute(source_name)),
    deparse(substitute(target_name)),
    deparse(substitute(value)),
    coord_system,
    name,
    rm_x,
    rm_y,
    ...
  )

  return(e)
}

#' Scatter 3D
#'
#' Add 3D scatter.
#'
#' @inheritParams e_bar
#' @param y,z Coordinates.
#' @param bind Binding.
#' @param color,size Color and Size of bubbles.
#' @param coord_system Coordinate system to use, one of \code{geo3D}, \code{globe}, or \code{cartesian3D}.
#' @param rm_x,rm_y Whether to remove x and y axis, defaults to \code{TRUE}.
#'
#' @examples
#' v <- LETTERS[1:10]
#' matrix <- data.frame(
#'   x = sample(v, 300, replace = TRUE),
#'   y = sample(v, 300, replace = TRUE),
#'   z = rnorm(300, 10, 1),
#'   color = rnorm(300, 10, 1),
#'   size = rnorm(300, 10, 1),
#'   stringsAsFactors = FALSE
#' ) %>%
#'   dplyr::group_by(x, y) %>%
#'   dplyr::summarise(
#'     z = sum(z),
#'     color = sum(color),
#'     size = sum(size)
#'   ) %>%
#'   dplyr::ungroup()
#'
#' matrix %>%
#'   e_charts(x) %>%
#'   e_scatter_3d(y, z, size, color) %>%
#'   e_visual_map(
#'     min = 1,
#'     max = 100,
#'     inRange = list(symbolSize = c(1, 30)),
#'     # scale size
#'     dimension = 3 # third dimension 0 = x, y = 1, z = 2, size = 3
#'   ) %>%
#'   e_visual_map(
#'     min = 1,
#'     max = 100,
#'     inRange = list(color = c("#bf444c", "#d88273", "#f6efa6")),
#'     # scale colors
#'     dimension = 4,
#'     # third dimension 0 = x, y = 1, z = 2, size = 3, color = 4
#'     bottom = 300 # padding to avoid visual maps overlap
#'   )
#'
#' airports <- read.csv(
#'   paste0(
#'     "https://raw.githubusercontent.com/plotly/datasets/",
#'     "master/2011_february_us_airport_traffic.csv"
#'   )
#' )
#'
#' airports %>%
#'   e_charts(long) %>%
#'   e_globe(
#'     globeOuterRadius = 100
#'   ) %>%
#'   e_scatter_3d(lat, cnt, coord_system = "globe", blendMode = "lighter") %>%
#'   e_visual_map(inRange = list(symbolSize = c(1, 10)))
#'
#' # timeline
#' airports %>%
#'   group_by(state) %>%
#'   e_charts(long, timeline = TRUE) %>%
#'   e_globe(
#'     globeOuterRadius = 100
#'   ) %>%
#'   e_scatter_3d(lat, cnt, coord_system = "globe", blendMode = "lighter") %>%
#'   e_visual_map(inRange = list(symbolSize = c(1, 10)))
#' @seealso \href{https://echarts.apache.org/en/option-gl.html#series-scatter3D}{Additional arguments}
#'
#' @rdname e_scatter_3d
#' @export
e_scatter_3d <- function(
  e,
  y,
  z,
  color,
  size,
  bind,
  coord_system = "cartesian3D",
  name = NULL,
  rm_x = TRUE,
  rm_y = TRUE,
  legend = FALSE,
  ...
) {
  UseMethod("e_scatter_3d")
}

#' @export
#' @method e_scatter_3d echarts4r
e_scatter_3d.echarts4r <- function(
  e,
  y,
  z,
  color,
  size,
  bind,
  coord_system = "cartesian3D",
  name = NULL,
  rm_x = TRUE,
  rm_y = TRUE,
  legend = FALSE,
  ...
) {
  if (missing(y) || missing(z)) {
    stop("must pass y and z", call. = FALSE)
  }

  if (!missing(color)) {
    colour <- deparse(substitute(color))
  } else {
    colour <- NULL
  }

  if (!missing(size)) {
    sz <- deparse(substitute(size))
  } else {
    sz <- NULL
  }

  if (!missing(bind)) {
    bd <- deparse(substitute(bind))
  } else {
    bd <- NULL
  }

  e_scatter_3d_(
    e,
    deparse(substitute(y)),
    deparse(substitute(z)),
    colour,
    sz,
    bd,
    coord_system,
    name,
    rm_x,
    rm_y,
    legend,
    ...
  )
}

#' @export
#' @method e_scatter_3d echarts4rProxy
e_scatter_3d.echarts4rProxy <- function(
  e,
  y,
  z,
  color,
  size,
  bind,
  coord_system = "cartesian3D",
  name = NULL,
  rm_x = TRUE,
  rm_y = TRUE,
  legend = FALSE,
  ...
) {
  if (missing(y) || missing(z)) {
    stop("must pass y and z", call. = FALSE)
  }

  if (!missing(color)) {
    colour <- deparse(substitute(color))
  } else {
    colour <- NULL
  }

  if (!missing(size)) {
    sz <- deparse(substitute(size))
  } else {
    sz <- NULL
  }

  if (!missing(bind)) {
    bd <- deparse(substitute(bind))
  } else {
    bd <- NULL
  }

  e$chart <- e_scatter_3d_(
    e$chart,
    deparse(substitute(y)),
    deparse(substitute(z)),
    colour,
    sz,
    bd,
    coord_system,
    name,
    rm_x,
    rm_y,
    legend,
    ...
  )

  return(e)
}

#' Flow GL
#'
#' @inheritParams e_bar
#' @param y Vector position on the y axis.
#' @param sx,sy Velocity in respective axis.
#' @param color Vector color.
#' @param coord_system Coordinate system to use.
#' @param rm_x,rm_y Whether to remove x and y axis, only applies if \code{coord_system} is not \code{null}.
#'
#' @examples
#' # coordinates
#' vectors <- expand.grid(0:9, 0:9)
#' names(vectors) <- c("x", "y")
#' vectors$sx <- rnorm(100)
#' vectors$sy <- rnorm(100)
#' vectors$color <- log10(runif(100, 1, 10))
#'
#' vectors %>%
#'   e_charts(x) %>%
#'   e_flow_gl(y, sx, sy, color) %>%
#'   e_visual_map(
#'     min = 0,
#'     max = 1,
#'     # log 10
#'     dimension = 4,
#'     # x = 0, y = 1, sx = 3, sy = 4
#'     show = FALSE,
#'     # hide
#'     inRange = list(
#'       color = c(
#'         "#313695",
#'         "#4575b4",
#'         "#74add1",
#'         "#abd9e9",
#'         "#e0f3f8",
#'         "#ffffbf",
#'         "#fee090",
#'         "#fdae61",
#'         "#f46d43",
#'         "#d73027",
#'         "#a50026"
#'       )
#'     )
#'   ) %>%
#'   e_x_axis(
#'     splitLine = list(show = FALSE)
#'   ) %>%
#'   e_y_axis(
#'     splitLine = list(show = FALSE)
#'   )
#'
#' # map
#' latlong <- seq(-180, 180, by = 5)
#' wind <- expand.grid(lng = latlong, lat = latlong)
#' wind$slng <- rnorm(nrow(wind), 0, 200)
#' wind$slat <- rnorm(nrow(wind), 0, 200)
#' wind$color <- abs(wind$slat) - abs(wind$slng)
#' rng <- range(wind$color)
#'
#' trans <- list(opacity = 0.5) # transparency
#'
#' wind %>%
#'   e_charts(lng, backgroundColor = "#333") %>%
#'   e_geo() %>%
#'   e_flow_gl(
#'     lat,
#'     slng,
#'     slat,
#'     color,
#'     itemStyle = trans,
#'     particleSize = 2
#'   ) %>%
#'   e_visual_map(
#'     color,
#'     # range
#'     dimension = 4,
#'     # lng = 0, lat = 1, slng = 2, slat = 3, color = 4
#'     show = FALSE,
#'     # hide
#'     inRange = list(
#'       color = c(
#'         "#313695",
#'         "#4575b4",
#'         "#74add1",
#'         "#abd9e9",
#'         "#e0f3f8",
#'         "#ffffbf",
#'         "#fee090",
#'         "#fdae61",
#'         "#f46d43",
#'         "#d73027",
#'         "#a50026"
#'       )
#'     )
#'   ) %>%
#'   e_x_axis(show = FALSE) %>%
#'   e_y_axis(show = FALSE)
#' @seealso \href{https://echarts.apache.org/en/option-gl.html#series-flowGL}{Additional arguments}
#'
#' @rdname e_flow_gl
#' @export
e_flow_gl <- function(e, y, sx, sy, color, name = NULL, coord_system = NULL, rm_x = TRUE, rm_y = TRUE, ...) UseMethod("e_flow_gl")

#' @export
#' @method e_flow_gl echarts4r
e_flow_gl.echarts4r <- function(e, y, sx, sy, color, name = NULL, coord_system = NULL, rm_x = TRUE, rm_y = TRUE, ...) {
  if (missing(y) || missing(sx) || missing(sy)) {
    stop("must pass y and z", call. = FALSE)
  }

  if (!missing(color)) {
    colour <- deparse(substitute(color))
  } else {
    colour <- NULL
  }

  e_flow_gl_(
    e = e,
    y = deparse(substitute(y)),
    sx = deparse(substitute(sx)),
    sy = deparse(substitute(sy)),
    color = colour,
    name,
    coord_system,
    rm_x,
    rm_y,
    ...
  )
}

#' @export
#' @method e_flow_gl echarts4rProxy
e_flow_gl.echarts4rProxy <- function(e, y, sx, sy, color, name = NULL, coord_system = NULL, rm_x = TRUE, rm_y = TRUE, ...) {
  if (missing(y) || missing(sx) || missing(sy)) {
    stop("must pass y and z", call. = FALSE)
  }

  if (!missing(color)) {
    colour <- deparse(substitute(color))
  } else {
    colour <- NULL
  }

  e$chart <- e_flow_gl_(
    e = e$chart,
    y = deparse(substitute(y)),
    sx = deparse(substitute(sx)),
    sy = deparse(substitute(sy)),
    color = colour,
    name,
    coord_system,
    rm_x,
    rm_y,
    ...
  )
  return(e)
}

#' Scatter GL
#'
#' Draw scatter GL.
#'
#' @inheritParams e_bar
#' @param y,z Column names containing y and z data.
#' @param coord_system Coordinate system to plot against.
#' @param rm_x,rm_y Whether to remove x and y axis, defaults to \code{TRUE}.
#'
#' @examples
#' quakes %>%
#'   e_charts(long) %>%
#'   e_geo(
#'     roam = TRUE,
#'     boundingCoords = list(
#'       c(185, -10),
#'       c(165, -40)
#'     )
#'   ) %>%
#'   e_scatter_gl(lat, depth)
#'
#' # timeline
#' quakes$year <- rep(c("2017", "2018"), 500)
#'
#' quakes %>%
#'   group_by(year) %>%
#'   e_charts(long, timeline = TRUE) %>%
#'   e_geo(
#'     roam = TRUE,
#'     boundingCoords = list(
#'       c(185, -10),
#'       c(165, -40)
#'     )
#'   ) %>%
#'   e_scatter_gl(lat, depth)
#' @seealso \href{https://echarts.apache.org/en/option-gl.html#series-scatterGL}{Additional arguments}
#'
#' @rdname e_scatter_gl
#' @export
e_scatter_gl <- function(e, y, z, name = NULL, coord_system = "geo", rm_x = TRUE, rm_y = TRUE, ...) UseMethod("e_scatter_gl")

#' @export
#' @method e_scatter_gl echarts4r
e_scatter_gl.echarts4r <- function(e, y, z, name = NULL, coord_system = "geo", rm_x = TRUE, rm_y = TRUE, ...) {
  if (missing(y) || missing(z)) {
    stop("must pass y and z", call. = FALSE)
  }

  z_serie <- NULL
  if (!missing(z)) {
    z_serie <- deparse(substitute(z))
  }

  e_scatter_gl_(
    e,
    deparse(substitute(y)),
    z_serie,
    name,
    coord_system,
    rm_x,
    rm_y,
    ...
  )
}

#' @export
#' @method e_scatter_gl echarts4rProxy
e_scatter_gl.echarts4rProxy <- function(e, y, z, name = NULL, coord_system = "geo", rm_x = TRUE, rm_y = TRUE, ...) {
  if (missing(y) || missing(z)) {
    stop("must pass y and z", call. = FALSE)
  }

  e$chart <- e_scatter_gl_(
    e$chart,
    deparse(substitute(y)),
    deparse(substitute(z)),
    name,
    coord_system,
    rm_x,
    rm_y,
    ...
  )
  return(e)
}

#' Pictorial
#'
#' Pictorial bar chart is a type of bar chart that custimzed glyph
#' (like images, SVG PathData) can be used instead of rectangular bar.
#'
#' @inheritParams e_bar
#' @param symbol Symbol to plot.
#'
#' @section Symbols:
#' \itemize{
#'   \item{Built-in}{ \code{circle}, \code{rect}, \code{roundRect}, \code{triangle}, \code{diamond},
#'   \code{pin}, \code{arrow}.}
#'   \item{SVG Path}
#'   \item{Images}{ Path to image, don't forget to precede it with \code{image://}, see examples.}
#' }
#'
#' @examples
#' # built-in symbols
#' y <- rnorm(10, 10, 2)
#' df <- data.frame(
#'   x = 1:10,
#'   y = y,
#'   z = y - rnorm(10, 5, 1)
#' )
#'
#' df %>%
#'   e_charts(x) %>%
#'   e_bar(z, barWidth = 10) %>%
#'   e_pictorial(
#'     y,
#'     symbol = "rect",
#'     symbolRepeat = TRUE,
#'     z = -1,
#'     symbolSize = c(10, 4)
#'   ) %>%
#'   e_theme("westeros")
#'
#' # svg path
#' path <- "path://M0,10 L10,10 C5.5,10 5.5,5 5,0 C4.5,5 4.5,10 0,10 z"
#'
#' style <- list(
#'   normal = list(opacity = 0.5),
#'   # normal
#'   emphasis = list(opacity = 1) # on hover
#' )
#'
#' df %>%
#'   e_charts(x) %>%
#'   e_pictorial(
#'     y,
#'     symbol = path,
#'     barCategoryGap = "-130%",
#'     itemStyle = style
#'   )
#'
#' # image
#' # might not work in RStudio viewer
#' # open in browser
#' qomo <- paste0(
#'   "https://ecomfe.github.io/echarts-examples/public/",
#'   "data/asset/img/hill-Qomolangma.png"
#' )
#'
#' kili <- paste0(
#'   "https://ecomfe.github.io/echarts-examples/public/",
#'   "data/asset/img/hill-Kilimanjaro.png"
#' )
#'
#' data <- data.frame(
#'   x = c("Qomolangma", "Kilimanjaro"),
#'   value = c(8844, 5895),
#'   symbol = c(
#'     paste0("image://", qomo),
#'     paste0("image://", kili)
#'   )
#' )
#'
#' data %>%
#'   e_charts(x) %>%
#'   e_pictorial(value, symbol) %>%
#'   e_legend(FALSE)
#'
#' # timeline
#' df <- data.frame(
#'   x = rep(1:5, 2),
#'   y = runif(10, 1, 10),
#'   year = c(
#'     rep(2017, 5),
#'     rep(2018, 5)
#'   )
#' )
#'
#' df %>%
#'   group_by(year) %>%
#'   e_charts(x, timeline = TRUE) %>%
#'   e_pictorial(
#'     y,
#'     symbol = "rect",
#'     symbolRepeat = TRUE,
#'     z = -1,
#'     symbolSize = c(10, 4)
#'   )
#' @seealso \href{https://echarts.apache.org/en/option.html#series-pictorialBar}{Additional arguments}
#'
#' @rdname e_pictorial
#' @export
e_pictorial <- function(e, serie, symbol, bind, name = NULL, legend = TRUE, y_index = 0, x_index = 0, ...) UseMethod("e_pictorial")

#' @export
#' @method e_pictorial echarts4r
e_pictorial.echarts4r <- function(e, serie, symbol, bind, name = NULL, legend = TRUE, y_index = 0, x_index = 0, ...) {
  if (missing(serie) || missing(symbol)) {
    stop("must pass serie and symbol", call. = FALSE)
  }

  if (!missing(bind)) {
    bd <- deparse(substitute(bind))
  } else {
    bd <- NULL
  }

  # only deparse if it is a column name
  if (deparse(substitute(symbol)) %in% colnames(e$x$data[[1]])) {
    symbol <- deparse(substitute(symbol))
  }

  e_pictorial_(
    e,
    deparse(substitute(serie)),
    symbol,
    bd,
    name,
    legend,
    y_index,
    x_index,
    ...
  )
}

#' @export
#' @method e_pictorial echarts4rProxy
e_pictorial.echarts4rProxy <- function(e, serie, symbol, bind, name = NULL, legend = TRUE, y_index = 0, x_index = 0, ...) {
  if (missing(serie) || missing(symbol)) {
    stop("must pass serie and symbol", call. = FALSE)
  }

  if (!missing(bind)) {
    bd <- deparse(substitute(bind))
  } else {
    bd <- NULL
  }

  # only deparse if it is a column name
  if (deparse(substitute(symbol)) %in% colnames(e$chart$x$data[[1]])) {
    symbol <- deparse(substitute(symbol))
  }

  e$chart <- e_pictorial_(
    e$chart,
    deparse(substitute(serie)),
    symbol,
    bd,
    name,
    legend,
    y_index,
    x_index,
    ...
  )
  return(e)
}

#' Smooth
#'
#' Plot formulas.
#'
#' @inheritParams e_bar
#' @param formula formula to pass to \code{\link{lm}}.
#' @param symbol Symbol to use in \code{\link{e_line}}.
#' @param smooth Whether to smooth the line.
#' @param model_args Arguments to pass to the underlying model.
#' @param ... Additional arguments to pass to \code{\link{e_line}}.
#'
#' @examples
#' iris %>%
#'   group_by(Species) %>%
#'   e_charts(Sepal.Length) %>%
#'   e_scatter(Sepal.Width) %>%
#'   e_lm(Sepal.Width ~ Sepal.Length) %>%
#'   e_x_axis(min = 4)
#'
#' mtcars %>%
#'   e_charts(disp) %>%
#'   e_scatter(mpg, qsec) %>%
#'   e_loess(mpg ~ disp, smooth = TRUE, showSymbol = FALSE)
#'
#' # timeline
#' iris %>%
#'   group_by(Species) %>%
#'   e_charts(Sepal.Length, timeline = TRUE) %>%
#'   e_scatter(Sepal.Width) %>%
#'   e_lm(Sepal.Width ~ Sepal.Length) %>%
#'   e_x_axis(min = 4, max = 8) %>%
#'   e_y_axis(max = 5)
#' @rdname smooth
#' @export
e_lm <- function(e, formula, name = NULL, legend = TRUE, symbol = "none", smooth = TRUE, model_args = list(), ...) UseMethod("e_lm")

#' @export
#' @method e_lm echarts4r
#' @importFrom stats complete.cases
e_lm.echarts4r <- function(e, formula, name = NULL, legend = TRUE, symbol = "none", smooth = TRUE, model_args = list(), ...) {
  form <- as.formula(formula)

  for (i in seq_along(e$x$data)) {
    e$x$data[[i]] <- e$x$data[[i]][stats::complete.cases(e$x$data[[i]]), ]

    model_args$formula <- form
    model_args$data <- e$x$data[[i]]

    model <- tryCatch(
      do.call(lm, model_args),
      error = function(e) e
    )

    if (!inherits(model, "error")) {
      data <- broom::augment(model)
      data <- data %>% dplyr::select(-dplyr::one_of(names(model$model)))

      e$x$data[[i]] <- dplyr::bind_cols(e$x$data[[i]], data)

      vector <- .build_data2(e$x$data[[i]], e$x$mapping$x, ".fitted")

      l_data <- list(data = vector)

      l <- list(
        name = name,
        type = "line",
        symbol = symbol,
        smooth = smooth,
        ...
      )

      if (!e$x$tl) {
        if (is.null(name)) {
          nm <- paste0(names(e$x$data[[i]])[i], "-lm")
        } else {
          nm <- name
        }

        l$name <- nm

        l <- append(l, l_data)

        e$x$opts$series <- append(e$x$opts$series, list(l))

        if (isTRUE(legend)) {
          e$x$opts$legend$data <- append(e$x$opts$legend$data, list(nm))
        }
      } else {
        e$x$opts$options[[i]]$series <- append(e$x$opts$options[[i]]$series, list(l_data))
      }
    }
  }

  if (isTRUE(e$x$tl) && !inherits(model, "error")) {
    if (isTRUE(legend) && !is.null(name)) {
      e$x$opts$baseOption$legend$data <- append(e$x$opts$baseOption$legend$data, list(name))
    }

    e$x$opts$baseOption$series <- append(e$x$opts$baseOption$series, list(l))
  }

  e
}

#' @export
#' @method e_lm echarts4rProxy
#' @importFrom stats complete.cases
e_lm.echarts4rProxy <- function(e, formula, name = NULL, legend = TRUE, symbol = "none", smooth = TRUE, model_args = list(), ...) {
  form <- as.formula(formula)

  for (i in seq_along(e$chart$x$data)) {
    e$chart$x$data[[i]] <- e$chart$x$data[[i]][stats::complete.cases(e$chart$x$data[[i]]), ]

    model_args$formula <- form
    model_args$data <- e$chart$x$data[[i]]

    model <- tryCatch(
      do.call(lm, model_args),
      error = function(e) e
    )

    if (!inherits(model, "error")) {
      data <- broom::augment(model)
      data <- data %>% dplyr::select(-dplyr::one_of(names(model$model)))

      e$chart$x$data[[i]] <- dplyr::bind_cols(e$chart$x$data[[i]], data)

      vector <- .build_data2(e$chart$x$data[[i]], e$chart$x$mapping$x, ".fitted")

      l_data <- list(data = vector)

      l <- list(
        name = name,
        type = "line",
        symbol = symbol,
        smooth = smooth,
        ...
      )

      if (!e$chart$x$tl) {
        if (is.null(name)) {
          nm <- paste0(names(e$chart$x$data[[i]])[i], "-lm")
        } else {
          nm <- name
        }

        l$name <- nm

        l <- append(l, l_data)

        e$chart$x$opts$series <- append(e$chart$x$opts$series, list(l))

        if (isTRUE(legend)) {
          e$chart$x$opts$legend$data <- append(e$chart$x$opts$legend$data, list(nm))
        }
      } else {
        e$chart$x$opts$options[[i]]$series <- append(e$chart$x$opts$options[[i]]$series, list(l_data))
      }
    }
  }

  if (isTRUE(e$chart$x$tl) && !inherits(model, "error")) {
    if (isTRUE(legend) && !is.null(name)) {
      e$chart$x$opts$baseOption$legend$data <- append(e$chart$x$opts$baseOption$legend$data, list(name))
    }

    e$chart$x$opts$baseOption$series <- append(e$chart$x$opts$baseOption$series, list(l))
  }

  e
}

#' @rdname smooth
#' @export
e_glm <- function(e, formula, name = NULL, legend = TRUE, symbol = "none", smooth = TRUE, model_args = list(), ...) UseMethod("e_glm")

#' @export
#' @method e_glm echarts4r
#' @importFrom stats complete.cases
e_glm.echarts4r <- function(e, formula, name = NULL, legend = TRUE, symbol = "none", smooth = TRUE, model_args = list(), ...) {
  form <- as.formula(formula)

  for (i in seq_along(e$x$data)) {
    e$x$data[[i]] <- e$x$data[[i]][stats::complete.cases(e$x$data[[i]]), ]

    model_args$formula <- form
    model_args$data <- e$x$data[[i]]

    model <- tryCatch(
      do.call(glm, model_args),
      error = function(e) e
    )

    if (!inherits(model, "error")) {
      data <- broom::augment(model)
      data <- data %>% dplyr::select(-dplyr::one_of(names(model$model)))

      e$x$data[[i]] <- dplyr::bind_cols(e$x$data[[i]], data)

      vector <- .build_data2(e$x$data[[i]], e$x$mapping$x, ".fitted")

      l_data <- list(data = vector)

      l <- list(
        name = name,
        type = "line",
        symbol = symbol,
        smooth = smooth,
        ...
      )

      if (!e$x$tl) {
        if (is.null(name)) {
          nm <- paste0(names(e$x$data[[i]])[i], "-glm")
        } else {
          nm <- name
        }

        l$name <- nm

        l <- append(l, l_data)

        e$x$opts$series <- append(e$x$opts$series, list(l))

        if (isTRUE(legend)) {
          e$x$opts$legend$data <- append(e$x$opts$legend$data, list(nm))
        }
      } else {
        e$x$opts$options[[i]]$series <- append(e$x$opts$options[[i]]$series, list(l_data))
      }
    }
  }

  if (isTRUE(e$x$tl) && !inherits(model, "error")) {
    if (isTRUE(legend) && !is.null(name)) {
      e$x$opts$baseOption$legend$data <- append(e$x$opts$baseOption$legend$data, list(name))
    }

    e$x$opts$baseOption$series <- append(e$x$opts$baseOption$series, list(l))
  }

  e
}

#' @export
#' @method e_glm echarts4rProxy
#' @importFrom stats complete.cases
e_glm.echarts4rProxy <- function(e, formula, name = NULL, legend = TRUE, symbol = "none", smooth = TRUE, model_args = list(), ...) {
  form <- as.formula(formula)

  for (i in seq_along(e$chart$x$data)) {
    e$chart$x$data[[i]] <- e$chart$x$data[[i]][stats::complete.cases(e$chart$x$data[[i]]), ]

    model_args$formula <- form
    model_args$data <- e$chart$x$data[[i]]

    model <- tryCatch(
      do.call(glm, model_args),
      error = function(e) e
    )

    if (!inherits(model, "error")) {
      data <- broom::augment(model)
      data <- data %>% dplyr::select(-dplyr::one_of(names(model$model)))

      e$chart$x$data[[i]] <- dplyr::bind_cols(e$chart$x$data[[i]], data)

      vector <- .build_data2(e$chart$x$data[[i]], e$chart$x$mapping$x, ".fitted")

      l_data <- list(data = vector)

      l <- list(
        name = name,
        type = "line",
        symbol = symbol,
        smooth = smooth,
        ...
      )

      if (!e$chart$x$tl) {
        if (is.null(name)) {
          nm <- paste0(names(e$chart$x$data[[i]])[i], "-glm")
        } else {
          nm <- name
        }

        l$name <- nm

        l <- append(l, l_data)

        e$chart$x$opts$series <- append(e$chart$x$opts$series, list(l))

        if (isTRUE(legend)) {
          e$chart$x$opts$legend$data <- append(e$chart$x$opts$legend$data, list(nm))
        }
      } else {
        e$chart$x$opts$options[[i]]$series <- append(e$chart$x$opts$options[[i]]$series, list(l_data))
      }
    }
  }

  if (isTRUE(e$chart$x$tl) && !inherits(model, "error")) {
    if (isTRUE(legend) && !is.null(name)) {
      e$chart$x$opts$baseOption$legend$data <- append(e$chart$x$opts$baseOption$legend$data, list(name))
    }

    e$chart$x$opts$baseOption$series <- append(e$chart$x$opts$baseOption$series, list(l))
  }

  e
}

#' @rdname smooth
#' @export
e_loess <- function(
  e,
  formula,
  name = NULL,
  legend = TRUE,
  symbol = "none",
  smooth = TRUE,
  x_index = 0,
  y_index = 0,
  model_args = list(),
  ...
) {
  UseMethod("e_loess")
}

#' @export
#' @method e_loess echarts4r
#' @importFrom stats complete.cases
e_loess.echarts4r <- function(
  e,
  formula,
  name = NULL,
  legend = TRUE,
  symbol = "none",
  smooth = TRUE,
  x_index = 0,
  y_index = 0,
  model_args = list(),
  ...
) {
  for (i in seq_along(e$x$data)) {
    e$x$data[[i]] <- e$x$data[[i]][stats::complete.cases(e$x$data[[i]]), ]

    model_args$formula <- as.formula(formula)
    model_args$data <- e$x$data[[i]]

    mod <- tryCatch(
      do.call(loess, model_args),
      error = function(e) e
    )

    if (!inherits(mod, "error")) {
      ser <- "ECHARTS4RLOESS"

      e$x$data[[i]][, ser] <- predict(mod)

      vector <- .build_data2(
        e$x$data[[i]],
        e$x$mapping$x,
        ser
      )

      l_data <- list(data = vector)

      l_opts <- list(
        type = "line",
        yAxisIndex = y_index,
        xAxisIndex = x_index,
        name = name,
        ...
      )

      if (y_index != 0) {
        e <- .set_y_axis(e, name, y_index)
      }

      if (x_index != 0) {
        e <- .set_x_axis(e, x_index)
      }

      if (!e$x$tl) {
        if (is.null(name)) {
          nm <- paste0(names(e$x$data[[i]])[i], "-loess")
        } else {
          nm <- name
        }

        l_opts$name <- nm

        l_data <- append(l_data, l_opts)

        if (isTRUE(legend) && !is.null(nm)) {
          e$x$opts$legend$data <- append(e$x$opts$legend$data, list(nm))
        }

        e$x$opts$series <- append(e$x$opts$series, list(l_data))
      } else {
        e$x$opts$options[[i]]$series <- append(e$x$opts$options[[i]]$series, list(l_data))
      }
    }
  }

  if (isTRUE(e$x$tl) && !inherits(mod, "error")) {
    if (isTRUE(legend) && !is.null(name)) {
      e$x$opts$baseOption$legend$data <- append(e$x$opts$baseOption$legend$data, list(name))
    }

    e$x$opts$baseOption$series <- append(e$x$opts$baseOption$series, list(l_opts))
  }

  e
}

#' @export
#' @method e_loess echarts4rProxy
#' @importFrom stats complete.cases
e_loess.echarts4rProxy <- function(
  e,
  formula,
  name = NULL,
  legend = TRUE,
  symbol = "none",
  smooth = TRUE,
  x_index = 0,
  y_index = 0,
  model_args = list(),
  ...
) {
  for (i in seq_along(e$chart$x$data)) {
    e$chart$x$data[[i]] <- e$chart$x$data[[i]][stats::complete.cases(e$chart$x$data[[i]]), ]

    model_args$formula <- as.formula(formula)
    model_args$data <- e$chart$x$data[[i]]

    mod <- tryCatch(
      do.call(loess, model_args),
      error = function(e) e
    )

    if (!inherits(mod, "error")) {
      ser <- "ECHARTS4RLOESS"

      e$chart$x$data[[i]][, ser] <- predict(mod)

      vector <- .build_data2(
        e$chart$x$data[[i]],
        e$chart$x$mapping$x,
        ser
      )

      l_data <- list(data = vector)

      l_opts <- list(
        type = "line",
        yAxisIndex = y_index,
        xAxisIndex = x_index,
        name = name,
        ...
      )

      if (y_index != 0) {
        e <- .set_y_axis(e, name, y_index)
      }

      if (x_index != 0) {
        e <- .set_x_axis(e, x_index)
      }

      if (!e$chart$x$tl) {
        if (is.null(name)) {
          nm <- paste0(names(e$chart$x$data[[i]])[i], "-loess")
        } else {
          nm <- name
        }

        l_opts$name <- nm

        l_data <- append(l_data, l_opts)

        if (isTRUE(legend) && !is.null(nm)) {
          e$chart$x$opts$legend$data <- append(e$chart$x$opts$legend$data, list(nm))
        }

        e$chart$x$opts$series <- append(e$chart$x$opts$series, list(l_data))
      } else {
        e$chart$x$opts$options[[i]]$series <- append(e$chart$x$opts$options[[i]]$series, list(l_data))
      }
    }
  }

  if (isTRUE(e$chart$x$tl) && !inherits(mod, "error")) {
    if (isTRUE(legend) && !is.null(name)) {
      e$chart$x$opts$baseOption$legend$data <- append(e$chart$x$opts$baseOption$legend$data, list(name))
    }

    e$chart$x$opts$baseOption$series <- append(e$chart$x$opts$baseOption$series, list(l_opts))
  }

  e
}

#' Histogram & Density
#'
#' Add a histogram or density plots.
#'
#' @inheritParams e_bar
#' @param bar_width Width of bars.
#' @param breaks Passed to \code{\link{hist}}.
#' @param smooth Whether to use smoothed lines, passed to \code{\link{e_line}}.
#'
#' @examples
#' mtcars %>%
#'   e_charts() %>%
#'   e_histogram(mpg, name = "histogram") %>%
#'   e_density(mpg, areaStyle = list(opacity = .4), smooth = TRUE, name = "density", y_index = 1) %>%
#'   e_tooltip(trigger = "axis")
#'
#' # timeline
#' mtcars %>%
#'   group_by(cyl) %>%
#'   e_charts(timeline = TRUE) %>%
#'   e_histogram(mpg, name = "histogram") %>%
#'   e_density(mpg, name = "density", y_index = 1)
#' @seealso \href{https://echarts.apache.org/en/option.html#series-bar}{Additional arguments for histogram},
#'  \href{https://echarts.apache.org/en/option.html#series-line}{Additional arguments for density}
#'
#' @rdname histogram
#' @export
e_histogram <- function(
  e,
  serie,
  breaks = "Sturges",
  name = NULL,
  legend = TRUE,
  bar_width = "99%",
  x_index = 0,
  y_index = 0,
  ...
) {
  UseMethod("e_histogram")
}

#' @export
#' @method e_histogram echarts4r
e_histogram.echarts4r <- function(
  e,
  serie,
  breaks = "Sturges",
  name = NULL,
  legend = TRUE,
  bar_width = "99%",
  x_index = 0,
  y_index = 0,
  ...
) {
  if (missing(e)) {
    stop("must pass e", call. = FALSE)
  }

  if (missing(serie)) {
    stop("must pass serie", call. = FALSE)
  }

  e_histogram_(e, deparse(substitute(serie)), breaks, name, legend, bar_width, x_index, y_index, ...)
}

#' @export
#' @method e_histogram echarts4rProxy
e_histogram.echarts4rProxy <- function(
  e,
  serie,
  breaks = "Sturges",
  name = NULL,
  legend = TRUE,
  bar_width = "99%",
  x_index = 0,
  y_index = 0,
  ...
) {
  if (missing(e)) {
    stop("must pass e", call. = FALSE)
  }

  if (missing(serie)) {
    stop("must pass serie", call. = FALSE)
  }

  e$chart <- e_histogram_(e$chart, deparse(substitute(serie)), breaks, name, legend, bar_width, x_index, y_index, ...)
  return(e)
}

#' @rdname histogram
#' @export
e_density <- function(
  e,
  serie,
  breaks = "Sturges",
  name = NULL,
  legend = TRUE,
  x_index = 0,
  y_index = 0,
  smooth = TRUE,
  ...
) {
  UseMethod("e_density")
}

#' @export
#' @method e_density echarts4r
e_density.echarts4r <- function(
  e,
  serie,
  breaks = "Sturges",
  name = NULL,
  legend = TRUE,
  x_index = 0,
  y_index = 0,
  smooth = TRUE,
  ...
) {
  if (missing(e)) {
    stop("must pass e", call. = FALSE)
  }

  if (missing(serie)) {
    stop("must pass serie", call. = FALSE)
  }

  e_density_(e, deparse(substitute(serie)), breaks, name, legend, x_index, y_index, ...)
}

#' @export
#' @method e_density echarts4rProxy
e_density.echarts4rProxy <- function(
  e,
  serie,
  breaks = "Sturges",
  name = NULL,
  legend = TRUE,
  x_index = 0,
  y_index = 0,
  smooth = TRUE,
  ...
) {
  if (missing(e)) {
    stop("must pass e", call. = FALSE)
  }

  if (missing(serie)) {
    stop("must pass serie", call. = FALSE)
  }

  e$chart <- e_density_(e$chart, deparse(substitute(serie)), breaks, name, legend, x_index, y_index, ...)
  return(e)
}

#' Lines WebGL
#'
#' Draw WebGL lines.
#'
#' @inheritParams e_bar
#' @param data A list.
#' @param ... Any other options (this series type is mostly undocumented).
#'
#' @export
e_lines_gl <- function(e, data, coord_system = "geo", ...) UseMethod("e_lines_gl")

#' @export
#' @method e_lines_gl echarts4r
e_lines_gl.echarts4r <- function(e, data, coord_system = "geo", ...) {
  if (missing(data) || missing(e)) {
    stop("missing e or data", call. = FALSE)
  }

  serie <- list(
    type = "linesGL",
    coordinateSystem = coord_system,
    data = data,
    ...
  )

  e$x$opts$series <- append(e$x$opts$series, list(serie))
  e
}

#' @export
#' @method e_lines_gl echarts4rProxy
e_lines_gl.echarts4rProxy <- function(e, data, coord_system = "geo", ...) {
  if (missing(data) || missing(e)) {
    stop("missing e or data", call. = FALSE)
  }

  serie <- list(
    type = "linesGL",
    coordinateSystem = coord_system,
    data = data,
    ...
  )

  e$chart$x$opts$series <- append(e$chart$x$opts$series, list(serie))
  e
}

#' Confidence bands
#'
#' Add confidence bands
#'
#' @inheritParams e_bar
#' @param min,max series.
#' @param stack Name of stack.
#' @param symbol Whether to show symbols on lower and upper band lines.
#' @param areaStyle The style of lower and upper bands, i.e.: color.
#' @param legend Whether to show \code{min} and \code{max} in legend.
#' @param ... All options must be of vectors or lists of length 2 where the first argument is
#' for the lower bound and the second for the upper bound, see examples.
#'
#' @examples
#' df <- data.frame(
#'   x = 1:10,
#'   y = runif(10, 5, 10)
#' ) %>%
#'   dplyr::mutate(
#'     lwr = y - runif(10, 1, 3),
#'     upr = y + runif(10, 2, 4)
#'   )
#'
#' df %>%
#'   e_charts(x) %>%
#'   e_line(y) %>%
#'   e_band(lwr, upr)
#' @name band
#' @export
e_band <- function(
  e,
  min,
  max,
  stack = "confidence-band",
  symbol = c("none", "none"),
  areaStyle = list(list(color = "rgba(0,0,0,0)"), list()),
  legend = list(FALSE, FALSE),
  ...
) {
  UseMethod("e_band")
}

#' @export
#' @method e_band echarts4r
e_band.echarts4r <- function(
  e,
  min,
  max,
  stack = "confidence-band",
  symbol = c("none", "none"),
  areaStyle = list(list(color = "rgba(0,0,0,0)"), list()),
  legend = list(FALSE, FALSE),
  ...
) {
  if (missing(e)) {
    stop("must pass e", call. = FALSE)
  }

  if (missing(min) || missing(max)) {
    stop("must pass min and max", call. = FALSE)
  }

  e_band_(
    e,
    deparse(substitute(min)),
    deparse(substitute(max)),
    stack = "confidence-band",
    symbol = symbol,
    areaStyle = areaStyle,
    legend = legend,
    ...
  )
}

#' @export
#' @method e_band echarts4rProxy
e_band.echarts4rProxy <- function(
  e,
  min,
  max,
  stack = "confidence-band",
  symbol = c("none", "none"),
  areaStyle = list(list(color = "rgba(0,0,0,0)"), list()),
  legend = list(FALSE, FALSE),
  ...
) {
  if (missing(e)) {
    stop("must pass e", call. = FALSE)
  }

  if (missing(min) || missing(max)) {
    stop("must pass min and max", call. = FALSE)
  }

  e$chart <- e_band_(
    e$chart,
    deparse(substitute(min)),
    deparse(substitute(max)),
    stack = "confidence-band",
    symbol = symbol,
    areaStyle = areaStyle,
    legend = legend,
    ...
  )
}

#' Correlation
#'
#' @inheritParams e_bar
#' @param visual_map Whether to add the visual map.
#' @param order Ordering method, passed to \link[corrplot]{corrMatOrder}.
#'
#' @param ... Any argument to pass to \code{\link{e_heatmap}} and \code{\link{e_visual_map}}.
#'
#' @examples
#' cor(mtcars) %>%
#'   e_charts() %>%
#'   e_correlations(
#'     order = "hclust",
#'     visual_map = FALSE
#'   ) %>%
#'   e_visual_map(
#'     min = -1,
#'     max = 1
#'   )
#' @export
e_correlations <- function(e, order = NULL, visual_map = TRUE, ...) UseMethod("e_correlations")

#' @export
#' @method e_correlations echarts4r
e_correlations.echarts4r <- function(e, order = NULL, visual_map = TRUE, ...) {
  if (missing(e)) {
    stop("missing e", call. = FALSE)
  }

  mat <- e$x$data[[1]]

  if (!is.null(order)) {
    order <- corrplot::corrMatOrder(mat, order = order)
    mat <- mat[order, order]
  }

  row.names(mat) <- colnames(mat)
  mat <- as.data.frame(as.table(mat))
  names(mat) <- c("x", "y", "correlation")

  e <- e %>%
    e_data(mat, x) %>%
    e_heatmap_("y", "correlation", ...)

  if (isTRUE(visual_map)) {
    e <- e %>% e_visual_map_(min = -1, max = 1, ...)
  }

  return(e)
}

#' @export
#' @method e_correlations echarts4rProxy
e_correlations.echarts4rProxy <- function(e, order = NULL, visual_map = TRUE, ...) {
  if (missing(e)) {
    stop("missing e", call. = FALSE)
  }

  mat <- e$chart$x$data[[1]]

  if (!is.null(order)) {
    order <- corrplot::corrMatOrder(mat, order = order)
    mat <- mat[order, order]
  }

  row.names(mat) <- colnames(mat)
  mat <- as.data.frame(as.table(mat))
  names(mat) <- c("x", "y", "correlation")

  e$chart <- e$chart %>%
    e_data(mat, x) %>%
    e_heatmap_("y", "correlation", ...)

  if (isTRUE(visual_map)) {
    e$chart <- e$chart %>% e_visual_map_(min = -1, max = 1, ...)
  }

  return(e)
}

#' Error bar
#'
#' Add error bars.
#'
#' @inheritParams e_bar
#' @param lower,upper Lower and upper error bands.
#' @param renderer mame of render function from renderers.js
#' @param itemStyle  mostly used for borderWidth, default 1.5
#'
#' @examples
#' df <- data.frame(
#'   x = factor(c(1, 2)),
#'   y = c(1, 5),
#'   upper = c(1.1, 5.3),
#'   lower = c(0.8, 4.6)
#' )
#'
#' df %>%
#'   e_charts(x) %>%
#'   e_bar(y) %>%
#'   e_error_bar(lower, upper)
#'
#' # timeline
#' df <- data.frame(
#'   x = factor(c(1, 1, 2, 2)),
#'   y = c(1, 5, 3, 4),
#'   step = factor(c(1, 2, 1, 2)),
#'   upper = c(1.1, 5.3, 3.3, 4.2),
#'   lower = c(0.8, 4.6, 2.4, 3.6)
#' )
#'
#' df %>%
#'   group_by(step) %>%
#'   e_charts(x, timeline = TRUE) %>%
#'   e_bar(y) %>%
#'   e_error_bar(lower, upper)
#' @rdname errorbar
#' @export
e_error_bar <- function(
  e,
  lower,
  upper,
  name = NULL,
  legend = FALSE,
  y_index = 0,
  x_index = 0,
  coord_system = "cartesian2d",
  ...
) {
  UseMethod("e_error_bar")
}

#' @export
#' @method e_error_bar echarts4r
e_error_bar.echarts4r <- function(
  e,
  lower,
  upper,
  name = NULL,
  legend = FALSE,
  y_index = 0,
  x_index = 0,
  coord_system = "cartesian2d",
  ...
) {
  if (missing(e)) {
    stop("must pass e", call. = FALSE)
  }

  if (missing(lower) || missing(upper)) {
    stop("must pass lower, or upper", call. = FALSE)
  }

  e_error_bar_(
    e,
    deparse(substitute(lower)),
    deparse(substitute(upper)),
    name = name,
    legend = legend,
    y_index = y_index,
    x_index = x_index,
    coord_system = coord_system,
    ...
  )
}

#' @export
#' @method e_error_bar echarts4rProxy
e_error_bar.echarts4rProxy <- function(
  e,
  lower,
  upper,
  name = NULL,
  legend = FALSE,
  y_index = 0,
  x_index = 0,
  coord_system = "cartesian2d",
  ...
) {
  if (missing(e)) {
    stop("must pass e", call. = FALSE)
  }

  if (missing(lower) || missing(upper)) {
    stop("must pass lower, or upper", call. = FALSE)
  }

  e$chart <- e_error_bar_(
    e$chart,
    deparse(substitute(lower)),
    deparse(substitute(upper)),
    name = name,
    legend = legend,
    y_index = y_index,
    x_index = x_index,
    coord_system = coord_system,
    ...
  )
  return(e)
}


#' Area bands
#'
#' Add area bands
#'
#' @inheritParams e_bar
#' @param lower,upper series of lower and upper borders of the band
#' @param itemStyle  mostly used for borderWidth, default 0.5
#' @param ... additional options
#'
#' @examples
#' data(EuStockMarkets)
#' as.data.frame(EuStockMarkets) %>%
#'   dplyr::slice_head(n = 200) %>%
#'   dplyr::mutate(day = 1:dplyr::n()) %>%
#'   e_charts(day) %>%
#'   e_line(CAC, symbol = "none") %>%
#'   e_band2(DAX, FTSE, color = "lemonchiffon") %>%
#'   e_band2(DAX, SMI, color = "lightblue", itemStyle = list(borderWidth = 0)) %>%
#'   e_y_axis(scale = TRUE) %>%
#'   e_datazoom(start = 50)
#' @name band2
#' @export
e_band2 <- function(e, lower, upper, ...) {
  UseMethod("e_band2")
}

#' @export
#' @method e_band2 echarts4r
e_band2.echarts4r <- function(e, lower, upper, ...) {
  if (missing(e)) {
    stop("must pass e", call. = FALSE)
  }
  if (missing(lower) || missing(upper)) {
    stop("must pass lower and upper", call. = FALSE)
  }

  e_band2_(
    e,
    deparse(substitute(lower)),
    deparse(substitute(upper)),
    ...
  )
}

#' @export
#' @method e_band2 echarts4rProxy
e_band2.echarts4rProxy <- function(e, lower, upper, ...) {
  if (missing(e)) {
    stop("must pass e", call. = FALSE)
  }
  if (missing(lower) || missing(upper)) {
    stop("must pass lower and upper", call. = FALSE)
  }

  e$chart <- e_band2_(
    e$chart,
    deparse(substitute(lower)),
    deparse(substitute(upper)),
    ...
  )
}
