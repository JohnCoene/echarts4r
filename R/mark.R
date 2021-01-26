#' Mark point
#'
#' Mark points and lines.
#'
#' @inheritParams e_bar
#' @param serie Serie or vector of series to mark on, defaults to all series.
#' @param data Placement of point, line or area.
#' @param title A convenience argument to easily set label, see details.
#' @param title_position Position of title.
#'
#' @details To set a label you need to either use the \code{title}
#' argument or pass a list specifying the label formatter.
#' \code{label = list(formatter = "label")}. The former is more convenient
#' but more limited, e.g.: you cannot specify the placement of the label.
#'
#' @examples
#' max <- list(
#'   name = "Max",
#'   type = "max"
#' )
#'
#' min <- list(
#'   name = "Min",
#'   type = "min"
#' )
#'
#' avg <- list(
#'   type = "average",
#'   name = "AVG"
#' )
#'
#' mtcars %>%
#'   e_charts(mpg) %>%
#'   e_line(wt) %>%
#'   e_line(drat) %>%
#'   e_line(cyl) %>%
#'   e_mark_point("wt", data = max) %>%
#'   e_mark_point(c("cyl", "drat"), data = min) %>%
#'   e_mark_line(data = avg) %>% # applies to all
#'   e_mark_area(
#'     serie = "wt",
#'     data = list(
#'       list(xAxis = "min", yAxis = "min"),
#'       list(xAxis = "max", yAxis = "max")
#'     )
#'   )
#'
#' # serie options
#'
#' iris %>%
#'   group_by(Species) %>%
#'   e_charts(Sepal.Length, timeline = TRUE) %>%
#'   e_line(Sepal.Width) %>%
#'   e_timeline_serie(
#'     title = list(
#'       list(text = "setosa"),
#'       list(text = "versicolor"),
#'       list(text = "virginica")
#'     )
#'   ) %>%
#'   e_mark_area(serie = "setosa") %>%
#'   e_mark_area(
#'     serie = "versicolor",
#'     data = list(
#'       list(xAxis = 2),
#'       list(xAxis = 4)
#'     ),
#'     itemStyle = list(color = "lightblue")
#'   ) %>%
#'   e_mark_area(serie = "virginica")
#' @seealso \href{https://echarts.apache.org/en/option.html#series-line.markPoint}{Additional point arguments},
#' \href{https://echarts.apache.org/en/option.html#series-line.markLine}{Additional line arguments}
#'
#' @rdname mark
#' @export
e_mark_point <- function(e, serie = NULL, data = NULL, ..., title = NULL, title_position = NULL) {
  if (missing(e)) {
    stop("must pass e", call. = FALSE)
  }

  if (is.null(serie)) {
    index <- seq_along(if (e$x$tl) e$x$opts$options else e$x$opts$series)
  } else {
    index <- .get_index(e, serie)
  }

  for (i in index) {
    point <- list(...)

    if (!is.null(data)) {
      point$data <- list(data)
    }

    if (!is.null(title) && !is.null(data)) {
      point$data[[1]]$label <- list(formatter = title, position = title_position)
    }

    if (e$x$tl) {
      if (is.null(e$x$opts$options[[i]]$series[[1]]$markPoint)) {
        e$x$opts$options[[i]]$series[[1]]$markPoint <- append(e$x$opts$options[[i]]$series[[1]]$markPoint, point)
      } else {
        e$x$opts$options[[i]]$series[[1]]$markPoint$data <- append(e$x$opts$options[[i]]$series[[1]]$markPoint$data, point$data)
      }
    }
    else {
      if (is.null(e$x$opts$series[[i]]$markPoint)) {
        e$x$opts$series[[i]]$markPoint <- append(e$x$opts$series[[i]]$markPoint, point)
      } else {
        e$x$opts$series[[i]]$markPoint$data <- append(e$x$opts$series[[i]]$markPoint$data, point$data)
      }
    }
  }

  e
}

#' @rdname mark
#' @export
e_mark_line <- function(e, serie = NULL, data = NULL, ..., title = NULL, title_position = NULL) {
  if (missing(e)) {
    stop("must pass e", call. = FALSE)
  }

  if (is.null(serie)) {
    index <- seq_along(if (e$x$tl) e$x$opts$options else e$x$opts$series)
  } else {
    index <- .get_index(e, serie)
  }

  for (i in index) {
    point <- list(...)

    if (!is.null(data)) {
      point$data <- list(data)
    }

    if (!is.null(title) && !is.null(data)) {
      point$data[[1]]$label <- list(formatter = title, position = title_position)
    }

    if (e$x$tl) {
      if (is.null(e$x$opts$options[[i]]$series[[1]]$markLine)) {
        e$x$opts$options[[i]]$series[[1]]$markLine <- append(e$x$opts$options[[i]]$series[[1]]$markLine, point)
      } else {
        e$x$opts$options[[i]]$series[[1]]$markLine$data <- append(e$x$opts$options[[i]]$series[[1]]$markLine$data, point$data)
      }
    }
    else {
      if (is.null(e$x$opts$series[[i]]$markLine)) {
        e$x$opts$series[[i]]$markLine <- append(e$x$opts$series[[i]]$markLine, point)
      } else {
        e$x$opts$series[[i]]$markLine$data <- append(e$x$opts$series[[i]]$markLine$data, point$data)
      }
    }
  }

  e
}

#' @rdname mark
#' @export
e_mark_area <- function(e, serie = NULL, data = NULL, ..., title = NULL, title_position = NULL) {
  if (missing(e)) {
    stop("must pass e", call. = FALSE)
  }

  if (is.null(serie)) {
    index <- seq_along(if (e$x$tl) e$x$opts$options else e$x$opts$series)
  } else {
    index <- .get_index(e, serie)
  }

  for (i in index) {
    point <- list(...)

    if (!is.null(data)) {
      point$data <- list(data)
    }

    if (!is.null(title) && !is.null(data)) {
      point$data[[1]]$label <- list(formatter = title, position = title_position)
    }

    if (e$x$tl) {
      if (is.null(e$x$opts$options[[i]]$series[[1]]$markArea)) {
        e$x$opts$options[[i]]$series[[1]]$markArea <- append(e$x$opts$options[[i]]$series[[1]]$markArea, point)
      } else {
        e$x$opts$options[[i]]$series[[1]]$markArea$data <- append(e$x$opts$options[[i]]$series[[1]]$markArea$data, point$data)
      }
    }
    else {
      if (is.null(e$x$opts$series[[i]]$markArea)) {
        e$x$opts$series[[i]]$markArea <- append(e$x$opts$series[[i]]$markArea, point)
      } else {
        e$x$opts$series[[i]]$markArea$data <- append(e$x$opts$series[[i]]$markArea$data, point$data)
      }
    }
  }

  e
}

#' Mark
#'
#' Mark points, lines, and areas with a proxy ([echarts4rProxy()]).
#'
#' @inheritParams e_bar
#' @param type Type of mark: 'point','line' or 'area', defaults to 'point'.
#' @param serie_index Single index of serie to mark on, defaults to 1.
#' Proxy doesn't know series' names, so it only uses index.
#' @param data Location of point, line or area, defaults to NULL.
#'
#' @details Allows the three type of marks to work with [echarts4rProxy()]
#'
#' @examples
#' library(shiny)
#' library(dplyr)
#'
#' ui <- fluidPage(
#'   fluidRow(
#'     column(3, actionButton("pxy", "Marks")),
#'     column(
#'       3,
#'       checkboxInput("tln", "Timeline", value = FALSE)
#'     )
#'   ),
#'   echarts4rOutput("plot")
#' )
#'
#' server <- function(input, output) {
#'   data(EuStockMarkets)
#'
#'   bb <- as.data.frame(EuStockMarkets) %>%
#'     slice_head(n = 150) %>%
#'     mutate(day = 1:n())
#'
#'   output$plot <- renderEcharts4r({
#'     react()
#'   })
#'
#'   observeEvent(input$pxy, {
#'     echarts4rProxy("plot", data = NULL) %>%
#'       e_mark_p(
#'         type = "line",
#'         serie_index = 1,
#'         data = list(type = "average"),
#'         lineStyle = list(type = "dashed", color = "cyan")
#'       ) %>%
#'       e_mark_p(
#'         serie_index = 2,
#'         data = list(
#'           xAxis = bb$day[60],
#'           yAxis = bb$SMI[60],
#'           value = "pnt"
#'         )
#'       ) %>%
#'       e_mark_p(
#'         type = "line",
#'         serie_index = 2,
#'         data = list(
#'           list(xAxis = bb$day[10], yAxis = bb$SMI[10]),
#'           list(xAxis = bb$day[37], yAxis = bb$SMI[37])
#'         ),
#'         lineStyle = list(type = "solid", color = "yellow")
#'       ) %>%
#'       e_mark_p(
#'         type = "area",
#'         serie_index = 1,
#'         data = list(
#'           list(xAxis = bb$day[95]),
#'           list(xAxis = bb$day[105])
#'         ),
#'         itemStyle = list(color = "lightblue"),
#'         label = list(formatter = "X-area", position = "middle")
#'       ) %>%
#'       e_merge()
#'   })
#'
#'   react <- eventReactive(input$tln, {
#'     tmp <- bb
#'     if (input$tln) tmp <- tmp %>% group_by(day < 75)
#'
#'     tmp %>%
#'       e_charts(
#'         day,
#'         backgroundColor = "#181818",
#'         legend = list(textStyle = list(color = "#aaa")),
#'         timeline = input$tln
#'       ) %>%
#'       e_y_axis(scale = TRUE, axisLabel = list(color = "#aaa")) %>%
#'       e_line(CAC, symbol = "none", color = "#ff33b8") %>%
#'       e_line(SMI, symbol = "none", color = "green")
#'   })
#' }
#' if (interactive()) {
#'   shinyApp(ui, server)
#' }
#' @name e_mark_p
#' @export
e_mark_p <- function(e, type, serie_index, data, ...) UseMethod("e_mark_p")

#' @method e_mark_p echarts4r
#' @export
e_mark_p.echarts4r <- function(e, type = "point", serie_index = NULL, data = NULL, ...) {
  if (missing(e)) stop("must pass e", call. = FALSE)
  e_mark_p_(e, type, serie_index, data, ...)
}

#' @method e_mark_p echarts4rProxy
#' @export
e_mark_p.echarts4rProxy <- function(e, type = "point", serie_index = NULL, data = NULL, ...) {
  if (missing(e)) stop("must pass e", call. = FALSE)
  e$chart <- e_mark_p_(e$chart, type, serie_index, data, ...)
  return(e)
}

#' @rdname e_mark_p
#' @export
e_mark_p_ <- function(e, type, serie_index, data = NULL, ...) {
  if (missing(e)) stop("must pass e", call. = FALSE)
  if (missing(type)) stop("must pass type", call. = FALSE)
  mtype <- type
  if (!startsWith(mtype, "mark")) {
    mtype <- switch(type, "point" = "markPoint", "line" = "markLine", "area" = "markArea")
  }
  if (!startsWith(mtype, "mark")) stop("type must be line,point or area", call. = FALSE)

  index <- ifelse(is.null(serie_index), 1, as.numeric(serie_index))

  for (i in 1:index) {
    if (length(e$x$opts$series) < i) {
      e$x$opts$series[[i]] <- list()
    } # init
    if (i < index) next

    point <- list(...)

    if (!is.null(data)) point$data <- list(data)

    suppressWarnings( # eval generates redundant warnings
      if (e$x$tl) {
        if (mtype %in% names(e$x$opts$options[[i]]$series[[1]])) {
          eval(parse(text = paste0("e$x$opts$options[[i]]$series[[1]]$", mtype, "$data <- append(e$x$opts$options[[i]]$series[[1]]$", mtype, "$data, point$data)")))
        } else {
          eval(parse(text = paste0("e$x$opts$options[[i]]$series[[1]]$", mtype, " <- point")))
        }
      }
      else {
        if (mtype %in% names(e$x$opts$series[[i]])) {
          eval(parse(text = paste0("e$x$opts$series[[i]]$", mtype, "$data <- append(e$x$opts$series[[i]]$", mtype, "$data, point$data)")))
        } else {
          eval(parse(text = paste0("e$x$opts$series[[i]]$", mtype, " <- point")))
        }
      }
    )
  }
  e
}
