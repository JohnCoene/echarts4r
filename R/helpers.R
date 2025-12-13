#' Country names
#'
#' Convert country names to echarts format.
#'
#' @param data Data.frame in which to find column names.
#' @param input,output Input and output columns.
#' @param type Passed to \link[countrycode]{countrycode} \code{origin} parameter.
#' @param ... Any other parameter to pass to \link[countrycode]{countrycode}.
#'
#' @details Taiwan and Hong Kong cannot be plotted.
#'
#' @examples
#' cns <- data.frame(country = c("US", "BE"))
#'
#' # replace
#' e_country_names(cns, country)
#'
#' # specify output
#' e_country_names(cns, country, country_name)
#' @rdname e_country_names
#' @export
e_country_names <- function(data, input, output, type = "iso2c", ...) {
  if (missing(data) || missing(input)) {
    stop("must pass data and input", call. = FALSE)
  }

  if (missing(output)) {
    output <- NULL
  } else {
    output <- deparse(substitute(output))
  }

  e_country_names_(data, deparse(substitute(input)), output, type, ...)
}

#' @rdname e_country_names
#' @export
e_country_names_ <- function(data, input, output = NULL, type = "iso2c", ...) {
  if (missing(data) || missing(input)) {
    stop("must pass data and input", call. = FALSE)
  }

  src <- input
  cn <- countrycode::countrycode(data[[src]], origin = type, destination = "country.name", ...)

  if (is.null(output)) {
    output <- src
  }

  data[[output]] <- .correct_countries(cn)
  data
}

#' Color range
#'
#' Build manual color range
#'
#' @param data Data.frame in which to find column names.
#' @param input,output Input and output columns.
#' @param colors Colors to pass to \code{\link{colorRampPalette}}.
#' @param ... Any other argument to pass to \code{\link{colorRampPalette}}.
#'
#' @examples
#' df <- data.frame(val = 1:10)
#'
#' e_color_range(df, val, colors)
#' @rdname e_color_range
#' @export
e_color_range <- function(data, input, output, colors = c("#bf444c", "#d88273", "#f6efa6"), ...) {
  if (missing(data) || missing(input) || missing(output)) {
    stop("must pass data, input and output", call. = FALSE)
  }

  e_color_range_(data, deparse(substitute(input)), deparse(substitute(output)), colors, ...)
}

#' @rdname e_color_range
#' @export
e_color_range_ <- function(data, input, output, colors = c("#bf444c", "#d88273", "#f6efa6"), ...) {
  if (missing(data) || missing(input) || missing(output)) {
    stop("must pass data, input and output", call. = FALSE)
  }

  serie <- data[[input]]

  if (inherits(serie, "factor") || inherits(serie, "character")) {
    col <- scales::col_numeric(colors, domain = range(serie))(serie)
  } else {
    col <- scales::col_numeric(colors, domain = range(serie))(serie)
  }

  data[[output]] <- col
  data
}

#' Get data
#'
#' Get data passed to \code{\link{e_charts}}.
#'
#' @inheritParams e_bar
#'
#' @return A list of data.frames, one for each group.
#'
#' @examples
#' echart <- cars |>
#'   e_charts(speed) |>
#'   e_scatter(dist) |>
#'   e_lm(dist ~ speed)
#'
#' echart
#'
#' e_get_data(echart)[[1]]
#' @export
e_get_data <- function(e) {
  e$x$data
}

#' Formatters
#'
#' Simple formatters as helpers.
#'
#' @inheritParams e_bar
#' @param axis Axis to apply formatter to.
#' @param suffix,prefix Suffix and prefix of label.
#' @param ... Any other arguments to pass to \code{\link{e_axis}}.
#'
#' @examples
#' # Y = %
#' df <- data.frame(
#'   x = 1:10,
#'   y = round(
#'     runif(10, 1, 100),
#'     2
#'   )
#' )
#'
#' df |>
#'   e_charts(x) |>
#'   e_line(y) |>
#'   e_format_y_axis(suffix = "%") |>
#'   e_format_x_axis(prefix = "A")
#' @rdname formatters
#' @export
e_format_axis <- function(e, axis = "y", suffix = NULL, prefix = NULL, ...) {
  if (missing(e)) {
    stop("must pass e", call. = FALSE)
  }

  if (is.null(suffix) && is.null(prefix)) {
    stop("missing formatting")
  }

  fmt <- paste(prefix, "{value}", suffix)

  e <- e |>
    e_axis(
      axis = axis,
      axisLabel = list(formatter = fmt),
      ...
    )

  e
}

#' @rdname formatters
#' @export
e_format_x_axis <- function(e, suffix = NULL, prefix = NULL, ...) {
  e_format_axis(e, "x", suffix, prefix, ...)
}

#' @rdname formatters
#' @export
e_format_y_axis <- function(e, suffix = NULL, prefix = NULL, ...) {
  e_format_axis(e, "y", suffix, prefix, ...)
}

#' Format labels
#'
#' @inheritParams e_bar
#' @param show Set to \code{TRUE} to show the labels.
#' @param position Position of labels, see
#' \href{https://echarts.apache.org/en/option.html#series-line.label.position}{official documentation}
#'  for the full list of options.
#' @param ... Any other options see
#' \href{https://echarts.apache.org/en/option.html#series-line.label}{documentation} for other options.
#'
#' @examples
#' mtcars |>
#'   e_chart(wt) |>
#'   e_scatter(qsec, cyl) |>
#'   e_labels(fontSize = 9)
#'
#' mtcars |>
#'   group_by(cyl) |>
#'   e_chart(wt) |>
#'   e_scatter(qsec, mpg) |>
#'   e_labels(fontSize = 9)
#'
#' # timeline
#' mtcars |>
#'   group_by(cyl) |>
#'   e_chart(wt) |>
#'   e_scatter(qsec, mpg) |>
#'   e_labels(fontSize = 9)
#' @export
e_labels <- function(e, show = TRUE, position = "top", ...) {
  if (missing(e)) {
    stop("must pass e", call. = FALSE)
  }

  opts <- list(
    show = show,
    position = position,
    ...
  )

  if (!e$x$tl) {
    for (i in seq_along(e$x$opts$series)) {
      e$x$opts$series[[i]]$label <- opts
    }
  } else {
    for (i in seq_along(e$x$opts$baseOption$series)) {
      e$x$opts$baseOption$series[[i]]$label <- opts
    }
  }

  return(e)
}

#' List
#'
#' simply pass a list of options, similar to a \code{JSON}.
#'
#' @inheritParams e_bar
#' @param list A \code{list} of options passed to \code{setOptions}.
#' @param append if \code{TRUE} the \code{list} is appended to the options,
#' otherwise it \emph{overwrites} everything.
#'
#' @examples
#' N <- 20 # data points
#'
#' opts <- list(
#'   xAxis = list(
#'     type = "category",
#'     data = LETTERS[1:N]
#'   ),
#'   yAxis = list(
#'     type = "value"
#'   ),
#'   series = list(
#'     list(
#'       type = "line",
#'       data = round(runif(N, 5, 20))
#'     )
#'   )
#' )
#'
#' e_charts() |>
#'   e_list(opts)
#' @export
e_list <- function(e, list, append = FALSE) {
  if (missing(list)) {
    stop("missing list", call. = FALSE)
  }

  if (isTRUE(append)) {
    e$x$opts <- append(e$x$opts, list)
  } else {
    e$x$opts <- list
  }

  e
}

#' Aria
#'
#' W3C defined the Accessible Rich Internet Applications Suite (WAI-ARIA)
#' to make Web content and Web applications more accessible to the disabled.
#' From ECharts 4.0, echarts4r supports ARIA by generating description for
#' charts automatically.
#'
#' @details There should be an aria-label attribute on the chart DOM, which
#' can help the disabled understand the content of charts with the help of certain devices.
#'
#' @inheritParams e_bar
#' @param enabled Whether to enable aria helper text.
#'
#' @seealso \href{https://echarts.apache.org/en/option.html#aria}{official documentation}
#'
#' @export
e_aria <- function(e, enabled = TRUE, ...) {
  e$x$opts$aria <- list(
    enabled = enabled,
    ...
  )

  return(e)
}

#' To & From JSON
#'
#' Get JSON options from an echarts4r object and build one from JSON.
#'
#' @inheritParams e_bar
#' @param json Whether to return the JSON, otherwise returns a \code{list}.
#' @param txt JSON character string, url, or file.
#' @param jswrapper Whether to wrap pure JS functions in \code{htmlwidgets::JS}, Default is FALSE.
#' @param ... Additional options to pass to \link[jsonlite]{toJSON}.
#'
#' @details \code{txt} should contain the full list of options required to build a chart.
#' This is subsequently passed to the \code{setOption} ECharts (JavaScript) function.
#'
#' @examples
#' p <- cars |>
#'   e_charts(dist) |>
#'   e_scatter(speed, symbol_size = 10)
#'
#' p # plot
#'
#' # extract the JSON
#' json <- p |>
#'   e_inspect(
#'     json = TRUE,
#'     pretty = TRUE
#'   )
#'
#' # print json
#' json
#'
#' # rebuild plot
#' echarts_from_json(json) |>
#'   e_theme("dark") # modify
#' @return \code{e_inspect} Returns a \code{list} if \code{json} is \code{FALSE} and a
#' JSON string otherwise. \code{echarts_from_json} returns an object of class \code{echarts4r}.
#'
#' @note Must be passed as last option.
#'
#' @rdname echartsNJSON
#' @export
e_inspect <- function(e, json = FALSE, ...) {
  opts <- e$x$opts

  if (isTRUE(json)) {
    opts <- jsonlite::toJSON(opts, force = TRUE, auto_unbox = TRUE, null = "null", ...)
  }

  return(opts)
}

#' @rdname echartsNJSON
#' @export
echarts_from_json <- function(txt, jswrapper = FALSE) {
  json <- jsonlite::fromJSON(txt, simplifyVector = FALSE)

  e_charts() -> e
  e$x$opts <- json
  if (isTRUE(jswrapper)) {
      for (i in seq_along(e$x$opts$tooltip)) {

        if (is.character(e$x$opts$tooltip[[i]]) && startsWith(e$x$opts$tooltip[[i]], "function")) {
          # Wrap pure JS functions in htmlwidgets::JS
          e$x$opts$tooltip[[i]] <- htmlwidgets::JS(e$x$opts$tooltip[[i]])
        }

      }
  }

  e
}

#' Axis ZigZags
#'
#' helper function for generating axis break zigzags in chart
#'
#' @inheritParams e_bar
#' @param axis Axis to apply formatter to. Supports x and y axis
#' @param start,end Start and End point for boundary of zigzag. Also supports vectors for generating multiple breaks. Can also support time values.
#' @param gap Determines the visual size of the axis break area.
#' Supports Percentage(String) as proportional value relative to axis. Supports Absolute value(numeric) which refers to literal values in the axis similar to start,end (Not a pixel value).
#' @param zigzagAmplitude Amplitude of zigzag. Unit is pixels.
#' @param ... Any other arguments to pass to breakArea argument.
#'
#' @examples
#'
#' df <- data.frame(
#'               x = c("a", "b", "c", "d", "c"),
#'               y = c(100, 200, 200, 700, 300)
#'              )
#'
#' df |>
#'   e_charts(x) |>
#'   e_bar(y) |>
#'   e_zigzag(axis = 'y', start = 400, end = 500)
#'
#' df |>
#'   e_charts(x) |>
#'   e_bar(y) |>
#'   e_zigzag(axis = 'y', start = c(125,400), end = c(150,500))
#' @seealso \href{https://echarts.apache.org/en/option.html#series-bar}{Additional arguments}
#'
#' @rdname e_zigzag
#' @export
e_zigzag <- function(e, axis = 'y', start, end, gap = "3%", zigzagAmplitude = 10, ...){

  if (missing(e)) {
    stop("must pass echart into function", call. = FALSE)
  }

  if (missing(axis)) {
    stop("must indicate which axis to zigzag. e.g. 'x' or 'y'", call. = FALSE)
  }

  if (missing(start) | missing(end)) {
    stop("must provide start and end values")
  }

  df <- data.frame("starts" = start, "ends" = end, "gap" = gap)
  b <- .build_zigzags(df, starts = "starts", ends = "ends", gap = "gap")

  bA <- list(
    zigzagAmplitude = zigzagAmplitude,
    ...
  )

  if(axis == 'y'){
    e <- e |> e_y_axis(breaks = b, breakArea = bA)
  }

  if(axis == 'x'){
    e <- e |> e_x_axis(breaks = b, breakArea = bA)
  }
  e
}


#' Axis Jitter
#'
#' helper function for generating jitter between points in a scatter plot. This is only applicable to e_scatter().
#'
#' @inheritParams e_bar
#' @param axis Axis to apply formatter to. Supports x and y axis
#' @param jitter Pixel units indicating the amount of random noise to add to each data point position.
#' @param jitterOverlap Boolean allowing overlap between data points. If false, overlap will not be allowed. For some cases, scatters may still overlap if there is no reasonable way to avoid.
#' @param jitterMargin When you have jitter and jiterOverlap is FALSE, this is the minimum distance in pixels between two data points.
#'
#' @examples
#'
#' df <- data.frame(
#' value = c(rnorm(50, mean = 5, sd = 1),
#'          rnorm(50, mean = 10, sd = 1),
#'          rnorm(50, mean = 15, sd = 1)),
#'          group = rep(c("Group A", "Group B", "Group C"), each = 50)
#'           )
#'
#' df |> e_charts(group) |> e_scatter(value) |> e_jitter()
#' @seealso \href{https://echarts.apache.org/en/option.html#yAxis.jitter}{Additional arguments}
#'
#' @rdname e_jitter
#' @export
e_jitter <- function(e, axis = 'x', jitter = 20, jitterOverlap = FALSE, jitterMargin = 5){
  if(missing(e)) {
    stop("must pass echart into function", call. = FALSE)
  }

  if(is.null(axis)) {
    stop("must indicate which axis to apply jitter. e.g. 'x' or 'y'", call. = FALSE)
  }

  if(e$x$opts$series[[1]]$type != 'scatter'){
    stop("jitter is only supported with scatter plots")
  }

  if(axis == 'x'){
    e <- e |> e_x_axis(jitter = jitter, jitterOverlap = jitterOverlap, jitterMargin = jitterMargin)
  }

  if(axis == 'y'){
    e <- e |> e_y_axis(jitter = jitter, jitterOverlap = jitterOverlap, jitterMargin = jitterMargin)
  }
  e
}

#' Generate Matrix
#'
#' helper function for generating the x and y axes for a matrix grid.
#'
#' @inheritParams e_bar
#' @param xAxis,yAxis provide column name of dataframe to generate X-axis and Y-axis header cells
#' @examples
#'
#' df <- data.frame("Class" = rep(c("Class1", "Class2", "Class3"),each = 3),
#' "Grade" = c("Grade1","Grade2", "Grade3"),
#' "Males" = sample(1:10, 9),
#' "Females" = sample(1:10,9))
#'
#' df |> e_charts() |> e_matrix(xAxis = "Class", yAxis = "Grade")
#'
#' @seealso \href{https://echarts.apache.org/en/option.html#matrix}{Additional arguments}
#'
#' @rdname e_matrix
#' @export
e_matrix <- function(e, xAxis, yAxis){
  if (missing(e)) {
    stop("must pass e", call. = FALSE)
  }


  if (missing(xAxis) | missing(yAxis)) {
    stop("must provide both x and y values", call. = FALSE)
  }

  e$x$opts$matrix <- append(e$x$opts$matrix, list(x = list(data = as.list(unique(e$x$data[[1]][[xAxis]])), name = xAxis)))

  e$x$opts$matrix <- append(e$x$opts$matrix, list(y = list(data = as.list(unique(e$x$data[[1]][[yAxis]])), name = yAxis)))


  e
}


#' Generate Matrix Axis Parents
#'
#' helper function for generating parent values for x or y axis headers
#'
#' @inheritParams e_bar
#' @param axis which axis the parent should be added
#' @param value text for the new parent header cell
#' @param children vector containing values for which current header cells will be children for the new parent cell
#' @examples
#'
#' df <- data.frame("Class" = rep(c("Class1", "Class2", "Class3"),each = 3),
#' "Grade" = c("Grade1","Grade2", "Grade3"),
#' "Males" = sample(1:10, 9),
#' "Females" = sample(1:10,9))
#'
#' df |> e_charts() |> e_matrix(xAxis = "Class", yAxis = "Grade") |>
#' e_matrix_parent(value = "Primary", children = c("Class1", "Class2")) |>
#' e_matrix_parent(value = "High", children = "Class3")
#'
#' @seealso \href{https://echarts.apache.org/en/option.html#matrix.x.data}{Additional arguments}
#'
#' @rdname e_matrix_parent
#' @export
e_matrix_parent <- function(e, axis = "x", value, children){
  if (missing(e)) {
    stop("must pass e", call. = FALSE)
  }

  if(axis=="x"){

    child_ndx <- which(e$x$opts$matrix$x$data %in% children)

    if(length(child_ndx)==0){
      for(i in 1:length(e$x$opts$matrix$x$data )){
        if(e$x$opts$matrix$x$data[[i]]$value %in% children){
          child_ndx <- append(child_ndx, i)
        }
      }
      if(length(child_ndx)==0){
        stop("No children found in the data")
      }
    }

    new_node <- list(value = value, children = e$x$opts$matrix$x$data[child_ndx])

    e$x$opts$matrix$x$data <- append(e$x$opts$matrix$x$data, list(new_node))
    e$x$opts$matrix$x$data <- e$x$opts$matrix$x$data[-child_ndx]
  }

  if(axis=="y"){

    child_ndx <- which(e$x$opts$matrix$y$data %in% children)

    if(length(child_ndx)==0){
      for(i in 1:length(e$x$opts$matrix$y$data )){
        if(e$x$opts$matrix$y$data[[i]]$value %in% children){
          child_ndx <- append(child_ndx, i)
        }
      }
      if(length(child_ndx)==0){
        stop("No children found in the data")
      }
    }

    new_node <- list(value = value, children = e$x$opts$matrix$y$data[child_ndx])

    e$x$opts$matrix$y$data <- append(e$x$opts$matrix$y$data, list(new_node))
    e$x$opts$matrix$y$data <- e$x$opts$matrix$y$data[-child_ndx]
  }

  e

}

#' Fill Matrix Axis Corner
#'
#' helper function for adding data to the corner of matrix
#'
#' @inheritParams e_bar
#' @param coord corner cell coordinate location
#' @param value text to display in corner cell
#' @param mergeCells whether the body cells and corner cells can be merged
#' @param coordClamp determines whether null values can be used to indicate an
#'   entire row/column
#' @examples
#'
#' df <- data.frame("Class" = rep(c("Class1", "Class2", "Class3"),each = 3),
#' "Grade" = c("Grade1","Grade2", "Grade3"),
#' "Males" = sample(1:10, 9),
#' "Females" = sample(1:10,9))
#'
#' df |> e_charts() |> e_matrix(xAxis = "Class", yAxis = "Grade") |>
#' e_matrix_parent(value = "Primary", children = c("Class1", "Class2")) |>
#' e_matrix_parent(value = "High", children = "Class3") |>
#' e_matrix_corner(value = "All School", label = list(
#'    fontSize = 24, color = "#555", position = "inside"))
#'
#' @seealso \href{https://echarts.apache.org/en/option.html#matrix.corner}{Additional arguments}
#'
#' @rdname e_matrix_corner
#' @export
e_matrix_corner <- function(e, coord = c(-1,-1), value, mergeCells = TRUE, coordClamp = FALSE, ...){
  if (missing(e)) {
    stop("must pass e", call. = FALSE)
  }

  data <- list(coord = coord, value = value, mergeCells = mergeCells, coordClamp = coordClamp)
  l <- list(data = list(data), ...)

  e$x$opts$matrix$corner <- append(e$x$opts$matrix$corner, l)

  e
}
