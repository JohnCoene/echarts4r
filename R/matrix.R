#' @keywords internal
#' @noRd
get_base_nodes <- function(x) {
  found_values <- list()

  if (is.list(x) && !is.data.frame(x)) {
    current_names <- names(x)

    for (i in seq_along(x)) {
      element <- x[[i]]

      is_unnamed <- is.null(current_names) || current_names[i] == ""
      is_atomic_data <- !is.list(element) || is.data.frame(element)

      if (is_unnamed && is_atomic_data) {
        found_values <- c(found_values, list(element))
      }

      found_values <- c(found_values, get_base_nodes(element))
    }
  }
  return(found_values)
}


#' Generate Matrix
#'
#' helper function for generating the x and y axes for a matrix grid.
#'
#' @inheritParams e_bar
#' @param xAxis,yAxis provide column name of dataframe to generate X-axis and
#'   Y-axis header cells
#' @examples
#'
#' df <- data.frame("Class" = rep(c("Class1", "Class2", "Class3"),each = 3),
#' "Grade" = c("Grade1","Grade2", "Grade3"),
#' "A" = sample(1:10, 9),
#' "B" = sample(1:10,9))
#'
#' df |> e_charts() |> e_matrix(xAxis = "Class", yAxis = "Grade")
#'
#' @seealso \href{https://echarts.apache.org/en/option.html#matrix}{Additional
#'   arguments}
#'
#' @rdname e_matrix
#' @export
e_matrix <- function(e, xAxis, yAxis, ...){
  if (missing(e)) {
    stop("must pass e", call. = FALSE)
  }

  if(e$x$tl == TRUE){
    stop("timeline is not supported in matrix")
  }


  if (missing(xAxis) | missing(yAxis)) {
    stop("must provide both x and y values", call. = FALSE)
  }
  
  e$x$opts$yAxis <- NULL

  e$x$opts$matrix <- append(e$x$opts$matrix, list(x = list(data = as.list(unique(e$x$data[[1]][[xAxis]])), name = xAxis)))

  e$x$opts$matrix <- append(e$x$opts$matrix, list(y = list(data = as.list(unique(e$x$data[[1]][[yAxis]])), name = yAxis)))

  e$x$opts$matrix <- append(e$x$opts$matrix, list(...))

  e
}

#' Generate Matrix
#'
#' helper function for generating default
#'
#' @inheritParams e_bar
#' @param rows,cols provide integer values for the number of rows and columns in the matrix grid
#' @examples
#'
#' e_matrix_raw(rows = 3, cols = 3, backgroundStyle=list(borderWidth=0))
#'
#' e_matrix_raw(rows = 3, cols = 3, body = list(itemStyle = list(borderWidth = 0)))
#'
#' @seealso \href{https://echarts.apache.org/en/option.html#matrix}{Additional arguments}
#'
#' @rdname e_matrix_raw
#' @export
e_matrix_raw <- function(rows = NULL, cols = NULL, ...){
  if(is.null(rows) || is.null(cols)) {
    stop("Must provide both the number of rows and columns.")
  }

  e <- e_chart()
  e$x$opts$yAxis <- NULL
  e$x$opts$matrix <- list(x = list(data = rep(list(NA), cols), show = FALSE),
                          y = list(data = rep(list(NA),rows), show = FALSE),
                          ...)
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
#' "A" = sample(1:10, 9),
#' "B" = sample(1:10,9))
#'
#' df |> e_charts() |> e_matrix(xAxis = "Class", yAxis = "Grade") |>
#' e_matrix_parent(value = "Primary", children = c("Class1", "Class2")) |>
#' e_matrix_parent(value = "High", children = "Class3")
#'
#' @seealso \href{https://echarts.apache.org/en/option.html#matrix.x.data}{Additional arguments}
#'
#' @rdname e_matrix_parent
#' @export
e_matrix_parent <- function(e, axis = "x", value, children, ...){
  if (missing(e)) {
    stop("must pass e", call. = FALSE)
  }

  if(axis=="x"){

    child_ndx <- which(e$x$opts$matrix$x$data %in% children)

    if(length(child_ndx)==0){
      for(i in seq_along(e$x$opts$matrix$x$data )){
        data_subset <- e$x$opts$matrix$x$data[[i]]
        if("value" %in% names(data_subset) && data_subset$value %in% children){
          child_ndx <- append(child_ndx, i)
        }
      }
      if(length(child_ndx)==0){
        stop("No children found in the data")
      }
    }

    new_node <- list(value = value, children = e$x$opts$matrix$x$data[child_ndx], ...)

    e$x$opts$matrix$x$data <- append(e$x$opts$matrix$x$data, list(new_node))
    e$x$opts$matrix$x$data <- e$x$opts$matrix$x$data[-child_ndx]
  }

  if(axis=="y"){

    child_ndx <- which(e$x$opts$matrix$y$data %in% children)

    if(length(child_ndx)==0){
      for(i in seq_along(e$x$opts$matrix$y$data )){
        data_subset <- e$x$opts$matrix$y$data[[i]]
        if("value" %in% names(data_subset) && data_subset$value %in% children){
          child_ndx <- append(child_ndx, i)
        }
      }
      if(length(child_ndx)==0){
        stop("No children found in the data")
      }
    }

    new_node <- list(value = value, children = e$x$opts$matrix$y$data[child_ndx], ...)

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
#' @param coordClamp determines whether null values can be used to indicate an entire row/column
#' @examples
#'
#' df <- data.frame("Class" = rep(c("Class1", "Class2", "Class3"),each = 3),
#' "Grade" = c("Grade1","Grade2", "Grade3"),
#' "A" = sample(1:10, 9),
#' "B" = sample(1:10,9))
#'
#' df |> e_charts() |> e_matrix(xAxis = "Class", yAxis = "Grade") |>
#' e_matrix_parent(value = "Primary", children = c("Class1", "Class2")) |>
#' e_matrix_parent(value = "High", children = "Class3") |>
#' e_matrix_corner(value = "All School",
#' label = list(fontSize = 24, color = "#555", position = "inside"))
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

#' Format Matrix Axis
#'
#' helper function for formatting the x and y axes for a matrix grid.
#'
#' @inheritParams e_bar
#' @param axis indicate which axis shoud be adjusted
#' @examples
#'
#' df <- data.frame("Class" = rep(c("Class1", "Class2", "Class3"),each = 3),
#' "Grade" = c("Grade1","Grade2", "Grade3"),
#' "A" = sample(1:10, 9),
#' "B" = sample(1:10,9))
#'
#' df |> e_charts() |> e_matrix(xAxis = "Class", yAxis = "Grade") |>
#' e_format_matrix_axis(axis = "x", label = list(color = "red"))
#'
#' @seealso \href{https://echarts.apache.org/en/option.html#matrix}{Additional arguments}
#'
#' @rdname e_format_matrix_axis
#' @export
e_format_matrix_axis <- function(e, axis = "x", ...){
  if (missing(e)) {
    stop("must pass e", call. = FALSE)
  }

  if (missing(axis)) {
    stop("must specify which axis", call. = FALSE)
  }

  if(axis == "x"){
    e$x$opts$matrix$x <- append(e$x$opts$matrix$x, list(...))
  }

  if(axis == "y"){
    e$x$opts$matrix$y <- append(e$x$opts$matrix$y, list(...))
  }

  e
}

#' Add new chart to matrix chart
#'
#' Adds an already existing echart to your new matrix chart. Charts with timelines are not supported.
#'
#' @inheritParams e_bar
#' @param chart An existing echart that you want to attach to your new matrix
#'   chart
#' @param coord X,Y Coordinate of matrix to place your new chart. Charts can
#'   cover multiple coordinates such as `list(c(0,1),0)`. See
#'   \href{https://echarts.apache.org/en/option.html#matrix.body.data.coord}{Matrix
#'   Coordinates} for more information on matrix coordinates.
#' @param id Unique id value that will be added when chart is placed in the
#'   matrix. Defaults to `"chart1"`
#' @param legend Whether chart contains a legend. Defaults to \code{TRUE}.
#' @param legend_pos Position of the legend. One of "top", "right", "bottom",
#'   "left". Determines to which side the `legend_space` argument applies.
#' @param legend_space Space between legend and plot area. Supports
#'   integers(pixels) or strings(percent of parent cell).
#' @param margin_trbl Adjusts the size of the outside margin around the plotting
#'   area. Default is `c(t = "15%", r = "5%", b = "5%", l = "5%")`. Supports
#'   integers(pixels) or strings(percent of parent cell) To change only e.g. two
#'   sides `c("r" = 8, "l" = 8)` could be used, other sides will use defaults.
#'
#'
#' @examples
#' echart <- iris |>
#' group_by(Species) |>
#'   e_charts(Sepal.Length) |>
#'   e_line(Sepal.Width) |>
#'   e_tooltip(trigger = "axis")
#'
#' e_matrix_raw(rows = 3, cols = 3, body = list(
#' itemStyle = list(borderWidth = 0))) |>
#' e_matrix_addChart(echart, coord = list(
#'    c(0,2),0), margin_trbl = c("b" = "20%"))
#'
#'
#' @rdname e_matrix_addChart
#' @export
e_matrix_addChart <- function(e,
                              chart,
                              coord,
                              id = "chart1",
                              legend = TRUE,
                              legend_pos = "bottom",
                              legend_space = "0%",
                              margin_trbl = c(t = "5%", r = "5%", b = "15%", l = "5%")
){
  if (missing(e)) {
    stop("must pass e", call. = FALSE)
  }
  if (missing(chart)) {
    stop("must provide an echart to add into matrix grid", call. = FALSE)
  }
  if (!("echarts4r" %in% class(chart))) {
    stop("the provided chart must be an echart", call. = FALSE)
  }
  if (missing(coord)) {
    stop("must provide coordinates for chart locations in matrix grid", call. = FALSE)
  }
  if (chart$x$tl) {
    stop("e_matrix_addChart does not currently support timeline charts", call. = FALSE)
  }
  
  if(!is.null(chart$x$opts$parallelAxis)){
    stop("e_matrix_addChart does not support parallel charts")
  }
  
  if(!is.null(chart$x$opts$calendar)){
    stop("e_matrix_addChart does not support calendar charts")
  }
  
  if(!is.null(chart$x$opts$radar)){
    stop("e_matrix_addChart does not support radar charts")
  }
  
  # Remove existing yAxis if using e_matrix instead of e_matrix_raw
  if(is.null(e$x$opts$yAxis[[1]]$id)){
    e$x$opts$yAxis <- NULL
  }
  
  
  # ID Check
  if(!is.null(e$x$opts$yAxis)){
    for(i in seq_along(e$x$opts$yAxis)){
      if(e$x$opts$yAxis[[i]]$id == id){
        stop("Chart id has already been used. Please supply a different id value")
      }
    }
  }

  # Get count of existing charts in matrix for future ids
  chart_count <- length(e$x$opts$xAxis)
  # Specified margins
  spec_margin <- c("t", "r", "b", "l") %in% names(margin_trbl) # see which elements are present
  margin_trbl <- c(margin_trbl, # add missing elements
                   c(t = 5, r = 5, b = 5, l = 5)[!spec_margin])

  # add new data into matrix chart
  e$x$data <- append(e$x$data, chart$x$data)

  #Build x and y axes if they are missing
  if(is.null(chart$x$opts$yAxis)){
    chart$x$opts$yAxis[[1]] <- list(show = FALSE)
  }
  if(is.null(chart$x$opts$xAxis)){
    chart$x$opts$xAxis[[1]] <- list(show = FALSE)
  }
  
  
  
  # add IDs for grid and matrix based on supplied id
  chart <- chart |> e_axis(axis = "x", id = id, gridId = id) |> e_axis(axis = "y", id = id, gridId = id)

  for(i in seq_along(chart$x$opts$series)){
    chart$x$opts$series[[i]]$xAxisId <- id
    chart$x$opts$series[[i]]$yAxisId <- id
    chart$x$opts$series[[i]]$xAxisIndex <- chart_count
    chart$x$opts$series[[i]]$yAxisIndex <- chart_count
    
    if(chart$x$opts$series[[i]]$type == "themeRiver"){
      stop("e_matrix_addChart does not support themeRiver charts")
    }
    
    if(chart$x$opts$series[[i]]$type == "pie"){
      chart$x$opts$series[[i]]$coordinateSystem <- "matrix"
      chart$x$opts$series[[i]]$coord <- coord
    }
  }
  
  ## Add visualMap
  if(!is.null(chart$x$opts$visualMap)){
    chart$x$opts$series[[1]]$id <- id
    chart$x$opts$visualMap[[1]]$coordinateSystem <- "matrix"
    chart$x$opts$visualMap[[1]]$seriesId <- id
    chart$x$opts$visualMap[[1]]$coord <- coord
    e$x$opts$visualMap <- append(e$x$opts$visualMap, chart$x$opts$visualMap)
  }

  # Attach series and axis information to the matrix chart
  e$x$opts$yAxis <- append(e$x$opts$yAxis, chart$x$opts$yAxis)
  e$x$opts$xAxis <- append(e$x$opts$xAxis, chart$x$opts$xAxis)
  e$x$opts$series <- append(e$x$opts$series, chart$x$opts$series)

  # Generate and link grid
  e$x$opts$grid <- append(e$x$opts$grid, list(list(id = id, coordinateSystem = "matrix", coord = coord
                                                   , left = margin_trbl[["l"]]
                                                   , top = margin_trbl[["t"]]
                                                   , right = margin_trbl[["r"]]
                                                   , bottom = margin_trbl[["b"]]
  )
  )
  )
  
  

  # Update and add legend
  if(legend){
    chart$x$opts$legend$coordinateSystem <- "matrix"
    chart$x$opts$legend$coord <- coord
    if(legend_pos == "top"){
      chart$x$opts$legend$top <- legend_space
    } else if(legend_pos == "right"){
      chart$x$opts$legend$right <- legend_space
    } else if(legend_pos == "left"){
      chart$x$opts$legend$left <- legend_space
    } else if(legend_pos == "bottom"){
      chart$x$opts$legend$bottom <- legend_space
    }
    e$x$opts$legend <- append(e$x$opts$legend, list(chart$x$opts$legend))
  }

  e
}


#' Generate Chart Titles for Matrix
#'
#' helper function for creating titles for every plot in a geofacet style
#' matrix. This generates the title using the name of the series.
#'
#' @inheritParams e_bar
#' @examples
#' df <- data.frame(group = rep(letters[1:6], each = 20),
#' date = seq(from = as.Date("2025-01-01"),
#'            to = as.Date("2025-01-20"), by = "day"),
#' temp = sample(c(10:20), size = 60, replace = TRUE))
#' grid <- data.frame(name = unique(df$group), row = c(1:6), col = c(1:6))
#'
#' df |>
#'   group_by(group) |>
#'   e_chart(date) |>
#'   e_line(temp, symbol = "none") |>
#'   e_x_axis(splitNumber = 2) |>
#'   e_y_axis(splitNumber = 2) |>
#'   e_geoFacet(legend = FALSE,
#'              grid = grid,
#'              margin_trbl = c("t"="25%"),
#'              left = "5%",
#'              width = "90%") |>
#'   e_title(text = "Group Temps") |>
#'   e_title_matrix(textStyle = list(fontSize = 10),
#'                  left = "center", top = "top")
#'
#' @seealso \href{https://echarts.apache.org/en/option.html#title}{Additional
#'   arguments}
#'
#' @rdname e_title_matrix
#' @export
e_title_matrix <- function(e, ...){
  if (missing(e)) {
    stop("must pass e", call. = FALSE)
  }

  for(i in seq_along(e$x$opts$series)){
    e$x$opts$title <- append(e$x$opts$title, list(list(text = e$x$opts$series[[i]]$name,
                                                       coordinateSystem = "matrix",
                                                       coord = e$x$opts$grid[[i]]$coord,
                                                       ...)
    ))
  }
  e
}


#' Create geoFacet Echart
#'
#' generates a faceted chart using matrix functionality. Provides similar
#' functionality to e_facet(). Similar to geoFacet package. Timeline is NOT
#' supported.
#'
#' @inheritParams e_bar
#' @param rows,cols Provide integer values for the number of rows and columns in
#'   the matrix grid
#' @param grid A custom grid containing row,col positions and name. Or a string
#'   containing the name of a premade geoFacet grid from the geoFacet package.
#' @param legend Whether chart contains a legend. Defaults to \code{TRUE}.
#' @param legend_pos Position of the legend. One of "top", "right", "bottom",
#'   "left". Determines to which side the `legend_space` argument applies.
#' @param legend_space Space between legend and plot area. Supports
#'   integers(pixels) or strings(percent of parent cell).
#' @param margin_trbl Adjusts the size of the outside margin around the plotting
#'   area. Default is `c(t = "15%", r = "5%", b = "5%", l = "5%")`. Supports
#'   integers(pixels) or strings(percent of parent cell) To change only e.g. two
#'   sides `c("r" = 8, "l" = 8)` could be used, other sides will use defaults.
#'
#'
#' @examples
#' if (require("geofacet", quietly = TRUE)) {
#'   library(geofacet)
#'
#'   df <- data.frame(group = rep(letters[1:6], each = 20),
#'   date = seq(from = as.Date("2025-01-01"),
#'              to = as.Date("2025-01-20"), by = "day"),
#'   temp = sample(c(10:20), size = 60, replace = TRUE))
#'   grid <- data.frame(name = unique(df$group), row = c(1:6), col = c(1:6))
#'
#'   df |>
#'     group_by(group) |>
#'     e_chart(date) |>
#'     e_line(temp, symbol = "none") |>
#'     e_x_axis(splitNumber = 2) |>
#'     e_y_axis(splitNumber = 2) |>
#'     e_geoFacet(legend = FALSE,
#'                grid = grid,
#'                margin_trbl = c("t"="25%"),
#'                left = "5%",
#'                width = "90%") |>
#'     e_title(text = "Group Temps")
#'
#'
#'   tem <- data.frame(state.x77)
#'   sta <- cbind(state.name, tem)
#'   colnames(sta)[1] <- "State"
#'
#'   sta |>
#'     group_by(State) |>
#'     e_charts(Population) |>
#'     e_scatter(Income) |>
#'     e_geoFacet(grid = "us_state_grid1") |>
#'     e_tooltip()
#'  }
#'
#' @rdname e_geoFacet
#' @export
e_geoFacet <- function(e,
                       rows,
                       cols,
                       grid,
                       legend = TRUE,
                       legend_pos = "top",
                       legend_space = "10%",
                       margin_trbl = c(t = "8%", r = "5%", b = "8%", l = "8%"),
                       ...){
  if (missing(e)) {
    stop("must pass e", call. = FALSE)
  }
  if(typeof(grid) !="character" & !is.data.frame(grid)){
    stop("must provide valid grid. Either name of geofacet grid object or custom dataframe grid", call. = FALSE)
  }

  if(typeof(grid)=="character"){
    check_installed_installer("geofacet")
    grid <- getExportedValue("geofacet", grid)
  }

  if(missing(rows) || missing(cols)){
    rows <- max(grid$row)
    cols <- max(grid$col)
  }

  e2 <- e_matrix_raw(rows = rows, cols = cols, ...)

  n <- length(e$x$opts$series)
  for(i in seq_len(nrow(grid))){
    e3 <- e
    series_pos <- NA
    for(j in 1:n){
      if(e$x$opts$series[[j]]$name == grid$name[i]){
        series_pos <- j
      }
    }
    if(is.na(series_pos)){
      warning(paste(grid$name[i], "data not found in series", sep = ": "))
      next
    }
    e3$x$opts$series <- list(e3$x$opts$series[[series_pos]])
    e3$x$data <- NULL
    e2 <- e2 |> e_matrix_addChart(e2,
                                  chart = e3,
                                  coord = list(grid$col[i]-1, grid$row[i]-1),
                                  id = paste0("chart",i),
                                  legend = FALSE,
                                  margin_trbl = margin_trbl)
  }

  e2
}


#' Generate pie chart for matrix
#'
#' Draw pie or donut chart in matrix coordinate system
#'
#' @inheritParams e_bar
#' @param x,y x and y column names for data to be used in the pie chart
#' @param legend Whether to add a legend, defaults to \code{TRUE}.
#' @examples
#'
#' df <- data.frame("Class" = rep(c("Class1", "Class2", "Class3"),each = 3),
#'                    "Grade" = c("Grade1","Grade2", "Grade3"),
#'                   "A" = sample(1:10, 9),
#'                   "B" = sample(1:10,9),
#'                   "C" = sample(1:10,9))
#'
#' df |> e_chart(x = A) |>
#'  e_matrix(xAxis = "Class", yAxis = "Grade") |>
#'  e_matrix_parent(value = "Primary", children = c("Class1", "Class2")) |>
#'  e_matrix_parent(value = "High", children = "Class3") |>
#'  e_matrix_corner(value = "All School",
#'    label = list(fontSize = 24, color = "#555", position = "inside")) |>
#'  e_pie(B, coord_system = "matrix", label = list(show = FALSE)) |>
#'  e_tooltip(trigger = "item") |>
#'  e_legend()
#'
#'  df |> e_chart() |>
#'   e_matrix(xAxis = "Class", yAxis = "Grade") |>
#'   e_pie_(serie = c("A","B","C"),
#'    coord_system = "matrix",
#'      label = list(show = FALSE)) |>
#'   e_tooltip(trigger = "item") |>
#'   e_legend()
#'
#'  df |> e_chart() |>
#'  e_matrix(xAxis = "Class", yAxis = "Grade") |>
#'  e_pie_matrix(x = "A", y = c( "B", "C"))
#'
#' @seealso \href{https://echarts.apache.org/en/option.html#series-pie}{Additional arguments}
#'
#' @rdname e_pie_matrix
#' @export
e_pie_matrix <- function(e, x, y, legend = TRUE, ...){

  if (missing(e)) {
    stop("must pass e", call. = FALSE)
  }

  if(is.null(e$x$opts$matrix)){
    stop("Matrix coordinate system must be built before adding data. e.g. e_matrix()")
  }
  e <- .rm_axis(e, TRUE, "x")
  e <- .rm_axis(e, TRUE, "y")

  base_nodes_x <- get_base_nodes(e$x$opts$matrix$x$data)
  base_nodes_y <- get_base_nodes(e$x$opts$matrix$y$data)

  for(i in seq_along(base_nodes_x)){
    for(j in seq_along(base_nodes_y)){
      center <- c(base_nodes_x[[i]], base_nodes_y[[j]])
      data <- e$x$data[[1]] |> dplyr::filter(.data[[e$x$opts$matrix$x$name]] == center[[1]] & .data[[e$x$opts$matrix$y$name]] == center[[2]]) |> dplyr::select(dplyr::all_of(c(x,y)))
      l_data <- list()
      for(k in seq_along(col(data))){
        l <- list(name = colnames(data)[k], value = data[,k])
        l_data <- append(l_data, list(l))
      }
      serie <- list(type = "pie",
                    coordinateSystem = "matrix",
                    center = center,
                    data = l_data,
                    ...
      )
      e$x$opts$series <- append(e$x$opts$series, list(serie))
    }
  }

  e
}


#' Generate scatter point for matrix
#'
#' Draw scatter points chart in matrix coordinate system
#'
#' @inheritParams e_bar
#' @param z Column name for data to be used for scatter points
#' @examples
#'
#' df <- data.frame("Class" = rep(c("Class1", "Class2", "Class3"),each = 3),
#'                    "Grade" = c("Grade1","Grade2", "Grade3"),
#'                   "A" = sample(1:10, 9))
#'
#' df |> e_chart() |>
#'  e_matrix(xAxis = "Class", yAxis = "Grade") |>
#'  e_matrix_parent(value = "Primary", children = c("Class1", "Class2")) |>
#'  e_matrix_parent(value = "High", children = "Class3") |>
#'  e_matrix_corner(value = "All School",
#'    label = list(fontSize = 24, color = "#555", position = "inside")) |>
#'  e_scatter(A, coord_system = "matrix", symbolSize = 0) |>
#'  e_labels(position = "inside",
#'           formatter = htmlwidgets::JS(
#'            'function(params){return(params.value[2]);}'),
#'           color = "#555",
#'           fontWeight = "bold")
#'
#' df |> e_chart() |>
#'  e_matrix(xAxis = "Class", yAxis = "Grade") |>
#'  e_scatter_matrix("A") |>
#'  e_labels(position = "inside",
#'           formatter = htmlwidgets::JS('function(params){return(params.value[2]);}'),
#'           color = "#555",
#'           fontWeight = "bold") |>
#'  e_visual_map(A, inRange = list(symbolSize = c(1,50)))
#'
#'
#' @seealso \href{https://echarts.apache.org/en/option.html#series-scatter}{Additional arguments}
#'
#' @rdname e_scatter_matrix
#' @export
e_scatter_matrix <- function(e, z, ...){
  if (missing(e)) {
    stop("must pass e", call. = FALSE)
  }

  if(is.null(e$x$opts$matrix)){
    stop("Matrix coordinate system must be built before adding data. e.g. e_matrix()")
  }
  e <- .rm_axis(e, TRUE, "x")
  e <- .rm_axis(e, TRUE, "y")

  base_nodes_x <- get_base_nodes(e$x$opts$matrix$x$data)
  base_nodes_y <- get_base_nodes(e$x$opts$matrix$y$data)

  all_data <- list()
  for(i in seq_along(base_nodes_x)){
    for(j in seq_along(base_nodes_y)){
      center <- c(base_nodes_x[[i]], base_nodes_y[[j]])
      data <- e$x$data[[1]] |> dplyr::filter(.data[[e$x$opts$matrix$x$name]] == center[[1]] & .data[[e$x$opts$matrix$y$name]] == center[[2]]) |> dplyr::select(dplyr::all_of(c(z)))
      serie <- list(center[[1]], center[[2]], data[[1]])
      all_data <- append(all_data, list(serie))
    }
  }

  serie <- list(type = "scatter", coordinateSystem = "matrix", data = all_data, ...)
  e$x$opts$series <- append(e$x$opts$series, list(serie))

  e
}


#' Generate heatmap for matrix
#'
#' Draw heatmap chart in matrix coordinate system
#'
#' @inheritParams e_bar
#' @param z_val Column name for data to be used for heatmap
#' @examples
#'
#' df <- data.frame("Class" = rep(c("Class1", "Class2", "Class3"),each = 3),
#'                    "Grade" = c("Grade1","Grade2", "Grade3"),
#'                   "A" = sample(1:10, 9))
#'
#' df |> e_chart() |>
#'  e_matrix(xAxis = "Class", yAxis = "Grade") |>
#'  e_matrix_parent(value = "Primary", children = c("Class1", "Class2")) |>
#'  e_matrix_parent(value = "High", children = "Class3") |>
#'  e_matrix_corner(value = "All School", label = list(
#'    fontSize = 24, color = "#555", position = "inside")) |>
#'  e_heatmap(A, coord_system = "matrix") |>
#'  e_labels(position = "inside",
#'           formatter = htmlwidgets::JS(
#'             'function(params){return(params.value[2]);}'),
#'           color = "#555",
#'           fontWeight = "bold")
#'
#' df |> e_chart() |>
#'  e_matrix(xAxis = "Class", yAxis = "Grade") |>
#'  e_heatmap_matrix("A") |>
#'  e_labels(position = "inside",
#'           formatter = htmlwidgets::JS(
#'             'function(params){return(params.value[2]);}'),
#'           fontWeight = "bold") |>
#'  e_visual_map(A, inRange = list(color = c("#bf444c", "#d88273", "#f6efa6")))
#'
#' @seealso \href{https://echarts.apache.org/en/option.html#series-scatter}{Additional arguments}
#'
#' @rdname e_heatmap_matrix
#' @export
e_heatmap_matrix <- function(e, z_val, ...){
  if (missing(e)) {
    stop("must pass e", call. = FALSE)
  }

  if(is.null(e$x$opts$matrix)){
    stop("Matrix coordinate system must be built before adding data. e.g. e_matrix()")
  }
  e <- .rm_axis(e, TRUE, "x")
  e <- .rm_axis(e, TRUE, "y")

  base_nodes_x <- get_base_nodes(e$x$opts$matrix$x$data)
  base_nodes_y <- get_base_nodes(e$x$opts$matrix$y$data)

  all_data <- list()
  for(i in seq_along(base_nodes_x)){
    for(j in seq_along(base_nodes_y)){
      center <- c(base_nodes_x[[i]], base_nodes_y[[j]])
      data <- e$x$data[[1]] |> dplyr::filter(.data[[e$x$opts$matrix$x$name]] == center[[1]] & .data[[e$x$opts$matrix$y$name]] == center[[2]]) |> dplyr::select(dplyr::all_of(c(z_val)))
      serie <- list(center[[1]], center[[2]], data[[1]])
      all_data <- append(all_data, list(serie))
    }
  }

  serie <- list(type = "heatmap", coordinateSystem = "matrix", data = all_data, ...)
  e$x$opts$series <- append(e$x$opts$series, list(serie))

  e
}
