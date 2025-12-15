.get_base_nodes <- function(x) {
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
#' @param coordClamp determines whether null values can be used to indicate an entire row/column
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

#' Generate pie chart for matrix
#'
#' Draw pie or donut chart in matrix coordinate system
#'
#' @inheritParams e_bar
#' @param x,y x and y column names for data to be used in the pie chart
#' @param legend Whether to add a legend, defaults to \code{TRUE}.
#' @examples
#'
# df <- data.frame("Class" = rep(c("Class1", "Class2", "Class3"),each = 3),
#                    "Grade" = c("Grade1","Grade2", "Grade3"),
#                   "A" = sample(1:10, 9),
#                   "B" = sample(1:10,9))
#
# df |> e_chart(x = A) |>
#  e_matrix(xAxis = "Class", yAxis = "Grade") |>
#  e_matrix_parent(value = "Primary", children = c("Class1", "Class2")) |>
#  e_matrix_parent(value = "High", children = "Class3") |>
#  e_matrix_corner(coord = c(-2,-2), value = "All School", label = list(
#   fontSize = 24, color = "#555", position = "inside")) |>
#  e_pie(B, coord_system = "matrix", label = list(show = FALSE)) |>
#  e_tooltip(trigger = "item") |>
#  e_legend()
#'
#'
#' @seealso \href{https://echarts.apache.org/en/option.html#series-pie}{Additional arguments}
#'
#' @rdname e_matrix_pie
#' @export
e_matrix_pie <- function(e, x, y, legend = TRUE, ...){

  if (missing(e)) {
    stop("must pass e", call. = FALSE)
  }

  if(is.null(e$x$opts$matrix)){
    stop("Matrix coordinate system must be built before adding data. e.g. e_matrix()")
  }

  base_nodes_x <- .get_base_nodes(e$x$opts$matrix$x$data)
  base_nodes_y <- .get_base_nodes(e$x$opts$matrix$y$data)

  for(i in 1:length(base_nodes_x)){
    for(j in 1:length(base_nodes_y)){
      center <- c(base_nodes_x[[i]], base_nodes_y[[j]])
      data <- e$x$data[[1]] |> dplyr::filter(.data[[e$x$opts$matrix$x$name]] == center[[1]] & .data[[e$x$opts$matrix$y$name]] == center[[2]]) |> dplyr::select(dplyr::all_of(c(x,y)))
      l_data <- list()
      for(k in 1:length(col(data))){
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

  if (isTRUE(e$x$tl)) {
    if (isTRUE(legend)) {
      e$x$opts$baseOption$legend$data <- append(
        e$x$opts$baseOption$legend$data,
        purrr::map(e$x$data, "model") |>
          unlist() |>
          unique()
      )
    }
  }

  e
}

