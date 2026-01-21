#' Add nested data
#'
#' Utility function to add data where the original JavaScript library expects nested data.
#'
#' @inheritParams e_bar
#' @param param The nested parameter to add data to.
#' @param .serie Serie's index to add the data to, if
#' `NULL` then it is added to all.
#' @param .data A dataset to use, if none are specified than
#' the original dataset passed to `e_charts` is used.
#' @param value The column to map to the parameter.
#'
#' @section Functions:
#'
#' - `e_add_nested`: Adds nested data, e.g.:
#' `e_add_nested("itemStyle", color, fontBold)` creates
#' `\{itemStyle: \{color: 'red', fontBold: 'bold'\}\}`.
#' - `e_add_unnested`: Adds unnested data, e.g.:
#' `e_add_unnested("symbolSize", size)` creates
#' `\{symbolSize: 4\}`.
#'
#' @details For instance, \code{\link{e_funnel}} lets you pass \code{values} and \code{labels}
#' (from your initial data.frame) which corresponds to \code{name} and \code{value} in the
#' \href{https://echarts.apache.org/en/option.html#series-heatmap.data}{original library}.
#' However the latter also takes, \code{label}, \code{itemStyle}, and \code{emphasis} but being JSON arrays
#' they translate to lists in R and dealing with nested data.frames is not ideal. \code{e_add} remedies to that.
#' It allows adding those nested data points, see the examples below.
#'
#' @examples
#' # funnel can take nested itemStyle
#' # https://echarts.apache.org/en/option.html#series-funnel.data
#' funnel <- data.frame(
#'   stage = c("View", "Click", "Purchase"),
#'   value = c(80, 30, 20),
#'   color = c("blue", "red", "green")
#' )
#'
#' funnel |>
#'   e_charts() |>
#'   e_funnel(value, stage) |>
#'   e_add_nested("itemStyle", color)
#'
#' # Heatmap can take nested label
#' # https://echarts.apache.org/en/option.html#series-heatmap.data
#' v <- LETTERS[1:10]
#' matrix <- data.frame(
#'   x = sample(v, 300, replace = TRUE),
#'   y = sample(v, 300, replace = TRUE),
#'   z = rnorm(300, 10, 1),
#'   stringsAsFactors = FALSE
#' ) |>
#'   dplyr::group_by(x, y) |>
#'   dplyr::summarise(z = sum(z)) |>
#'   dplyr::ungroup() |>
#'   dplyr::mutate(
#'     show = TRUE,
#'     fontStyle = round(runif(dplyr::n(), 5, 12))
#'   )
#'
#' matrix |>
#'   e_charts(x) |>
#'   e_heatmap(y, z) |>
#'   e_visual_map(z) |>
#'   e_add_nested(
#'     "label",
#'     show,
#'     fontStyle
#'   )
#'
#' @name nesting
#' @export
e_add <- function(e, param, ..., .serie = NULL, .data = NULL) {
  .Deprecated("e_add_nested", "echarts4r")
  e_add_nested(e, param, ..., .serie = NULL, .data = NULL)
}

#' @rdname nesting
#' @export
e_add_nested <- function(e, param, ..., .serie = NULL, .data = NULL) {
  if (missing(e) || missing(param)) {
    stop("missing e or param", call. = FALSE)
  }

  if(is.null(.data))
    ds <- e$x$data
  else
    ds <- map_grps_(.data, e$x$tl)

  for (i in seq_along(ds)) {
    data <- ds[[i]] |>
      dplyr::select(...)

    data <- apply(data, 1, as.list)

    for (j in seq_along(data)) {

      if (!e$x$tl) {
        for(k in seq_along(e$x$opts$series)){
          if(!is.null(.serie) && .serie != k)
            next
          if(!is.numeric(e$x$opts$series[[k]]$data[[j]])){
        e$x$opts$series[[k]]$data[[j]][[param]] <- data[[j]]
          } else {
            # If polar, 'itemStyle' may be in a different location - this fixes that.
            e$x$opts$series[[k]]$data[[j]] <- c(
              list(value = e$x$opts$series[[k]]$data[[j]]),
              stats::setNames(list(data[[j]]), param)
            )
          }
        }
      } else {
        for(k in seq_along(e$x$opts$options[[i]]$series)){
          if(!is.null(.serie) && .serie != k)
            next
          e$x$opts$options[[i]]$series[[k]]$data[[j]][[param]] <- data[[j]]
        }
      }
    }
  }
  e
}

#' @rdname nesting
#' @export
e_add_unnested <- function(e, param, value, .serie = NULL, .data = NULL) {
  if (missing(e) || missing(param) || missing(value)) {
    stop("missing e, param, or value", call. = FALSE)
  }

  if(is.null(.data))
    ds <- e$x$data
  else
    ds <- map_grps_(.data, e$x$tl)

  for (i in seq_along(ds)) {
    data <- ds[[i]] |>
      dplyr::pull({{ value }}) |>
      unname()

    for (j in seq_along(data)) {
      if(!is.null(.serie) && .serie != j)
        next

      if (!e$x$tl) {
        if(!is.numeric(e$x$opts$series[[i]]$data[[j]])){
        e$x$opts$series[[i]]$data[[j]][[param]] <- data[j]
        } else {
          # If polar, 'itemStyle' may be in a different location - this fixes that.N
          e$x$opts$series[[i]]$data[[j]] <- c(
            list(value = e$x$opts$series[[i]]$data[[j]]),
            stats::setNames(list(data[[j]]), param)
          )
        }
      } else {
        for(k in seq_along(e$x$opts$options[[i]]$series)){
          e$x$opts$options[[i]]$series[[k]]$data[[j]][[param]] <- data[j]
        }
      }
    }
  }
  e
}


#' Insert binded data
#'
#' Utility function to attach an existing column(s) from your data to an existing series
#'
#' @inheritParams e_bar
#' @param .serie Serie's index to add the data.
#' @param .data Column names for the new data.
#'
#' @details This inserts new data into a series's values which allows for use in mapping things like e_visual_map 
#' or tooltip parameters. This function also works when using timelines.
#'
#' @examples
#' mtcars |>
#'  e_charts(mpg) |>
#'  e_scatter(wt, scale = e_scale) |>
#'  e_insert_data(.serie = 1, .data = "qsec") |>
#'  e_visual_map(qsec, scale = e_scale, dimension = 2) |> 
#'  e_tooltip(trigger = "item", 
#'            formatter = htmlwidgets::JS("function(params) {
#'                                        return 'mpg: ' + params.value[0] + 
#'                                        '<br />wt: ' + params.value[1] +
#'                                        '<br />qsec: ' + params.value[2];
#'                                        }")
#'  ) 
#'
#' @name e_insert_data
#' @export
e_insert_data <- function(e, .serie, .data){
  if(!e$x$tl){
    for(i in seq_along(e$x$data)) {
      bind <- e$x$data[[i]] |>
        dplyr::select(dplyr::all_of(.data)) |>
        unname() |>
        unlist()
      for(j in seq_along(e$x$opts$series[[.serie]]$data)){
        e$x$opts$series[[.serie]]$data[[j]]$value <- append(e$x$opts$series[[.serie]]$data[[j]]$value, bind[j])
      }
    }
  } else {
    for(i in seq_along(e$x$data)) {
      bind <- e$x$data[[i]] |>
        dplyr::select(dplyr::all_of(.data)) |>
        unname() |>
        unlist()
      for(j in seq_along(e$x$opts$options[[i]]$series[[.serie]]$data)){
        e$x$opts$options[[i]]$series[[.serie]]$data[[j]]$value <- append(e$x$opts$options[[i]]$series[[.serie]]$data[[j]]$value, bind[j])
      }
    }
  }
  e
}
