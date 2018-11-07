#' Axis 3D
#' 
#' Customise 3D axis.
#' 
#' @inheritParams e_bar
#' @param axis Axis to customise.
#' @param index Index of axis to customise.
#' 
#' @examples 
#' # phony data
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
#'   e_x_axis_3d(axisLine = list(lineStyle = list(color = "blue")))
#' 
#' @seealso \href{http://www.echartsjs.com/option-gl.html#xAxis3D}{Additional x arguments},
#'  \href{http://www.echartsjs.com/option-gl.html#yAxis3D}{Additional y arguments},
#'  \href{http://www.echartsjs.com/option-gl.html#zAxis3D}{Additional z arguments}
#' 
#' @rdname axis3d
#' @export
e_axis_3d <- function(e, axis = c("x", "y", "z"), index = 0, ...){
  
  if(missing(e))
    stop("missing e", call. = FALSE)
  
  axis <- .r2axis3D(axis[1])
  
  r.index <- index + 1
  max <- length(e$x$opts[[axis]])
  
  if(r.index > max)
    stop("invalid axis", call. = FALSE)
  
  attrs <- list(...)
  
  if(!length(attrs))
    stop("no attribute", call. = FALSE)
  
  for(i in 1:length(attrs)){
    arg <- names(attrs)[i]
    e$x$opts[[axis]][[r.index]][[arg]] <- attrs[[i]]
  }
  
  e
}

#' @rdname axis3d
#' @export
e_x_axis_3d <- function(e, index = 0, ...){
  if(missing(e))
    stop("missing e", call. = FALSE)
  e <- e_axis_3d(e, axis = "x", index = index, ...)
  e
}

#' @rdname axis3d
#' @export
e_y_axis_3d <- function(e, index = 0, ...){
  if(missing(e))
    stop("missing e", call. = FALSE)
  e <- e_axis_3d(e = e, axis = "y", index = index, ...)
  e
}

#' @rdname axis3d
#' @export
e_z_axis_3d <- function(e, index = 0, ...){
  if(missing(e))
    stop("missing e", call. = FALSE)
  e <- e_axis_3d(e = e, axis = "z", index = index, ...)
  e
}

#' Grid
#' 
#' Customise grid.
#' 
#' @inheritParams e_bar
#' @inheritParams e_axis
#' 
#' @examples 
#' # phony data
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
#'   e_grid_3d(splitLine = list(lineStyle = list(color = "blue")))
#' 
#' @seealso \href{http://www.echartsjs.com/option-gl.html#grid3D}{Additional arguments}
#' 
#' @export
e_grid_3d <- function(e, index = 0, ...){
  
  if(missing(e))
    stop("missing e", call. = FALSE)
  
  r.index <- index + 1
  
  # initialise of not existing
  if(!length(e$x$opts[["grid3D"]]))
    e$x$opts$grid3D  <- list()
  
  attrs <- list(...)
  
  if(!length(attrs))
    stop("no attribute", call. = FALSE)
  
  for(i in 1:length(attrs)){
    arg <- names(attrs)[i]
    e$x$opts$grid3D[[r.index]][[arg]] <- attrs[[i]]
  }
  
  e
}