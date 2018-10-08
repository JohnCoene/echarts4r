#' Axis 3D
#' 
#' Customise 3D axis.
#' 
#' @inheritParams e_bar
#' @param axis Axis to customise.
#' @param index Index of axis to customise.
#' 
#' @seealso \href{https://ecomfe.github.io/echarts-doc/public/en/option.html#xAxis3D}{Additional x arguments},
#'  \href{https://ecomfe.github.io/echarts-doc/public/en/option.html#yAxis3D}{Additional y arguments},
#'  \href{https://ecomfe.github.io/echarts-doc/public/en/option.html#zAxis3D}{Additional arguments}
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
#' USArrests %>% 
#'   e_charts(UrbanPop) %>% 
#'   e_line(Assault, smooth = TRUE) %>% 
#'   e_area(Murder, y.index = 1, x.index = 1) %>% 
#'   e_y_axis(gridIndex = 1) %>%
#'   e_x_axis(gridIndex = 1) %>% 
#'   e_grid(height = "40%") %>% 
#'   e_grid(height = "40%", top = "55%")
#' 
#' @seealso \href{https://ecomfe.github.io/echarts-doc/public/en/option.html#grid3D}{Additional arguments}
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