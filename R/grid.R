#' Axis
#' 
#' Customise axis.
#' 
#' @inheritParams e_bar
#' @param axis Axis to customise.
#' @param index Index of axis to customise.
#' 
#' @examples 
#' USArrests %>% 
#'   e_charts(Assault) %>% 
#'   e_line(Murder, smooth = TRUE) %>% 
#'   e_line(Rape, y.index = 1) %>% # add secondary axis
#'   e_y_axis(index = 1, show = FALSE) # hide secondary axis
#' 
#' # plot all labels & rotate
#' USArrests %>% 
#'   head(10) %>% 
#'   dplyr::mutate(State = row.names(.)) %>% 
#'   e_charts(State) %>% 
#'   e_area(Murder) %>% 
#'   e_x_axis(axisLabel = list(interval = 0, rotate = 45)) # rotate
#' 
#' @rdname axis
#' @export
e_axis <- function(e, axis = c("x", "y", "z"), index = 0, ...){
  
  if(missing(e))
    stop("missing e", call. = FALSE)
  
  axis <- .r2axis(axis[1])
  
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

#' @rdname axis
#' @export
e_x_axis <- function(e, index = 0, ...){
  if(missing(e))
    stop("missing e", call. = FALSE)
  e <- e_axis(e, "x", index, ...)
  e
}

#' @rdname axis
#' @export
e_y_axis<- function(e, index = 0, ...){
  if(missing(e))
    stop("missing e", call. = FALSE)
  e <- e_axis(e = e, axis = "y",index =  index, ...)
  e
}

#' @rdname axis
#' @export
e_z_axis <- function(e, index = 0, ...){
  if(missing(e))
    stop("missing e", call. = FALSE)
  e <- e_axis(e = e, axis = "z",index =  index, ...)
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
#' @export
e_grid <- function(e, index = NULL, ...){
  
  if(missing(e))
    stop("missing e", call. = FALSE)
  
  if(is.null(index))
    index <- length(e$x$opts[["grid"]]) + 1
  
  # initialise of not existing
  if(!length(e$x$opts[["grid"]]))
    e$x$opts$grid  <- list()
  
  attrs <- list(...)
  e$x$opts$grid <- append(e$x$opts$grid, list(attrs))
  
  e
}

#' Radius axis
#' 
#' Customise radius axis.
#' 
#' @inheritParams e_bar
#' @param show Whether to display the axis.
#' 
#' @examples 
#' df <- data.frame(x = 1:10, y = seq(1, 20, by = 2))
#' 
#' df %>% 
#'   e_charts(x) %>% 
#'   e_polar() %>% 
#'   e_angle_axis() %>% 
#'   e_radius_axis(FALSE) %>% 
#'   e_line(y, coord.system = "polar", smooth = TRUE) 
#' 
#' @export
e_radius_axis <- function(e, show = TRUE, ...){
  
  if(missing(e))
    stop("missing e", call. = FALSE)
  
  e$x$opts$radiusAxis <- list(show = show, ...)
  
  e
}

#' Angle axis
#' 
#' Customise angle axis.
#' 
#' @inheritParams e_bar
#' @param show Whether to display the axis.
#' 
#' @examples 
#' df <- data.frame(x = 1:100, y = seq(1, 200, by = 2))
#' 
#' df %>% 
#'   e_charts(x) %>% 
#'   e_polar(FALSE) %>% 
#'   e_angle_axis(FALSE) %>% 
#'   e_radius_axis(FALSE) %>% 
#'   e_line(y, coord.system = "polar", smooth = TRUE) %>% 
#'   e_legend(show = FALSE)
#' 
#' @export
e_angle_axis <- function(e, show = TRUE, ...){
  
  if(missing(e))
    stop("missing e", call. = FALSE)
  
  e$x$opts$angleAxis <- list(show = show, ...)
  
  e
}
