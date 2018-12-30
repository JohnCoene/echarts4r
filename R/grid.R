#' Axis
#' 
#' Customise axis.
#' 
#' @inheritParams e_bar
#' @param axis Axis to customise.
#' @param index Index of axis to customise.
#' @param formatter An axis formatter as returned by \code{\link{e_axis_formatter}}.
#' @param style Formatter style, one of \code{decimal}, \code{percent}, or \code{currency}.
#' @param currency Currency to to display.
#' @param digits Number of decimals.
#' @param locale Locale, if \code{NULL} then it is inferred from \code{Sys.getlocale}.
#' 
#' @section Functions:
#' \itemize{
#'   \item{\code{e_axis} to customise axis}
#'   \item{\code{e_rm_axis} to remove axis}
#' }
#' 
#' @examples 
#' # hide axis
#' USArrests %>% 
#'   e_charts(Assault) %>% 
#'   e_line(Murder, smooth = TRUE) %>% 
#'   e_line(Rape, y.index = 1) %>% # add secondary axis
#'   e_y_axis(index = 1, show = FALSE) # hide secondary axis
#'   
#' # use formatter
#' cars %>% 
#'   dplyr::mutate(
#'     speed = speed / 25
#'   ) %>% 
#'   e_charts(speed) %>% 
#'   e_scatter(dist) %>% 
#'   e_y_axis(
#'     formatter = e_axis_formatter("currency")
#'   ) %>% 
#'   e_x_axis(
#'     formatter = e_axis_formatter("percent", digits = 0)
#'   )
#' 
#' # plot all labels & rotate
#' USArrests %>% 
#'   head(10) %>% 
#'   dplyr::mutate(State = row.names(.)) %>% 
#'   e_charts(State) %>% 
#'   e_area(Murder) %>% 
#'   e_x_axis(axisLabel = list(interval = 0, rotate = 45)) # rotate
#'   
#'   
#' @seealso \href{https://ecomfe.github.io/echarts-doc/public/en/option.html#xAxis}{Additional x arguments}, 
#' \href{https://ecomfe.github.io/echarts-doc/public/en/option.html#yAxis}{Additional y arguments}
#' 
#' @rdname axis
#' @export
e_axis <- function(e, axis = c("x", "y", "z"), index = 0, formatter = NULL, ...){
  
  if(missing(e))
    stop("missing e", call. = FALSE)
  
  axis <- .r2axis(axis[1])
  
  r.index <- index + 1
  max <- length(e$x$opts[[axis]])
  
  attrs <- list(...)
  
  if(!is.null(formatter))
    attrs$axisLabel$formatter <- formatter
  
  if(!length(attrs))
    stop("no attribute", call. = FALSE)
  
  # initiatlise if wrong index
  if(r.index > max){
    r.index <- 1
  }
  
  if(!e$x$tl)
    dp <- .list_depth(e$x$opts[[axis]])
  else
    dp <- .list_depth(e$x$opts$baseOption[[axis]])
  
  if(dp >= 2){
    for(i in 1:length(attrs)){
      arg <- names(attrs)[i]
      if(!e$x$tl)
        e$x$opts[[axis]][[r.index]][[arg]] <- attrs[[i]]
      else
        e$x$opts$baseOption[[axis]][[r.index]][[arg]] <- attrs[[i]]
    }
  } else {
    for(i in 1:length(attrs)){
      arg <- names(attrs)[i]
      if(!e$x$tl)
        e$x$opts[[axis]][[arg]] <- attrs[[i]]
      else
        e$x$opts$baseOption[[axis]][[arg]] <- attrs[[i]]
    }
  }
  
  e
}

#' @rdname axis
#' @export
e_x_axis <- function(e, index = 0, formatter = NULL, ...){
  if(missing(e))
    stop("missing e", call. = FALSE)
  e <- e_axis(e, "x", index, formatter, ...)
  e
}

#' @rdname axis
#' @export
e_y_axis<- function(e, index = 0, formatter = NULL, ...){
  if(missing(e))
    stop("missing e", call. = FALSE)
  e <- e_axis(e = e, axis = "y",index =  index, formatter,...)
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

#' @rdname axis
#' @export
e_rm_axis <- function(e, axis = c("x", "y", "z")){
  axis <- .r2axis(axis[1])
  
  e$x$opts[[axis]] <- NULL
  
  return(e)
}

#' @rdname axis
#' @export
e_axis_formatter <- function(style = c("decimal", "percent", "currency"), digits = 0, 
                             locale = NULL, currency = "USD") {
  
  if(is.null(locale))
    locale <- .get_locale()
  
  style <- match.arg(style)
  opts <- list(
    style = style,
    minimumFractionDigits = digits,
    maximumFractionDigits = digits,
    currency = currency
  )
  htmlwidgets::JS(sprintf("function(value, index) {
        var fmt = new Intl.NumberFormat('%s', %s);
        return fmt.format(value);
    }", locale, jsonlite::toJSON(opts, auto_unbox = TRUE)))
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
#' @seealso \href{https://ecomfe.github.io/echarts-doc/public/en/option.html#grid}{Additional arguments}
#' 
#' @export
e_grid <- function(e, index = NULL, ...){
  
  if(missing(e))
    stop("missing e", call. = FALSE)
  
  if(is.null(index))
    index <- length(e$x$opts[["grid"]]) + 1
  
  attrs <- list(...)
  
  # initialise of not existing
  if(!e$x$tl){
    if(!length(e$x$opts[["grid"]]))
      e$x$opts$grid  <- list()
    
    e$x$opts$grid <- append(e$x$opts$grid, list(attrs))
  } else {
    if(!length(e$x$opts$baseOption[["grid"]]))
      e$x$opts$baseOption$grid  <- list()
    
    e$x$opts$baseOption$grid <- append(e$x$opts$baseOption$grid, list(attrs))
  }
  
  e
}

#' Radius axis
#' 
#' Customise radius axis.
#' 
#' @inheritParams e_bar
#' @param serie Serie to use as axis labels.
#' @param show Whether to display the axis.
#' 
#' @examples 
#' df <- data.frame(x = LETTERS[1:10], y = seq(1, 20, by = 2))
#' 
#' df %>% 
#'   e_charts(x) %>% 
#'   e_polar() %>% 
#'   e_angle_axis() %>% 
#'   e_radius_axis(x) %>% 
#'   e_bar(y, coord.system = "polar") 
#' 
#' @seealso \href{https://ecomfe.github.io/echarts-doc/public/en/option.html#radiusAxis}{Additional arguments}
#' 
#' @export
#' @name radius_axis
e_radius_axis <- function(e, serie, show = TRUE, ...){
  
  if(missing(e))
    stop("missing e", call. = FALSE)
  
  opts <- list(show = show, ...)
  
  if(!missing(serie))
    sr <- deparse(substitute(serie))
  
  if(!missing(serie))
    opts$data <- e$x$data %>% purrr::map(sr) %>% unlist %>% unname %>% unique %>% as.list
  
  if(!e$x$tl)
    e$x$opts$radiusAxis <- opts
  else
    e$x$opts$baseOption$radiusAxis <- opts
  
  e
}

#' @rdname radius_axis
#' @export
e_radius_axis_ <- function(e, serie = NULL, show = TRUE, ...){
  
  if(missing(e))
    stop("missing e", call. = FALSE)
  
  opts <- list(show = show, ...)
  
  if(!is.null(serie))
    opts$data <- e$x$data %>% purrr::map(serie) %>% unlist %>% unname %>% unique %>%  as.list
  
  if(e$x$tl)
    e$x$opts$radiusAxis <- opts
  else
    e$x$opts$baseOption$radiusAxis <- opts
  
  e
}

#' Angle axis
#' 
#' Customise angle axis.
#' 
#' @inheritParams e_bar
#' @param serie Serie to use as axis labels.
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
#' df <- data.frame(x = LETTERS[1:5], y = runif(5))
#' 
#' df %>% 
#'   e_charts(x) %>% 
#'   e_polar() %>% 
#'   e_angle_axis(x) %>% 
#'   e_radius_axis() %>% 
#'   e_line(y, coord.system = "polar", smooth = TRUE)
#' 
#' @seealso \href{https://ecomfe.github.io/echarts-doc/public/en/option.html#angleAxis}{Additional arguments}
#' 
#' @name angle_axis
#' @export
e_angle_axis <- function(e, serie, show = TRUE, ...){
  
  if(missing(e))
    stop("missing e", call. = FALSE)
  
  opts <- list(show = show, ...)
  
  if(!missing(serie))
    sr <- deparse(substitute(serie))
  
  if(!missing(serie))
    opts$data <- e$x$data %>% purrr::map(sr) %>% unlist %>% unname %>% unique %>% as.list
  
  if(!e$x$tl)
    e$x$opts$angleAxis <- opts
  else
    e$x$opts$baseOption$angleAxis <- opts
  
  e
}

#' @rdname angle_axis
#' @export
e_angle_axis_ <- function(e, serie = NULL, show = TRUE, ...){
  
  if(missing(e))
    stop("missing e", call. = FALSE)
  
  opts <- list(show = show, ...)
  
  if(!is.null(serie))
    opts$data <- e$x$data %>% purrr::map(serie) %>% unlist %>% unname %>% unique %>%  as.list
  
  if(!e$x$tl)
    e$x$opts$angleAxis <- opts
  else
    e$x$opts$baseOption$angleAxis <- opts
  
  e
}

#' Radar axis
#' 
#' Radar axis setup and options.
#' 
#' @inheritParams e_bar
#' @param index Index of axis to customise.
#' 
#' @export
e_radar_opts <- function(e, index = 0, ...){
  
  if(missing(e))
    stop("missing e", call. = FALSE)
  
  r.index <- index + 1
  max <- length(e$x$opts$radar)
  
  attrs <- list(...)
  
  if(!length(attrs))
    stop("no attribute", call. = FALSE)
  
  # initiatlise if wrong index
  if(r.index > max){
    r.index <- 1
    if(!e$xtl)
      e$x$opts$radar <- list(list())
    else
      e$x$opts$baseOption$radar <- list(list())
  }
  
  for(i in 1:length(attrs)){
    arg <- names(attrs)[i]
    if(!e$x$tl)
      e$x$opts$radar[[r.index]][[arg]] <- attrs[[i]]
    else
      e$x$opts$baseOption$radar[[r.index]][[arg]] <- attrs[[i]]
  }
  
  e
  
}

#' Single Axis
#' 
#' Setup single axis.
#' 
#' @inheritParams e_bar
#' @param index Index of axis to customise.
#' 
#' @examples 
#' df <- data.frame(
#'   axis = LETTERS[1:10],
#'   value = runif(10, 3, 20),
#'   size = runif(10, 3, 20)
#' )
#' 
#' df %>% 
#'   e_charts(axis) %>% 
#'   e_single_axis() %>% # add the single axis
#'   e_scatter(
#'     value,
#'     size, 
#'     coord.system = "singleAxis"
#'   ) 
#' 
#' @export
e_single_axis <- function(e, index = 0, ...){
  
  if(missing(e))
    stop("missing e", call. = FALSE)
  
  r.index <- index + 1
  
  e$x$opts$xAxis <- NULL
  e$x$opts$yAxis <- NULL
  
  if(!e$x$tl){
    if(!length(e$x$opts$singleAxis))
      e$x$opts$singleAxis <- list(...)
    else
      e$x$opts$singleAxis <- append(e$x$opts$singleAxis, list(...))
  } else {
    if(!length(e$x$opts$baseOption$singleAxis))
      e$x$opts$baseOption$singleAxis <- list(...)
    else
      e$x$opts$baseOption$singleAxis <- append(e$x$opts$baseOption$singleAxis, list(...))
  }
  
  if(!is.null(e$x$mapping$x)){
    
    type <- .get_type(e, e$x$mapping$x)
    
    if(!e$x$tl)
      e$x$opts$singleAxis$type <- type
    else
      e$x$opts$baseOption$singleAxis$type <- type
    
    if(type == "category" || type == "time"){
      
      vect <- c()
      for(i in 1:length(e$x$data)){
        dat <- e$x$data[[i]] %>% dplyr::select_(e$x$mapping$x) %>% unlist()
        dat <- unname(dat)
        dat <- as.character(dat)
        
        vect <- append(vect, dat)
      }
      
      vect <- unique(vect)
      
      if(!e$x$tl)
        e$x$opts$singleAxis$data <- vect
      else
        e$x$opts$baseOption$singleAxis$data <- vect
      
    }
  } else {
    warning("x not pass to e_charts", call. = FALSE)
  }
  
  e
}
