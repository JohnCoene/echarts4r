#' Visual Map
#' 
#' @inheritParams e_bar
#' @param calculable Whether show handles, which can be dragged to adjust "selected range".
#' @param type One of \code{continuous} or \code{piecewise}.
#' 
#' @export
e_visual_map <- function(e, calculable = TRUE, type = c("continuous", "piecewise"), ...){
  
  if(missing(e))
    stop("must pass e", call. = FALSE)
  
  if(!length(e$x$opts$visualMap))
    e$x$opts$visualMap <- list()
  
  vm <- list(...)
  vm$calculable <- calculable
  vm$type <- type[1]
  
  e$x$opts$visualMap <- append(e$x$opts$visualMap, list(vm))
  e
}

#' Tooltip
#' 
#' Customise tooltip
#' 
#' @inheritParams e_bar
#' @param show Set to \code{FALSE} to hide the tooltip.
#' @param trigger Type of triggering, \code{item} or \code{axis}.
#' @param show.content Whether to show the tooltip floating layer.
#' @param always.show.content Whether to show tooltip content all the time. By default, 
#' it will be hidden after some time. It can be set to be \code{TRUE} to preserve displaying.
#' @param trigger.on Conditions to trigger tooltip; \code{mousemove|click}, \code{mousemove}, 
#' or \code{click}.
#' @param show.delay Delay time for showing tooltip, in millisecond. 
#' @param hide.delay Delay time for hiding tooltip, in ms. It will be invalid when 
#' \code{always.show.content} is true.
#' @param enterable Whether mouse is allowed to enter the floating layer of tooltip. 
#' If you need to interact in the tooltip like with links or buttons, it can be set as \code{TRUE}.
#' @param confine Whether tooltip content in the view rect of chart instance.
#' @param transition.duration The transition duration of tooltip's animation, in seconds.
#' 
#' @examples 
#' USArrests %>% 
#'   e_charts(Assault) %>% 
#'   e_bar(Murder) %>% 
#'   e_tooltip(trigger = "axis")
#' 
#' @export
e_tooltip <- function(e, show = TRUE, trigger = c("item", "axis"), show.content = TRUE,
                      trigger.on = c("mousemove|click", "mousemove", "click"),
                      show.delay = 0, hide.delay = 100, enterable = TRUE, confine = FALSE,
                      transition.duration = 0.4, always.show.content = FALSE, ...){
  
  if(missing(e))
    stop("must pass e", call. = FALSE)
  
  tooltip <- list(
    show = show,
    trigger = trigger[1],
    showContent = show.content,
    triggerOn = trigger.on,
    showDelay = show.delay,
    hideDelay = hide.delay,
    enterable = enterable,
    confine = confine,
    transitionDuration = transition.duration,
    alwaysShowContent = always.show.content,
    ...
  )
  
  e$x$opts$tooltip <- tooltip
  
  e
}

#' Legend
#' 
#' Customise the legend.
#' 
#' @inheritParams e_bar
#' @param show Set to \code{FALSE} to hide the legend.
#' @param type Type of legend, \code{plain} or \code{scroll}.
#' 
#' @examples 
#' mtcars %>% 
#'   head() %>% 
#'   dplyr::mutate(model = row.names(.)) %>% 
#'   e_charts(model) %>% 
#'   e_pie(carb) %>% 
#'   e_legend(FALSE)
#' 
#' @export
e_legend <- function(e, show = TRUE, type = c("plain", "scroll"), ...){
  
  if(missing(e))
    stop("must pass e", call. = FALSE)
  
  legend <- list(
    show = show,
    type = type[1],
    ...
  )
  
  e$x$opts$legend <- append(e$x$opts$legend, legend)
  
  e
  
}

#' Toolbox
#' 
#' Add toolbox interface.
#' 
#' @inheritParams e_bar
#' @param feature Feature to add, defaults to all.
#' 
#' @examples 
#' USArrests %>% 
#'   e_charts(UrbanPop) %>% 
#'   e_line(Assault) %>% 
#'   e_area(Murder, y.index = 1, x.index = 1) %>% 
#'   e_datazoom(x.index = 0) 
#' 
#' @export
e_toolbox_interface <- function(e, feature){
  
  if(missing(e))
    stop("must pass e", call. = FALSE)
  
  if(missing(feature))
    feature <- c("saveAsImage", "restore", "dataView", "dataView", "dataZoom", "magicType", "brush")
  
  if(!length(e$x$opts$toolbox))
    e$x$opts$toolbox <- list(feature = list())
  
  for(i in 1:length(feature)){
    e$x$opts$toolbox$feature[[feature[i]]] <- list()
  }
  
  e
}

#' Data zoom
#' 
#' Add data zoom.
#' 
#' @inheritParams e_bar
#' 
#' @examples 
#' USArrests %>% 
#'   e_charts(UrbanPop) %>% 
#'   e_line(Assault) %>% 
#'   e_area(Murder, y.index = 1, x.index = 1) %>% 
#'   e_y_axis(gridIndex = 1) %>%
#'   e_x_axis(gridIndex = 1) %>% 
#'   e_grid(height = "35%") %>% 
#'   e_grid(height = "35%", top = "50%") %>% 
#'   e_datazoom(x.index = c(0, 1))
#' 
#' @export
e_datazoom <- function(e, x.index = NULL, y.index = NULL, ...){
  
  if(missing(e))
    stop("must pass e", call. = FALSE)
  
  if(!is.null(x.index) && !is.null(y.index))
    stop("pass x.index or y.index, not both", call. = FALSE)
  
  if(!length(e$x$opts$dataZoom)) # initiatilise if not existing
    e$x$opts$dataZoom <- list()
  
  if(!length(e$x$opts$toolbox$feature$dataZoom))
    e <- e_toolbox_interface(e, "dataZoom")
  
  opts <- list(...)
  opts$xAxisIndex <- x.index
  opts$yAxisIndex <- y.index
  
  e$x$opts$dataZoom <- append(e$x$opts$dataZoom, opts)
  
  e
}

#' Brush
#' 
#' Add a brush.
#' 
#' @inheritParams e_bar
#' 
#' @export
e_brush <- function(e, x.index = NULL, y.index = NULL, ...){
  
  if(missing(e))
    stop("must pass e", call. = FALSE)
  
  if(!is.null(x.index) && !is.null(y.index))
    stop("pass x.index or y.index, not both", call. = FALSE)
  
  if(!length(e$x$opts$brush)) # initiatilise if not existing
    e$x$opts$brush <- list()
  
  if(!length(e$x$opts$toolbox$feature$brush))
    e <- e_toolbox_interface(e, "brush")
  
  opts <- list(
    brushLink = "all",
    ...
  )
  opts$xAxisIndex <- x.index
  opts$yAxisIndex <- y.index
  
  e$x$opts$brush <- append(e$x$opts$brush, opts)
  
  e
}