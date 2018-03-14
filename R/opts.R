#' Visual Map
#' 
#' @inheritParams e_bar
#' @param calculable Whether show handles, which can be dragged to adjust "selected range".
#' @param type One of \code{continuous} or \code{piecewise}.
#' 
#' @export
e_visual_map <- function(e, calculable = TRUE, type = c("continuous", "piecewise"), ...){
  
  vm <- list(...)
  vm$calculable <- calculable
  vm$calculable <- type[1]
  
  e$x$opts$visualMap <- vm
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
