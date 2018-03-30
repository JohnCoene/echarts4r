#' Loading
#' 
#' Show or hide loading.
#' 
#' @inheritParams e_bar
#' @param text Text to display.
#' @param color Color of spinner.
#' @param text.color Color of text.
#' @param mask.color Color of mask.
#' @param zlevel Z level.
#' 
#' @examples 
#' mtcars %>% 
#'   e_charts(mpg) %>% 
#'   e_scatter(drat) %>% 
#'   e_show_loading()
#' 
#' @rdname loading
#' @export
e_show_loading <- function(e, text = "loading", color = "#c23531", text.color = "#000", 
                           mask.color = "rgba(255, 255, 255, 0.8)", zlevel = 0){
  e$x$loading <- TRUE
  opts <- list(
    text = text,
    color = color,
    textColor = text.color,
    maskColor = mask.color,
    zlevel = zlevel
  )
  e$x$loadingOpts <- opts
  e
}

#' @rdname loading
#' @export
e_hide_loading <- function(e){
  e$x$loading <- FALSE
  e
}