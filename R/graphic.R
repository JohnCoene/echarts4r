#' Graphic
#' 
#' Low level API to define graphic elements.
#' 
#' @inheritParams  e_bar
#' 
#' @section Functions:
#' \itemize{
#'   \item{\code{g_graphic_g} to initialise graphics, entirely optional.}
#'   \item{\code{g_group_g} to create group, the children of which will share attributes.}
#'   \item{\code{g_image_g} to a png or jpg image.}
#'   \item{\code{g_text_g} to add text.}
#'   \item{\code{g_rect_g} to add a rectangle.}
#'   \item{\code{g_circle_g} to add a circle.}
#'   \item{\code{g_ring_g} to add a ring.}
#'   \item{\code{g_sector_g}}
#'   \item{\code{g_arc_g} to create an arc.}
#'   \item{\code{g_polygon_g} to create a polygon.}
#'   \item{\code{g_polyline_g} to create a polyline.}
#'   \item{\code{g_line_g} to draw a line.}
#'   \item{\code{g_bezier_curve_g} to draw a quadratic bezier curve or cubic bezier curve.}
#' }
#' 
#' @note Some elements, i.e.: \code{e_image_g} may not display in the RStudio browwser but will work fine in your browser, R markdown documents and Shiny applications.
#' 
#' @examples 
#' # may not work in RStudio viewer
#' # Open in browser
#' cars %>% 
#'   e_charts(speed) %>% 
#'   e_scatter(dist) %>%
#'   e_image_g(
#'     right = 20,
#'     top = 20,
#'     z = -999,
#'     style = list(
#'       image = "https://www.r-project.org/logo/Rlogo.png",
#'       width = 150,
#'       height = 150,
#'       opacity = .6
#'     )
#'   )
#' 
#' @seealso \href{https://ecomfe.github.io/echarts-doc/public/en/option.html#graphic}{official documentation}
#' 
#' @rdname graphic
#' @export
e_graphic_g <- function(e, ...){
  
  if(missing(e))
    stop("must pass e", call. = FALSE)
  
  if(!e$x$tl)
    e$x$opts$graphic <- list(...)
  else
    e$x$opts$baseOption$graphic <- list(...)
  e
}

#' @rdname graphic
#' @export
e_group_g <- function(e, ...){
  
  if(missing(e))
    stop("must pass e", call. = FALSE)
  
  .e_graphic_elem(e, "group", ...)
}

#' @rdname graphic
#' @export
e_image_g <- function(e, ...){
  
  if(missing(e))
    stop("must pass e", call. = FALSE)
  
  .e_graphic_elem(e, "image", ...)
}

#' @rdname graphic
#' @export
e_text_g <- function(e, ...){
  
  if(missing(e))
    stop("must pass e", call. = FALSE)
  
  .e_graphic_elem(e, "text", ...)
}

#' @rdname graphic
#' @export
e_rect_g <- function(e, ...){
  
  if(missing(e))
    stop("must pass e", call. = FALSE)
  
  .e_graphic_elem(e, "rect", ...)
}

#' @rdname graphic
#' @export
e_circle_g <- function(e, ...){
  
  if(missing(e))
    stop("must pass e", call. = FALSE)
  
  .e_graphic_elem(e, "circle", ...)
}

#' @rdname graphic
#' @export
e_ring_g <- function(e, ...){
  
  if(missing(e))
    stop("must pass e", call. = FALSE)
  
  .e_graphic_elem(e, "ring", ...)
}

#' @rdname graphic
#' @export
e_sector_g <- function(e, ...){
  
  if(missing(e))
    stop("must pass e", call. = FALSE)
  
  .e_graphic_elem(e, "sector", ...)
}

#' @rdname graphic
#' @export
e_arc_g <- function(e, ...){
  
  if(missing(e))
    stop("must pass e", call. = FALSE)
  
  .e_graphic_elem(e, "arc", ...)
}

#' @rdname graphic
#' @export
e_polygon_g <- function(e, ...){
  
  if(missing(e))
    stop("must pass e", call. = FALSE)
  
  .e_graphic_elem(e, "polygon", ...)
}

#' @rdname graphic
#' @export
e_polyline_g <- function(e, ...){
  
  if(missing(e))
    stop("must pass e", call. = FALSE)
  
  .e_graphic_elem(e, "polyline", ...)
}

#' @rdname graphic
#' @export
e_line_g <- function(e, ...){
  
  if(missing(e))
    stop("must pass e", call. = FALSE)
  
  .e_graphic_elem(e, "line", ...)
}

#' @rdname graphic
#' @export
e_bezier_curve_g <- function(e, ...){
  
  if(missing(e))
    stop("must pass e", call. = FALSE)
  
  .e_graphic_elem(e, "bezierCurve", ...)
}

#' Draft
#' 
#' Add a draft watermark to your graph.
#' 
#' @inheritParams e_bar
#' @param text Text to display.
#' @param size Font size of text.
#' @param opacity,color Opacity and color of text.
#' 
#' @examples 
#' cars %>% 
#'   e_charts(speed) %>% 
#'   e_scatter(dist) %>%
#'   e_draft()
#' 
#' @export
e_draft <- function(e, text = "DRAFT", size = "120px", opacity = 0.4, color = "#d3d3d3"){
  e %>% 
    e_text_g(
      left = "center",
      top = "center",
      z = -1000,
      style = list(
        fill = color,
        text = text,
        font = paste(size, '"STHeiti", sans-serif'),
        opacity = opacity
      )
    )
}