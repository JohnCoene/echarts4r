echarts_build <- function(e) {
  e$x$data <- NULL
  e$x$mapping <- NULL
  
  ff <- getOption("ECHARTS4R_FONT_FAMILY")
  theme <- getOption("ECHARTS4R_THEME")
  
  if(!is.null(theme))
    e <- e_theme(e, theme)
  
  if(!is.null(ff))
    e <- e_text_style(e, fontFamily = ff)
  
  e
} 

#' Initialise
#'
#' Initialise a chart.
#'
#' @param data A \code{data.frame}.
#' @param e An object of class \code{echarts4r} as returned by \code{e_charts}.
#' @param x Column name containing x axis.
#' @param draw Whether to draw the chart, intended to be used with \code{\link{e_draw_p}}.
#' @param width,height Must be a valid CSS unit (like \code{'100\%'},
#'   \code{'400px'}, \code{'auto'}) or a number, which will be coerced to a
#'   string and have \code{'px'} appended.
#' @param elementId Id of element.
#' @param dispose Set to \code{TRUE} to force redraw of chart, set to \code{FALSE} to update.
#' @param renderer Renderer, takes \code{canvas} (default) or \code{svg}.
#' @param timeline Set to \code{TRUE} to build a timeline, see timeline section.
#' @param ... Any other argument.
#' 
#' @section Timeline:
#' The timeline feature currently supports the following chart types.
#' \itemize{
#'   \item{\code{\link{e_bar}}}
#'   \item{\code{\link{e_line}}}
#'   \item{\code{\link{e_step}}}
#'   \item{\code{\link{e_area}}}
#'   \item{\code{\link{e_scatter}}}
#'   \item{\code{\link{e_effect_scatter}}}
#'   \item{\code{\link{e_candle}}}
#'   \item{\code{\link{e_heatmap}}}
#'   \item{\code{\link{e_pie}}}
#'   \item{\code{\link{e_line_3d}}}
#'   \item{\code{\link{e_lines_3d}}}
#'   \item{\code{\link{e_bar_3d}}}
#'   \item{\code{\link{e_lines}}}
#'   \item{\code{\link{e_scatter_3d}}}
#'   \item{\code{\link{e_scatter_gl}}}
#'   \item{\code{\link{e_histogram}}}
#'   \item{\code{\link{e_lm}}}
#'   \item{\code{\link{e_loess}}}
#'   \item{\code{\link{e_glm}}}
#'   \item{\code{\link{e_density}}}
#'   \item{\code{\link{e_pictorial}}}
#'   \item{\code{\link{e_boxplot}}}
#'   \item{\code{\link{e_map}}}
#'   \item{\code{\link{e_map_3d}}}
#'   \item{\code{\link{e_line_3d}}}
#'   \item{\code{\link{e_gauge}}}
#' }
#' 
#' @examples 
#' mtcars %>% 
#'   e_charts(qsec) %>%
#'   e_line(mpg)
#'
#' @import htmlwidgets
#' @importFrom grDevices boxplot.stats
#' @importFrom grDevices colorRampPalette
#' @importFrom stats as.formula lm glm loess predict
#' @importFrom graphics hist
#' 
#' @name init
#' @export
e_charts <- function(data, x, width = NULL, height = NULL, elementId = NULL, dispose = TRUE, 
                     draw = TRUE, renderer = "canvas", timeline = FALSE, ...) {

  xmap <- NULL
  
  if(!missing(x))
    xmap <- deparse(substitute(x))

  # forward options using x
  x = list(
    theme = "",
    tl = timeline,
    draw = draw, 
    renderer = tolower(renderer),
    mapping = list(),
    events = list(),
    buttons = list(),
    opts = list(
      ...,
      yAxis = list(
        list(show = TRUE)
      )
    )
  )
  
  if(!missing(data)){
    
    row.names(data) <- NULL
    
    if(!is.null(xmap) && !isTRUE(timeline))
      data <- .arrange_data_x(data, xmap)
    
    x$data <- map_grps_(data, timeline)
  }
  
  if(!is.null(xmap)){
    x$mapping$x <- xmap[1]
    x$mapping$x_class <- class(data[[xmap]])
    x <- .assign_axis(x, data)
  }
  
  if(isTRUE(timeline)){
    
    if(missing(data))
      stop("timeline expects data", call. = FALSE)
    
    if(!dplyr::is_grouped_df(data))
      stop("must pass grouped data when timeline = TRUE", call. = FALSE)
    
    if(!is.null(xmap))
      x$data <- .arrange_data_by_group(x$data, xmap)
    
    tl <- list(
      baseOption = list(
        yAxis = list(
          list(show = TRUE)
        )
      ),
      options = purrr::map(1:dplyr::n_groups(data), function(x) list())      
    )
    
    x$opts <- tl
    
    x$opts$baseOption$timeline <- list(
      data = as.list(names(x$data)),
      axisType = "category",
      ...
    )
    
    if(!is.null(xmap)){
      x$mapping$x <- xmap[1]
      x$mapping$x_class <- class(data[[xmap]])
      
      x$mapping$include_x <- FALSE
      cl <- x$mapping$x_class
      if(cl == "character" || cl == "factor"){
        labs <- unique(data[[x$mapping$x]])
        
        if(length(labs) == 1)
          labs <- list(labs)
        
        x$opts$baseOption$xAxis <- list(list(data = labs, type = "category", boundaryGap = TRUE))
      } else if(cl == "POSIXct" || cl == "POSIXlt" || cl == "Date") {
        
        labs <- unique(data[[x$mapping$x]])
        
        if(length(labs) == 1)
          labs <- list(labs)
        
        x$opts$baseOption$xAxis <- list(list(data = labs, type = "time", boundaryGap = TRUE))
      } else {
        x$mapping$include_x <- TRUE
        x$opts$baseOption$xAxis <- list(list(type = "value"))
      }
    }
  }
  
  x$dispose <- dispose
  
  # create widget
  htmlwidgets::createWidget(
    name = 'echarts4r',
    x,
    width = width,
    height = height,
    package = 'echarts4r',
    elementId = elementId,
    preRenderHook = echarts_build,
    sizingPolicy = htmlwidgets::sizingPolicy(
      defaultWidth = "100%",
      knitr.figure = FALSE,
      browser.fill = TRUE,
      padding = 0
    )
  )
}

#' @rdname init
#' @export
e_charts_ <- function(data, x = NULL, width = NULL, height = NULL, elementId = NULL, dispose = TRUE, 
                      draw = TRUE, renderer = "canvas", timeline = FALSE, ...) {
  
  xmap <- x
  
  # forward options using x
  x = list(
    theme = "",
    tl = timeline,
    renderer = tolower(renderer),
    draw = draw,
    mapping = list(),
    events = list(),
    buttons = list(),
    opts = list(
      ...,
      yAxis = list(
        list(show = TRUE)
      )
    )
  )
  
  if(!missing(data)){
    
    row.names(data) <- NULL
    
    if(!is.null(xmap))
      data <- .arrange_data_x(data, xmap)
    
    x$data <- map_grps_(data, timeline)
  }
  
  if(!is.null(xmap)){
    x$mapping$x <- xmap
    x$mapping$x_class <- class(data[[xmap]])
    x <- .assign_axis(x, data)
  }
  
  x$dispose <- dispose
  
  # create widget
  htmlwidgets::createWidget(
    name = 'echarts4r',
    x,
    width = width,
    height = height,
    package = 'echarts4r',
    elementId = elementId,
    sizingPolicy = htmlwidgets::sizingPolicy(
      defaultWidth = "100%",
      knitr.figure = FALSE,
      browser.fill = TRUE
    )
  )
}

#' @rdname init
#' @export
e_chart <- e_charts


#' Add a dataset 
#' This function can add one or more dataset into a echarts object. 
#' 
#' @examples 
#' points <- mtcars[1:3,]
#' mtcars %>% 
#'   e_charts_("qsec") %>%
#'   e_line(mpg) %>%
#'   e_data(points, qsec) %>%
#'   e_scatter(mpg, color = "blue")
#'
#' @rdname init
#' @export
e_data <- function(e, data, x){
  
  if(missing(data))
    data <- e$x$data[[1]]
  
  if(!missing(x))
    xmap <- deparse(substitute(x))
  
  if(!missing(x)){
    e$x$mapping$x <- xmap
    e$x$mapping$x_class <- class(data[[xmap]])
    e$x <- .assign_axis(e$x, data)
  }
  
  row.names(data) <- NULL
  
  if(!missing(x))
    data <- .arrange_data_x(data, xmap)
  
  data <- map_grps_(data, FALSE)
  e$x$data <- data
  
  e
}

#' Shiny bindings for echarts4r
#'
#' Output and render functions for using echarts4r within Shiny
#' applications and interactive Rmd documents.
#'
#' @param outputId output variable to read from.
#' @param width,height Must be a valid CSS unit (like \code{'100\%'},
#'   \code{'400px'}, \code{'auto'}) or a number, which will be coerced to a
#'   string and have \code{'px'} appended.
#' @param expr An expression that generates a echarts4r
#' @param env The environment in which to evaluate \code{expr}.
#' @param quoted Is \code{expr} a quoted expression (with \code{quote()})? This
#'   is useful if you want to save an expression in a variable.
#' @param id Target chart id.
#' @param session Shiny session.
#' 
#' @section Callbacks:
#' \itemize{
#'   \item{\code{id_brush}: returns data on brushed data points.}
#'   \item{\code{id_legend_change}: returns series name of legend selected/unselected.}
#'   \item{\code{id_clicked_data}: returns data of clicked data point.}
#'   \item{\code{id_clicked_data_value}: returns value of clicked data point.}
#'   \item{\code{id_clicked_row}: returns row number of clicked data point.}
#'   \item{\code{id_clicked_serie}: returns name of serie of clicked data point.}
#'   \item{\code{id_mouseover_data}: returns data on hovered data point.}
#'   \item{\code{id_mouseover_data_value}: returns value of hovered data point.}
#'   \item{\code{id_mouseover_row}: returns row o hovered data point.}
#'   \item{\code{id_mouseover_serie}: returns name of serie of hovered data point.}
#' }
#' 
#' @section Proxies:
#' \itemize{
#'   \item{\code{\link{e_append1_p}} & \code{\link{e_append2_p}}}
#'   \item{\code{\link{e_showtip_p}} & \code{\link{e_hidetip_p}}}
#'   \item{\code{\link{e_highlight_p}} & \code{\link{e_downplay_p}}}
#'   \item{\code{\link{e_focus_adjacency}} & \code{\link{e_unfocus_adjacency}}}
#'   \item{\code{\link{e_dispatch_action_p}}}
#' }
#' 
#'
#' @name echarts4r-shiny
#'
#' @export
echarts4rOutput <- function(outputId, width = '100%', height = '400px'){
  htmlwidgets::shinyWidgetOutput(outputId, 'echarts4r', width, height, package = 'echarts4r')
}

#' @rdname echarts4r-shiny
#' @export
renderEcharts4r <- function(expr, env = parent.frame(), quoted = FALSE) {
  if (!quoted) { expr <- substitute(expr) } # force quoted
  htmlwidgets::shinyRenderWidget(expr, echarts4rOutput, env, quoted = TRUE)
}

#' @rdname echarts4r-shiny
#' @export
echarts4rProxy <- function(id, session = shiny::getDefaultReactiveDomain()){
  
  proxy <- list(id = id, session = session)
  class(proxy) <- "echarts4rProxy"
  
  return(proxy)
}