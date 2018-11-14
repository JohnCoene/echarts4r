#' Visual Map
#' 
#' @inheritParams e_bar
#' @param serie Column name of serie to scale against.
#' @param calculable Whether show handles, which can be dragged to adjust "selected range".
#' @param type One of \code{continuous} or \code{piecewise}.
#' @param scale A function that takes a vector of \code{numeric} and returns a vector of \code{numeric}
#' of the same length.
#' 
#' @section Scaling function: defaults to \code{e_scale} which is a basic function that rescales \code{size}
#' between 1 and 20 for that makes for decent sized points on the chart.
#' 
#' @examples 
#' mtcars %>% 
#'   e_charts(mpg) %>% 
#'   e_scatter(wt, qsec, scale = e_scale) %>% 
#'   e_visual_map(qsec, scale = e_scale)
#' 
#' v <- LETTERS[1:10]
#' matrix <- data.frame(
#'   x = sample(v, 300, replace = TRUE), 
#'   y = sample(v, 300, replace = TRUE), 
#'   z = rnorm(300, 10, 1),
#'   color = rnorm(300, 10, 1),
#'   size = rnorm(300, 10, 1),
#'   stringsAsFactors = FALSE
#' ) %>% 
#'   dplyr::group_by(x, y) %>% 
#'   dplyr::summarise(
#'     z = sum(z),
#'     color = sum(color),
#'     size = sum(size)
#'   ) %>% 
#'   dplyr::ungroup() 
#'   
#' matrix %>% 
#'   e_charts(x) %>% 
#'   e_scatter_3d(y, z, color, size) %>% 
#'   e_visual_map(
#'     z, # scale to z
#'     inRange = list(symbolSize = c(1, 30)), # scale size
#'     dimension = 3 # third dimension 0 = x, y = 1, z = 2, size = 3
#'   ) %>% 
#'   e_visual_map(
#'     z, # scale to z
#'     inRange = list(color = c('#bf444c', '#d88273', '#f6efa6')), # scale colors
#'     dimension = 4, # third dimension 0 = x, y = 1, z = 2, size = 3, color = 4
#'     bottom = 300 # padding to avoid visual maps overlap
#'   )
#' 
#' @seealso \href{https://ecomfe.github.io/echarts-doc/public/en/option.html#visualMap}{Additional arguments}
#' 
#' @rdname e_visual_map
#' @export
e_visual_map <- function(e, serie, calculable = TRUE, type = c("continuous", "piecewise"), scale = NULL, ...){
  
  if(missing(e))
    stop("must pass e", call. = FALSE)
  
  if(!missing(serie))
    serie <- deparse(substitute(serie))
  else
    serie <- NULL
  
  e_visual_map_(e, serie, calculable, type, scale, ...)
}

#' @rdname e_visual_map
#' @export
e_visual_map_ <- function(e, serie = NULL, calculable = TRUE, type = c("continuous", "piecewise"), scale = NULL, ...){
  
  if(missing(e))
    stop("must pass e", call. = FALSE)
  
  if(!length(e$x$opts$visualMap))
    e$x$opts$visualMap <- list()
  
  vm <- list(...)
  vm$calculable <- calculable
  vm$type <- type[1]
  
  if(!is.null(serie)){
    dat <- .get_data(e, serie)
    if(!is.null(scale))
      dat <- scale(dat)
    rng <- range(dat)
    vm$min <- rng[1]
    vm$max <- rng[2]
  }
  
  e$x$opts$visualMap <- append(e$x$opts$visualMap, list(vm))
  e
}

#' Tooltip
#' 
#' Customise tooltip
#' 
#' @inheritParams e_bar
#' @param trigger What triggers the tooltip, one of \code{item} or \code{item}.
#' 
#' @examples 
#' USArrests %>% 
#'   e_charts(Assault) %>% 
#'   e_bar(Murder) %>% 
#'   e_tooltip()
#' 
#' @seealso \href{https://ecomfe.github.io/echarts-doc/public/en/option.html#tooltip}{Additional arguments}
#' 
#' @export
e_tooltip <- function(e, trigger = c("item", "axis"), ...){
  
  if(missing(e))
    stop("must pass e", call. = FALSE)
  
  tooltip <- list(trigger = trigger[1], ...)
  
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
#' @seealso \href{https://ecomfe.github.io/echarts-doc/public/en/option.html#legend}{Additional arguments}
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
#' @details Valid \code{feature}:
#' \itemize{
#'   \item{\code{saveAsImage}}
#'   \item{\code{brush}}
#'   \item{\code{restore}}
#'   \item{\code{dataView}}
#'   \item{\code{dataZoom}}
#'   \item{\code{magicType}}
#' }
#' 
#' @examples 
#' USArrests %>% 
#'   e_charts(UrbanPop) %>% 
#'   e_line(Assault) %>% 
#'   e_area(Murder, y_index = 1, x_index = 1) %>% 
#'   e_datazoom(x_index = 0) 
#'   
#' mtcars %>% 
#'   dplyr::mutate(model = row.names(.)) %>%  
#'   e_charts(model) %>% 
#'   e_line(qsec) %>% 
#'   e_toolbox() %>% 
#'   e_toolbox_feature(
#'     feature = "magicType", 
#'     type = list("line", "bar")
#'   ) 
#' 
#' @seealso \href{https://ecomfe.github.io/echarts-doc/public/en/option.html#toolbox}{Additional arguments}
#' 
#' @rdname toolbox
#' @export
e_toolbox_feature <- function(e, feature, ...){
  
  if(missing(e))
    stop("must pass e", call. = FALSE)
  
  if(missing(feature))
    feature <- c("saveAsImage", "restore", "dataView", "dataView", "dataZoom", "magicType", "brush")
  
  if(!length(e$x$opts$toolbox))
    e$x$opts$toolbox <- list(feature = list())
  
  for(i in 1:length(feature)){
    e$x$opts$toolbox$feature[[feature[i]]] <- list()
  }
  
  options <- list(...)
  if(length(options)){
    for(i in 1:length(options)){
      e$x$opts$toolbox$feature[[feature]][[names(options)[i]]] <- options[[i]]
    }
  }
  
  e
}

#' @rdname toolbox
#' @export
e_toolbox <- function(e, ...){
  
  e$x$opts$toolbox <- list(
    ...
  )
  
  e
}

#' Data zoom
#' 
#' Add data zoom.
#' 
#' @inheritParams e_bar
#' @param toolbox Whether to add the toolbox, \code{\link{e_toolbox_feature}}, (\code{e_toolbox_feature(e, "dataZoom")}).
#' 
#' @examples 
#' USArrests %>% 
#'   e_charts(UrbanPop) %>% 
#'   e_line(Assault) %>% 
#'   e_area(Murder, y_index = 1, x_index = 1) %>% 
#'   e_y_axis(gridIndex = 1) %>%
#'   e_x_axis(gridIndex = 1) %>% 
#'   e_grid(height = "35%") %>% 
#'   e_grid(height = "35%", top = "50%") %>% 
#'   e_toolbox_feature("dataZoom", title = list(zoom = "zoom", back = "back")) %>% 
#'   e_datazoom(x_index = c(0, 1))
#' 
#' @seealso \href{https://ecomfe.github.io/echarts-doc/public/en/option.html#dataZoom}{Additional arguments}
#' 
#' @export
e_datazoom <- function(e, x_index = NULL, y_index = NULL, toolbox = TRUE, ...){
  
  if(missing(e))
    stop("must pass e", call. = FALSE)
  
  if(!is.null(x_index) && !is.null(y_index))
    stop("pass x_index or y_index, not both", call. = FALSE)
  
  if(!length(e$x$opts$dataZoom)) # initiatilise if not existing
    e$x$opts$dataZoom <- list()
  
  if(!length(e$x$opts$toolbox$feature$dataZoom) && isTRUE(toolbox))
    e <- e_toolbox_feature(e, "dataZoom")
  
  opts <- list(...)
  if(!is.null(x_index)) opts$xAxisIndex <- x_index
  if(!is.null(y_index)) opts$yAxisIndex <- y_index
  
  e$x$opts$dataZoom <- append(e$x$opts$dataZoom, list(opts))
  
  e
}

#' Brush
#' 
#' Add a brush.
#' 
#' @inheritParams e_bar
#' 
#' @examples 
#' quakes %>% 
#'   e_charts(long) %>% 
#'   e_geo(
#'     boundingCoords = list(
#'       c(190, -10),
#'       c(180, -40)
#'      )
#'   ) %>% 
#'   e_scatter(lat, mag, stations, coord.system = "geo", name = "mag") %>% 
#'   e_data(quakes, depth) %>% 
#'   e_scatter(mag, mag, stations, name = "mag & depth") %>%  
#'   e_grid(right = 40, top = 100, width = "30%") %>% 
#'   e_y_axis(type = "value", name = "depth", min = 3.5) %>% 
#'   e_brush() %>% 
#'   e_theme("dark")
#' 
#' @seealso \href{https://ecomfe.github.io/echarts-doc/public/en/option.html#brush}{Additional arguments}
#' 
#' @export
e_brush <- function(e, x_index = NULL, y_index = NULL, ...){
  
  if(missing(e))
    stop("must pass e", call. = FALSE)
  
  if(!is.null(x_index) && !is.null(y_index))
    stop("pass x_index or y_index, not both", call. = FALSE)
  
  if(!length(e$x$opts$brush)) # initiatilise if not existing
    e$x$opts$brush <- list()
  
  if(!length(e$x$opts$toolbox$feature$brush))
    e <- e_toolbox_feature(e, "brush")
  
  opts <- list(
    brushLink = "all",
    ...
  )
  opts$xAxisIndex <- x_index
  opts$yAxisIndex <- y_index
  
  e$x$opts$brush <- append(e$x$opts$brush, opts)
  
  e
}

#' Title
#' 
#' Add title.
#' 
#' @inheritParams e_bar
#' @param text,subtext Title and Subtitle.
#' @param link,sublink Title and Subtitle link.
#' 
#' @examples 
#' quakes %>% 
#'   dplyr::mutate(mag = exp(mag) / 60) %>% 
#'   e_charts(stations) %>% 
#'   e_scatter(depth, mag) %>% 
#'   e_visual_map(min = 3, max = 7) %>% 
#'   e_title("Quakes", "Stations and Magnitude")
#' 
#' @seealso \href{https://ecomfe.github.io/echarts-doc/public/en/option.html#title}{Additional arguments}
#' 
#' @export
e_title <- function(e, text, subtext = NULL, link = NULL, sublink = NULL, ...){
  
  if(missing(e) || missing(text))
    stop("missing e or text", call. = FALSE)
  
  title <- list(...)
  title$text <- text
  title$subtext <- subtext
  title$link <- link
  title$sublink <- sublink
  
  e$x$opts$title <- title
  
  e
  
}

#' Polar
#' 
#' Customise polar coordinates.
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
#'   e_radius_axis() %>% 
#'   e_line(y, coord.system = "polar", smooth = TRUE) 
#' 
#' @seealso \href{https://ecomfe.github.io/echarts-doc/public/en/option.html#polar}{Additional arguments}
#' 
#' @export
e_polar <- function(e, show = TRUE, ...){
  
  if(missing(e))
    stop("missing e", call. = FALSE)
  
  e$x$opts$yAxis <- NULL
  e$x$opts$xAxis <- NULL
  
  e$x$opts$polar <- list(show = show, ...)
  
  e
}

#' Axis pointer
#' 
#' Customise axis pointer.
#' 
#' @inheritParams e_bar
#' 
#' @seealso \href{https://ecomfe.github.io/echarts-doc/public/en/option.html#axisPointer}{Additional arguments}
#' 
#' @export
e_axis_pointer <- function(e, ...){
  
  if(missing(e))
    stop("missing e", call. = FALSE)
  
  e$x$opts$axisPointer <- list(...)
  
  e
}

#' Animation
#' 
#' Customise animations.
#' 
#' @inheritParams e_bar
#' @param show Set to show animation.
#' @param threshold Whether to set graphic number threshold to animation. 
#' Animation will be disabled when graphic number is larger than threshold.
#' @param duration Duration of the first animation.
#' @param easing Easing method used for the first animation. 
#' @param delay Delay before updating the first animation.
#' @param duration.update Time for animation to complete.
#' @param easing.update Easing method used for animation.
#' @param delay.update Delay before updating animation.
#' 
#' 
#' @examples 
#' mtcars %>% 
#'   e_charts(mpg) %>% 
#'   e_area(drat) %>% 
#'   e_animation(duration = 10000)
#' 
#' @seealso \href{https://ecomfe.github.io/echarts-doc/public/en/option.html#animation}{Additional arguments}
#' 
#' @export
e_animation <- function(e, show = TRUE, threshold = NULL, duration = NULL, easing = NULL, delay = NULL,
                        duration.update = NULL, easing.update = NULL, delay.update = NULL){
  
  e$x$opts$animation <- show
  e$x$opts$animationThreshold <- threshold
  e$x$opts$animationDuration <- duration
  e$x$opts$animationEasing <- easing
  e$x$opts$animationDelay <- delay
  e$x$opts$animationDurationUpdate <- duration.update
  e$x$opts$animationEasingUpdate <- easing.update
  e$x$opts$animationDelayUpdate <- delay.update
  
  e
  
}

#' Use UTC
#' 
#' Use UTC
#' 
#' @inheritParams e_bar
#' 
#' @export
e_utc <- function(e){
  e$useUTC <- TRUE
  e
}

#' Flip coordinates
#' 
#' Flip cartesian 2D coordinates.
#' 
#' @inheritParams e_bar
#' 
#' @examples 
#' df <- data.frame(
#'   x = LETTERS[1:5],
#'   y = runif(5, 1, 5),
#'   z = runif(5, 3, 10)
#' )
#' 
#' df %>%
#'  e_charts(x) %>%
#'  e_bar(y) %>% 
#'  e_line(z) -> plot
#'  
#' plot # normal
#' e_flip_coords(plot) # flip
#' 
#' @export
e_flip_coords <- function(e){
  n <- names(e$x$opts)
  n <- gsub("xAxis", "RENAME", n)
  n <- gsub("yAxis", "xAxis", n)
  n <- gsub("RENAME", "yAxis", n)
  
  names(e$x$opts) <- n
  
  for(i in 1:length(e$x$opts$series)){
    for(j in 1:length(e$x$opts$series[[i]]$data)){
      vals <- e$x$opts$series[[i]]$data[[j]]$value
      e$x$opts$series[[i]]$data[[j]]$value <- rev(vals)
    }
  }
  
  e
}

#' Text style
#' 
#' Define global font style.
#' 
#' @inheritParams e_bar
#' 
#' @note Do not use \code{e_arrange} in R markdown or Shiny.
#' 
#' @seealso \href{https://ecomfe.github.io/echarts-doc/public/en/option.html#textStyle}{official documentation}
#' 
#' @examples 
#' cars %>% 
#'   e_charts(dist) %>% 
#'   e_scatter(speed) %>% 
#'   e_labels() %>% 
#'   e_text_style(
#'     color = "blue",
#'     fontStyle = "italic"
#'   )
#' 
#' @export
e_text_style <- function(e, ...){
  e$x$opts$textStyle <- list(...)
  e
}

#' Connect charts
#' 
#' Connect charts together.
#' 
#' @inheritParams e_bar
#' @param ids Scalar, vector or list of ids of chart to connect with.
#' @param rows,cols Number of rows and columns.
#' @param ... Any \code{echarts} objects.
#' @param group Group name.
#' @param title Title of charts.
#' 
#' @section Functions:
#' \itemize{
#'   \item{\code{e_connect}: connects charts by \code{ids}, \emph{cannot} be disconnected.}
#'   \item{\code{e_group}: assigns a group to chart.}
#'   \item{\code{e_connect_group}: connects chart with another group.}
#'   \item{\code{e_disconnect_group}: diconnects chart from group.}
#'   \item{\code{e_arrange}: arrange charts.}
#' }
#' 
#' @return \code{e_arrange}: in an interactive session, returns a \link[htmltools]{browsable}, in \code{rmarkdown} returns a 
#' container (\link[htmltools]{div}).
#' 
#' @examples
#' # linked datazoom
#' e1 <- cars %>% 
#'   e_charts(
#'     speed,
#'     height = 200
#'   ) %>% 
#'   e_scatter(dist) %>% 
#'   e_datazoom(show = FALSE) %>% 
#'   e_group("grp") # assign group
#'   
#' e2 <- cars %>% 
#'   e_charts(
#'     dist,
#'     height = 200
#'   ) %>% 
#'   e_scatter(speed) %>% 
#'   e_datazoom() %>% 
#'   e_group("grp") %>%  # assign group
#'   e_connect_group("grp") # connect
#' 
#' e_arrange(e1, e2, title = "Linked datazoom")
#' 
#' @note \code{e_arrange} may not work properly in the RStudio viewer.
#' 
#' @name connections
#' @export
e_connect <- function(e, ids){
  if(missing(ids)) stop("missing ids", call. = FALSE)
  e$x$connect <- as.list(ids)
  return(e)
}

#' @rdname connections
#' @export
e_group <- function(e, group){
  
  if(missing(group))
    stop("missing group", call. = FALSE)
  
  e$x$chartGroup <- group
  
  e
  
}

#' @rdname connections
#' @export
e_connect_group <- function(e, group){
  if(missing(group))
    stop("missing group", call. = FALSE)
  
  e$x$groupConnect <- group
  
  e
}

#' @rdname connections
#' @export
e_disconnect_group <- function(e, group = NULL){
  
  if(missing(group))
    stop("missing group", call. = FALSE)
  
  e$x$groupDisconnect <- group
  
  e
  
}

#' @rdname connections
#' @export
e_arrange <- function(..., rows = NULL, cols = NULL, title = NULL){
  
  plots <- list(...)
  
  if(is.null(rows))
    rows <- length(plots)
  
  if(is.null(cols))
    cols <- 1
  
  x <- 0
  tg <- htmltools::tagList()
  for(i in 1:rows){
    r <- htmltools::div(class = "row")
    
    for(j in 1:cols){
      x <- x + 1
      cl <- paste0("col-xs-", 12 / cols)
      if(x <= length(plots))
        c <- htmltools::div(class = cl, plots[[x]])
      else 
        c <- htmltools::div(class = cl)
      r <- htmltools::tagAppendChild(r, c)
    }
    tg <- htmltools::tagAppendChild(tg, r)
  }
  
  if(!isTRUE(getOption('knitr.in.progress'))){
    htmltools::browsable(
      htmltools::div(
        class = "container-fluid",
        htmltools::tags$head(
          htmltools::tags$link(
            rel="stylesheet",
            href="https://stackpath.bootstrapcdn.com/bootstrap/4.1.3/css/bootstrap.min.css",
            integrity="sha384-MCw98/SFnGE8fJT3GXwEOngsV7Zt27NXFoaoApmYm81iuXoPkFOJwJ8ERdknLPMO",
            crossorigin="anonymous"
          )
        ),
        htmltools::h3(title),
        tg
      )
    ) 
  } else {
    if(!is.null(title))
      htmltools::div(title, tg)
    else
      tg
  }
}
