#' Append Proxy
#'
#' Append data dynamically.
#'
#' @inheritParams e_highlight_p
#' @param series_index Index of serie to append to (starts from 0).
#' @param data Data.frame containing data to append.
#' @param x,y,z Columns names to plot.
#' @param scale A scaling function as passed to \code{\link{e_scatter}}.
#' @param symbol_size Multiplier of scaling function as in \code{\link{e_scatter}}.
#'
#' @details Currently not all types of series supported incremental rendering when using appendData.
#' Only these types of series support it: \code{\link{e_scatter}} and \code{\link{e_line}} of pure echarts, and
#' \code{\link{e_scatter_3d}}, and \code{\link{e_line_3d}} of echarts-gl.
#'
#' @examples
#' \dontrun{
#' library(shiny)
#'
#' ui <- fluidPage(
#'   actionButton("add", "Add Data to y"),
#'   echarts4rOutput("plot"),
#'   h4("Brush"),
#'   verbatimTextOutput("selected"),
#'   h4("Legend select change"),
#'   verbatimTextOutput("legend")
#' )
#'
#' server <- function(input, output, session) {
#'   data <- data.frame(x = rnorm(10, 5, 3), y = rnorm(10, 50, 12), z = rnorm(10, 5, 20))
#'
#'   react <- eventReactive(input$add, {
#'     set.seed(sample(1:1000, 1))
#'     data.frame(x = rnorm(10, 5, 2), y = rnorm(10, 50, 10), z = rnorm(10, 5, 20))
#'   })
#'
#'   output$plot <- renderEcharts4r({
#'     data %>%
#'       e_charts(x) %>%
#'       e_scatter(y, z, scale = NULL) %>%
#'       e_scatter(z) %>%
#'       e_brush()
#'   })
#'
#'   observeEvent(input$add, {
#'     echarts4rProxy("plot") %>%
#'       e_append2_p(0, react(), x, y, z)
#'   })
#'
#'   output$selected <- renderPrint({
#'     input$plot_brush
#'   })
#'
#'   output$legend <- renderPrint({
#'     input$plot_legend_change
#'   })
#' }
#'
#' shinyApp(ui, server)
#' }
#'
#' @rdname append
#' @export
e_append1_p <- function(proxy, series_index = NULL, data, x, y) {
  if (!"echarts4rProxy" %in% class(proxy)) {
    stop("must pass echarts4rProxy object", call. = FALSE)
  }

  e_append1_p_(proxy, series_index, data, deparse(substitute(x)), deparse(substitute(y)))
}

#' @rdname append
#' @export
e_append1_p_ <- function(proxy, series_index = NULL, data, x, y) {
  if (!"echarts4rProxy" %in% class(proxy)) {
    stop("must pass echarts4rProxy object", call. = FALSE)
  }

  dlist <- data %>%
    dplyr::select(x, y) %>%
    unname() %>%
    apply(1, as.list)

  opts <- list(id = proxy$id, seriesIndex = series_index, data = dlist)

  proxy$session$sendCustomMessage("e_append_p", opts)

  return(proxy)
}

#' @rdname append
#' @export
e_append2_p <- function(proxy, series_index = NULL, data, x, y, z, scale = NULL, symbol_size = 1) {
  if (!"echarts4rProxy" %in% class(proxy)) {
    stop("must pass echarts4rProxy object", call. = FALSE)
  }

  e_append2_p_(
    proxy = proxy,
    series_index = series_index,
    data = data,
    x = deparse(substitute(x)),
    y = deparse(substitute(y)),
    z = deparse(substitute(z)),
    scale = scale,
    symbol_size = symbol_size
  )
}

#' @rdname append
#' @export
e_append2_p_ <- function(proxy, series_index = NULL, data, x, y, z, scale = NULL, symbol_size = 1) {
  if (!"echarts4rProxy" %in% class(proxy)) {
    stop("must pass echarts4rProxy object", call. = FALSE)
  }

  data %>%
    dplyr::select(x, y, z) -> data

  if (!is.null(scale)) {
    data[[4]] <- scale(data[[3]]) * symbol_size
  } else {
    data[[4]] <- data[[3]]
  }

  data <- unname(data)

  dlist <- apply(data, 1, function(x) {
    list(value = unlist(x, use.names = FALSE))
  })

  opts <- list(id = proxy$id, seriesIndex = series_index, data = dlist)

  proxy$session$sendCustomMessage("e_append_p", opts)

  return(proxy)
}

#' Remove Serie
#'
#' Remove a serie by name or precising its index.
#'
#' @inheritParams e_highlight_p
#' @param serie_index Index of serie to append to (starts from 0).
#' @param serie_name Name of serie to remove.
#'
#' @examples
#' library(shiny)
#'
#' ui <- fluidPage(
#'   actionButton("rm", "Remove z serie"),
#'   echarts4rOutput("plot")
#' )
#'
#' server <- function(input, output, session) {
#'   data <- data.frame(
#'     x = rnorm(10, 5, 3),
#'     y = rnorm(10, 50, 12),
#'     z = rnorm(10, 50, 5)
#'   )
#'
#'   output$plot <- renderEcharts4r({
#'     data %>%
#'       e_charts(x) %>%
#'       e_scatter(y) %>%
#'       e_scatter(z)
#'   })
#'
#'   observeEvent(input$rm, {
#'     echarts4rProxy("plot") %>%
#'       e_remove_serie_p(serie_name = "z")
#'   })
#' }
#' \dontrun{
#' shinyApp(ui, server)
#' }
#'
#' @name e_remove
#' @export
e_remove_serie_p <- function(proxy, serie_name = NULL, serie_index = NULL) {
  if (is.null(serie_index) && is.null(serie_name)) {
    stop("Must define `serie_index` or `serie_name`")
  }

  opts <- list(id = proxy$id, serie_index = serie_index, serie_name = serie_name)

  proxy$session$sendCustomMessage("e_remove_serie_p", opts)

  return(proxy)
}

#' @rdname e_remove
#' @export
e_remove_serie <- e_remove_serie_p

#' Execute
#'
#' Executes a \code{\link{echarts4rProxy}} command
#'
#' @inheritParams e_highlight_p
#' @param cmd Name of command, default is \emph{e_send_p}.
#'
#' @details Currently three commands are supported. They are related to parameter notMerge of \href{https://echarts.apache.org/en/api.html#echartsInstance.setOption}{setOption}.\cr
#'   \emph{e_send_p} - to send new series to a chart (notMerge=true)\cr
#'   \emph{e_merge_p} - to add marks(\code{\link{e_mark_p}}) to a serie (notMerge=false)\cr
#'   \emph{e_replace_p} - to replace all options (notMerge=true)\cr
#'
#' @examples
#' library(shiny)
#'
#' ui <- fluidPage(
#'   actionButton("pxy", "Proxy"),
#'   echarts4rOutput("plot")
#' )
#' server <- function(input, output) {
#'   data(EuStockMarkets)
#'   df <- as.data.frame(EuStockMarkets) %>%
#'     dplyr::slice_head(n=50) %>% dplyr::mutate(day=1:dplyr::n())
#'   hsp <- NULL  # store plot as global
#'   
#'   output$plot <- renderEcharts4r({
#'     hsp <<- df %>% e_charts(day) %>% 
#'       e_y_axis(scale=TRUE) %>%
#'       e_line(CAC)
#'     hsp %>% e_datazoom(start=50)
#'   })
#'   
#'   observeEvent(input$pxy, {
#'     chart <- hsp %>%
#'       e_grid(height = "33%", top = "50%") %>%
#'       e_grid(height = "32%") %>% 
#'       e_bar(SMI, x_index=1, y_index=1, color="green") %>%
#'       e_y_axis(gridIndex = 1, scale=TRUE) %>% 
#'       e_x_axis(gridIndex = 1) %>%
#'       e_tooltip(trigger='axis') %>% 
#'       e_axis_pointer(link=list(xAxisIndex='all')) %>%
#'       e_datazoom(start=50, xAxisIndex=c(0,1))
#'     # fine tuning
#'     chart$x$opts$yAxis[[1]]$show <- NULL
#'     chart$x$opts$xAxis[[1]]$show <- FALSE
#'     chart$x$opts$yAxis[[2]]$scale <- TRUE
#'     
#'     proxy <- list(id = 'plot', session = shiny::getDefaultReactiveDomain(), 
#'                   chart = chart)
#'     class(proxy) <- "echarts4rProxy"
#'     proxy %>% e_execute('e_replace_p')
#'  })
#' }
#' if (interactive())
#'   shinyApp(ui, server)
#' 
#' @name e_execute
#' @export
e_execute <- function(proxy, cmd='e_send_p') {
  if (missing(proxy))
    stop("missing proxy", call. = FALSE)
  if (!"echarts4rProxy" %in% class(proxy)) 
    stop("must pass echarts4rProxy object", call. = FALSE)
  
  plist <- list(id = proxy$id, opts = proxy$chart$x$opts)
  
  # create web dependencies for JS, if present
  if (!is.null(proxy$chart$dependencies)) {
    deps <- list(shiny::createWebDependency(
      htmltools::resolveDependencies( proxy$chart$dependencies )[[1]]
    ))
    plist$deps <- deps
  }
  
  proxy$session$sendCustomMessage(cmd, plist )
  return(proxy)
}

#' @rdname e_execute
#' @export
e_execute_p <- e_execute

