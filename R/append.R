#' Append data
#' 
#' Append data.
#' 
#' @inheritParams e_highlight_p
#' @param series.index Index of serie to append to (starts from 0).
#' @param data Data.frame containing data to append.
#' @param x,y,z Columns names to plot.
#' 
#' @details Currently not all types of series supported incremental rendering when using appendData. 
#' Only these types of series support it: \code{\link{e_scatter}} and \code{\link{e_line}} of pure echarts, and 
#' \code{\link{e_scatter_3d}}, and \code{\link{e_line_3d}} of echarts-gl.
#' 
#' @examples 
#' \dontrun{
#'   library(shiny)
#'   
#'   ui <- fluidPage(
#'     actionButton("add", "Add Data to y"),
#'     echarts4rOutput("plot"),
#'     h4("Brush"),
#'     verbatimTextOutput("selected"),
#'     h4("Legend select change"),
#'     verbatimTextOutput("legend")
#'   )
#'   
#'   server <- function(input, output, session){
#'   
#'     data <- data.frame(x = rnorm(10, 5, 3), y = rnorm(10, 50, 12), z = rnorm(10, 50, 5))
#'     
#'     react <- eventReactive(input$add, {
#'       set.seed(sample(1:1000, 1))
#'       data.frame(x = rnorm(10, 5, 2), y = rnorm(10, 50, 10))
#'     })
#'     
#'     output$plot <- renderEcharts4r({
#'       data %>% 
#'        e_charts(x) %>% 
#'        e_scatter(y) %>%
#'        e_scatter(z) %>% 
#'        e_brush(throttleType = "debounce")
#'     })
#'     
#'     observeEvent(input$add, {
#'       echarts4rProxy("plot") %>% 
#'         e_append1_p(0, react(), x, y)
#'     })
#'     
#'     output$selected <- renderPrint({
#'       input$plot_brush
#'     })
#'     
#'     output$legend <- renderPrint({
#'       input$plot_legend_change
#'     })
#'     
#'   }
#'   
#'   shinyApp(ui, server)
#'   
#' }
#' 
#' @rdname append
#' @export
e_append1_p <- function(proxy, series.index = NULL, data, x, y){
  
  if (!"echarts4rProxy" %in% class(proxy)) 
    stop("must pass echarts4rProxy object", call. = FALSE)
  
  data <- .build_data_p(data, deparse(substitute(x)), deparse(substitute(y)))
  
  opts <- list(id = proxy$id, seriesIndex = series.index, data = data)
  
  proxy$session$sendCustomMessage("e_append_p", opts)
  
  return(proxy)
}

#' @rdname append
#' @export
e_append2_p <- function(proxy, series.index = NULL, data, x, y, z){
  
  if (!"echarts4rProxy" %in% class(proxy)) 
    stop("must pass echarts4rProxy object", call. = FALSE)
  
  data <- .build_data_p(data, deparse(substitute(x)), deparse(substitute(y)), deparse(substitute(z)))
  
  opts <- list(id = proxy$id, seriesIndex = series.index, data = data)
  
  proxy$session$sendCustomMessage("e_append_p", opts)
  
  return(proxy)
}