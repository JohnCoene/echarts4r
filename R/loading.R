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
#' @details This only applies to Shiny.
#' 
#' @examples 
#' if(interactive()){
#' 
#' # no redraw
#' # no loading
#' library(shiny)
#' ui <- fluidPage(
#'   fluidRow(
#'     column(12, actionButton("update", "Update"))
#'   ),
#'   fluidRow(
#'     column(12, echarts4rOutput("plot"))
#'   )
#' )
#' 
#' server <- function(input, output){
#'   data <- eventReactive(input$update, {
#'     data.frame(
#'       x = 1:10,
#'       y = rnorm(10)
#'     )
#'   })
#'   
#'   output$plot <- renderEcharts4r({
#'     data() %>% 
#'       e_charts(x) %>% 
#'       e_bar(y)
#'   })
#' }
#' 
#' shinyApp(ui, server)
#' 
#' # add loading
#' server <- function(input, output){
#'   data <- eventReactive(input$update, {
#'     Sys.sleep(1) # sleep one second to show loading
#'     data.frame(
#'       x = 1:10,
#'       y = rnorm(10)
#'     )
#'   })
#'   
#'   output$plot <- renderEcharts4r({
#'     data() %>% 
#'       e_charts(x) %>% 
#'       e_bar(y) %>% 
#'       e_show_loading()
#'   })
#' }
#' 
#' shinyApp(ui, server)
#' }
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