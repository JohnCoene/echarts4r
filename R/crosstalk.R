#' Crowsstalk
#' 
#' @examples 
#' \dontrun{
#' library(shiny)
#' library(DT)
#' 
#' ui <- fluidPage(
#'   actionButton("refresh", "refresh"),
#'   echarts4rOutput("plot")
#' )
#' 
#' server <- function(input, output){
#' 
#'   react <- eventReactive(input$refresh, {
#'     df <- data.frame(
#'       x = rnorm(20),
#'       y = rnorm(20)
#'     )
#'   })
#'   
#'   output$plot <- renderEcharts4r({
#'     react() %>% 
#'       e_charts(x) %>% 
#'       e_line(y) 
#'   })
#' }
#' 
#' shinyApp(ui, server)
#' }
#' 
#' @noRd
#' @keywords internal
NULL