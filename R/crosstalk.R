#' Crowsstalk
#' 
#' @examples 
#' \dontrun{
#' library(shiny)
#' library(DT)
#' 
#' ui <- fluidPage(
#'   fluidRow(
#'     column(
#'       6,
#'       DT::dataTableOutput("table")
#'     ),
#'     column(
#'       6,
#'       echarts4rOutput("plot")
#'     )
#'   )
#' )
#' 
#' server <- function(input, output){
#'   sd <- SharedData$new(dplyr::select(mtcars, qsec, mpg))
#'   
#'   output$table <- DT::renderDataTable({
#'     DT::datatable(sd)
#'   }, server = FALSE)
#'   
#'   output$plot <- renderEcharts4r({
#'     sd %>% 
#'       e_charts(qsec) %>% 
#'       e_scatter(mpg) %>% 
#'       e_brush()
#'   })
#' }
#' 
#' shinyApp(ui, server)
#' }
#' 
#' @noRd
#' @keywords internal
NULL