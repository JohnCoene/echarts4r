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
#'   sd <- crosstalk::SharedData$new(dplyr::select(mtcars, qsec, mpg))
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
#' 
#' library(crosstalk)
#' 
#' sd <- SharedData$new(quakes[sample(nrow(quakes), 100),])
#' filter_slider("mag", "Magnitude", sd, column=~mag, step=0.1, width=250)
#' 
#' bscols(
#' DT::datatable(sd),
#' sd %>% 
#'   e_charts(long) %>% 
#'   e_geo(
#'     roam = TRUE,
#'     boundingCoords = list(
#'       c(188, - 10),
#'       c(165, -38)
#'     )
#'   ) %>% 
#'   e_effect_scatter(lat, mag, coord.system = "geo") %>% 
#'   e_visual_map(min = 4, max = 6.5) %>% 
#'   e_brush()
#' )
#' }
#' 
#' @noRd
#' @keywords internal
NULL