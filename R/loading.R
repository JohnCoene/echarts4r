#' Loading
#'
#' Show or hide loading.
#'
#' @inheritParams e_bar
#' @param hide_overlay Hides the white overaly that appears in shiny when a plot is recalculating.
#' @param text Text to display.
#' @param color Color of spinner.
#' @param text_color Color of text.
#' @param mask_color Color of mask.
#' @param zlevel Z level.
#'
#' @details This only applies to Shiny.
#'
#' @examples
#' \dontrun{
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
#' server <- function(input, output) {
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
#' if (interactive()) {
#'   shinyApp(ui, server)
#' }
#'
#' # add loading
#' server <- function(input, output) {
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
#' if (interactive()) {
#'   shinyApp(ui, server)
#' }
#' }
#'
#' @rdname loading
#' @export
e_show_loading <- function(e, hide_overlay = TRUE, text = "loading", color = "#c23531", text_color = "#000",
                           mask_color = "rgba(255, 255, 255, 0.8)", zlevel = 0) {
  e$x$loading <- TRUE
  e$x$hideWhite <- hide_overlay
  opts <- list(
    text = text,
    color = color,
    textColor = text_color,
    maskColor = mask_color,
    zlevel = zlevel
  )
  e$x$loadingOpts <- opts
  e
}

#' @rdname loading
#' @export
e_hide_loading <- function(e) {
  e$x$loading <- FALSE
  e
}
