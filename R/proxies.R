#' Highlight
#' 
#' Proxies to highlight and downplay series.
#' 
#' @param proxy An echarts4r proxy as returned by \code{\link{echarts4rProxy}}.
#' @param series.index Series index, can be a vector.
#' @param series.name Series Name, can be vector.
#' 
#' @examples 
#' if(interactive()){
#' 
#'  ui <- fluidPage(
#'    fluidRow(
#'      column(
#'        3,
#'        actionButton("highlightmpg", "Highlight MPG")
#'      ),
#'      column(
#'        3,
#'        actionButton("highlighthp", "Highlight HP")
#'      ),
#'      column(
#'        3,
#'        actionButton("downplaympg", "Downplay MPG")
#'      ),
#'      column(
#'        3,
#'        actionButton("downplayhp", "Downplay HP")
#'      )
#'    ),
#'    echarts4rOutput("plot")
#'  ) 
#'  
#'  server <- function(input, output, session){
#'    output$plot <- renderEcharts4r({
#'      mtcars %>% 
#'        e_charts(mpg) %>% 
#'        e_line(disp) %>% 
#'        e_line(hp, name = "HP") # explicitly pass name
#'    })
#'    
#'    # highlight
#'    
#'    observeEvent(input$highlightmpg, {
#'      echarts4rProxy("plot") %>% 
#'        e_highlight_p(series.index = 0) # using index
#'    })
#'    
#'    observeEvent(input$highlighthp, {
#'      echarts4rProxy("plot") %>% 
#'        e_highlight_p(series.name = "HP") # using name
#'    })
#'    
#'    # downplay
#'    
#'    observeEvent(input$downplaympg, {
#'      echarts4rProxy("plot") %>% 
#'        e_downplay_p(series.name = "disp")
#'    })
#'    
#'    observeEvent(input$downplayhp, {
#'      echarts4rProxy("plot") %>% 
#'        e_downplay_p(series.index = 1)
#'    })
#'  }
#'  
#'  shinyApp(ui, server)
#'
#' }
#' 
#' @rdname highlight_proxy
#' @export
e_highlight_p <- function(proxy, series.index = NULL, series.name = NULL){
  
  if (!"echarts4rProxy" %in% class(proxy)) 
    stop("This function must be used with a billboarderProxy object")
  
  data <- list(id = proxy$id)
  if(!is.null(series.index)) data$seriesIndex <- series.index
  if(!is.null(series.name)) data$seriesName <- series.name
  
  proxy$session$sendCustomMessage("e_highlight_p", data)
  
  return(proxy)
}

#' @rdname highlight_proxy
#' @export
e_downplay_p <- function(proxy, series.index = NULL, series.name = NULL){
  
  if (!"echarts4rProxy" %in% class(proxy)) 
    stop("This function must be used with a billboarderProxy object")
  
  data <- list(id = proxy$id)
  if(!is.null(series.index)) data$seriesIndex <- series.index
  if(!is.null(series.name)) data$seriesName <- series.name
  
  proxy$session$sendCustomMessage("e_downplay_p", data)
  
  return(proxy)
}