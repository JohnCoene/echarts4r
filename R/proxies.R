#' Highlight
#' 
#' Proxies to highlight and downplay series.
#' 
#' @inheritParams e_bar
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
    stop("must pass echarts4rProxy object", call. = FALSE)
  
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
    stop("must pass echarts4rProxy object", call. = FALSE)
  
  data <- list(id = proxy$id)
  if(!is.null(series.index)) data$seriesIndex <- series.index
  if(!is.null(series.name)) data$seriesName <- series.name
  
  proxy$session$sendCustomMessage("e_downplay_p", data)
  
  return(proxy)
}

#' Tooltip
#' 
#' Proxies to show or hide tooltip.
#' 
#' @inheritParams e_highlight_p
#' @param series.index Index of serie.
#' @param name Name of serie.
#' @param position Tooltip position.
#' 
#'  @examples 
#'  if(interactive()){
#'    library(shiny)
#'  
#'    ui <- fluidPage(
#'      fluidRow(
#'        actionButton("show", "Show tooltip"),
#'        actionButton("hide", "Hide tooltip")
#'      ),
#'      fluidRow(
#'        echarts4rOutput("plot")
#'      )
#'    )
#'    
#'    server <- function(input, output, session){
#'      output$plot <- renderEcharts4r({
#'        mtcars %>% 
#'          e_charts(mpg) %>% 
#'          e_line(disp) %>% 
#'          e_theme("westeros") %>% 
#'          e_tooltip(show = FALSE)
#'      })
#'      
#'      observeEvent(input$show, {
#'        echarts4rProxy("plot") %>% 
#'          e_showtip_p()
#'      })
#'      
#'      observeEvent(input$hide, {
#'        echarts4rProxy("plot") %>% 
#'          e_hidetip_p()
#'      })
#'    }
#'    
#'    shinyApp(ui, server)
#'  } 
#' 
#' @rdname tooltip_proxy
#' @export
e_showtip_p <- function(proxy, position = c(10, 10), series.index = NULL, name = NULL){
  
  if (!"echarts4rProxy" %in% class(proxy)) 
    stop("must pass echarts4rProxy object", call. = FALSE)
  
  data <- list(id = proxy$id)
  if(!is.null(series.index)) data$seriesIndex <- series.index
  if(!is.null(name)) data$name <- name
  
  proxy$session$sendCustomMessage("e_showtip_p", data)
  
  return(proxy)
}

#' @rdname tooltip_proxy
#' @export
e_hidetip_p <- function(proxy){
  
  if (!"echarts4rProxy" %in% class(proxy)) 
    stop("must pass echarts4rProxy object", call. = FALSE)
  
  data <- list(id = proxy$id)
  
  proxy$session$sendCustomMessage("e_hidetip_p", data)
  
  return(proxy)
}

