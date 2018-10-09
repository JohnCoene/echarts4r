#' Highlight Proxy
#' 
#' Proxies to highlight and downplay series.
#' 
#' @inheritParams e_bar
#' @param proxy An echarts4r proxy as returned by \code{\link{echarts4rProxy}}.
#' @param series.index Series index, can be a vector.
#' @param series.name Series Name, can be vector.
#' 
#' @examples 
#' \dontrun{
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

#' Tooltip Proxy
#' 
#' Proxies to show or hide tooltip.
#' 
#' @inheritParams e_highlight_p
#' @param series.index Index of serie.
#' @param name Name of serie.
#' @param position Tooltip position.
#' 
#' @examples 
#' \dontrun{
#'    library(shiny)
#'  
#'    ui <- fluidPage(
#'      fluidRow(
#'        actionButton("show", "Show tooltip"),
#'        actionButton("hide", "Hide tooltip")
#'      ),
#'      fluidRow(
#'        echarts4rOutput("plot"),
#'        h3("clicked Data"),
#'        verbatimTextOutput("clickedData"),
#'        h3("clicked Serie"),
#'        verbatimTextOutput("clickedSerie"),
#'        h3("clicked Row"),
#'        verbatimTextOutput("clickedRow")
#'      )
#'    )
#'    
#'    server <- function(input, output, session){
#'      output$plot <- renderEcharts4r({
#'        mtcars %>% 
#'          e_charts(mpg) %>% 
#'          e_line(disp, bind = carb, name = "displacement") %>% 
#'          e_line(hp) %>% 
#'          e_x_axis(min = 10) %>% 
#'          e_theme("westeros") 
#'      })
#'      
#'      observeEvent(input$show, {
#'        echarts4rProxy("plot") %>% 
#'          e_showtip_p(0)
#'      })
#'      
#'      observeEvent(input$hide, {
#'        echarts4rProxy("plot") %>% 
#'          e_hidetip_p()
#'      })
#'      
#'      output$clickedData <- renderPrint({
#'        input$plot_clicked_data
#'      })
#'      
#'      output$clickedSerie <- renderPrint({
#'        input$plot_clicked_serie
#'      })
#'      
#'      output$clickedRow <- renderPrint({
#'        input$plot_clicked_row
#'      })
#'      
#'    }
#'    
#'    shinyApp(ui, server)
#' } 
#' 
#' @rdname tooltip_proxy
#' @export
e_showtip_p <- function(proxy, position = c(10, 10), series.index = NULL, name = NULL){
  
  if (!"echarts4rProxy" %in% class(proxy)) 
    stop("must pass echarts4rProxy object", call. = FALSE)
  
  data <- list(id = proxy$id)
  data$seriesIndex <- series.index
  data$name <- name
  data$position <- position
  
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

#' Node Adjacency
#' 
#' Focus or unfocus on node adjacency.
#' 
#' @examples 
#' value <- rnorm(10, 10, 2)
#' 
#' nodes <- data.frame(
#'   name = sample(LETTERS, 10),
#'   value = value,
#'   size = value,
#'   grp = rep(c("grp1", "grp2"), 5),
#'   stringsAsFactors = FALSE
#' )
#' 
#' edges <- data.frame(
#'   source = sample(nodes$name, 20, replace = TRUE),
#'   target = sample(nodes$name, 20, replace = TRUE),
#'   stringsAsFactors = FALSE
#' )
#' 
#' if(interactive()){
#' 
#'   ui <- fluidPage(
#'     fluidRow(
#'       column(
#'         2, numericInput("index", "Node", value = 3, min = 1, max = 9)
#'       ),
#'       column(
#'         2, br(), actionButton("focus", "Focus")
#'       ),
#'       column(
#'         2, br(), actionButton("unfocus", "Unfocus")
#'       )
#'     ),
#'     fluidRow(
#'       column(12, echarts4rOutput("graph"))
#'     )
#'   )
#'   
#'   server <- function(input, output, session){
#'   
#'     output$graph <- renderEcharts4r({
#'       e_charts() %>% 
#'         e_graph() %>% 
#'         e_graph_nodes(nodes, name, value, size, grp) %>% 
#'         e_graph_edges(edges, source, target)
#'     })
#'     
#'     observeEvent(input$focus, {
#'     
#'       echarts4rProxy("graph") %>% 
#'         e_focus_adjacency(
#'           seriesIndex = 0,
#'           index = input$index
#'         )
#'     
#'     })
#'     
#'     observeEvent(input$unfocus, {
#'       
#'       echarts4rProxy("graph") %>% 
#'         e_unfocus_adjacency(seriesIndex = 0)
#'       
#'     })
#'   
#'   }
#'   
#'   shinyApp(ui, server)
#' 
#' }
#' 
#' @rdname node_adjacency
#' @export
e_focus_adjacency <- function(proxy, index, ...){
  
  if (!"echarts4rProxy" %in% class(proxy)) 
    stop("must pass echarts4rProxy object", call. = FALSE)
  
  args <- list(...)
  
  hasArgs <- c("seriesId", "seriesIndex", "seriesName") %in% names(args)
  
  if(sum(hasArgs) != 1)
    stop("Must pass one of seriesId, seriesIndex, or seriesName", call. = FALSE)
  
  data <- list(
    id = proxy$id,
    opts = list(
      type = "focusNodeAdjacency",
      dataIndex = index,
      ...
    )
  )
  
  proxy$session$sendCustomMessage("e_focus_node_adjacency_p", data)
  
  return(proxy)
}

#' @rdname node_adjacency
#' @export
e_unfocus_adjacency <- function(proxy, ...){
  
  if (!"echarts4rProxy" %in% class(proxy)) 
    stop("must pass echarts4rProxy object", call. = FALSE)
  
  args <- list(...)
  
  hasArgs <- c("seriesId", "seriesIndex", "seriesName") %in% names(args)
  
  if(sum(hasArgs) != 1)
    stop("Must pass one of seriesId, seriesIndex, or seriesName", call. = FALSE)
  
  data <- list(
    id = proxy$id,
    opts = list(
      type = "unfocusNodeAdjacency",
      ...
    )
  )
  
  proxy$session$sendCustomMessage("e_unfocus_node_adjacency_p", data)
  
  return(proxy)
}