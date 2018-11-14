#' Highlight & Downplay Proxy
#' 
#' Proxies to highlight and downplay series.
#' 
#' @inheritParams e_bar
#' @param proxy An echarts4r proxy as returned by \code{\link{echarts4rProxy}}.
#' @param series_index Series index, can be a vector.
#' @param series_name Series Name, can be vector.
#' 
#' @examples 
#' \dontrun{
#'   library(shiny)
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
#'        e_highlight_p(series_index = 0) # using index
#'    })
#'    
#'    observeEvent(input$highlighthp, {
#'      echarts4rProxy("plot") %>% 
#'        e_highlight_p(series_name = "HP") # using name
#'    })
#'    
#'    # downplay
#'    
#'    observeEvent(input$downplaympg, {
#'      echarts4rProxy("plot") %>% 
#'        e_downplay_p(series_name = "disp")
#'    })
#'    
#'    observeEvent(input$downplayhp, {
#'      echarts4rProxy("plot") %>% 
#'        e_downplay_p(series_index = 1)
#'    })
#'  }
#'  
#'  shinyApp(ui, server)
#'
#' }
#' 
#' @rdname highlight_proxy
#' @export
e_highlight_p <- function(proxy, series_index = NULL, series_name = NULL){
  
  if (!"echarts4rProxy" %in% class(proxy)) 
    stop("must pass echarts4rProxy object", call. = FALSE)
  
  data <- list(id = proxy$id)
  if(!is.null(series_index)) data$seriesIndex <- series_index
  if(!is.null(series_name)) data$seriesName <- series_name
  
  proxy$session$sendCustomMessage("e_highlight_p", data)
  
  return(proxy)
}

#' @rdname highlight_proxy
#' @export
e_downplay_p <- function(proxy, series_index = NULL, series_name = NULL){
  
  if (!"echarts4rProxy" %in% class(proxy)) 
    stop("must pass echarts4rProxy object", call. = FALSE)
  
  data <- list(id = proxy$id)
  if(!is.null(series_index)) data$seriesIndex <- series_index
  if(!is.null(series_name)) data$seriesName <- series_name
  
  proxy$session$sendCustomMessage("e_downplay_p", data)
  
  return(proxy)
}

#' Tooltip Proxy
#' 
#' Proxies to show or hide tooltip.
#' 
#' @inheritParams e_highlight_p
#' @param ... Any other option, see \href{https://ecomfe.github.io/echarts-doc/public/en/api.html#action.tooltip}{showTip}.
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
#'          e_tooltip(show = FALSE) %>%  
#'          e_theme("westeros") 
#'      })
#'      
#'      observeEvent(input$show, {
#'        echarts4rProxy("plot") %>% 
#'          e_showtip_p(
#'            name = "displacement",
#'            position = list(5, 5)
#'          )
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
e_showtip_p <- function(proxy, ...){
  
  if (!"echarts4rProxy" %in% class(proxy)) 
    stop("must pass echarts4rProxy object", call. = FALSE)
  
  data <- list(
    id = proxy$id,
    opts = list(
      type = "showTip",
      ...
    )
  )
  
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
#' @inheritParams e_highlight_p
#' @param index Index of node to focus on.
#' @param ... Any other options, see 
#' \href{https://ecomfe.github.io/echarts-doc/public/en/api.html#action.graph}{official documentation} and details.
#' 
#' @details Must pass \code{seriesId}, \code{seriesIndex}, or \code{seriesName}, generally \code{seriesIndex = 0} will work.
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
#' \dontrun{
#' 
#'   library(shiny)
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
e_focus_adjacency_p <- function(proxy, index, ...){
  
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
e_unfocus_adjacency_p <- function(proxy, ...){
  
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

#' Dispatch Action
#' 
#' Create your own proxies, essentially a wrapper around the 
#' \href{https://ecomfe.github.io/echarts-doc/public/en/api.html#action}{action API}.
#' 
#' @inheritParams e_highlight_p
#' @param type Type of action to dispatch, i.e.: \code{highlight}.
#' @param ... Named options.
#' 
#' @export
e_dispatch_action_p <- function(proxy, type, ...){
  
  if(missing(proxy) || missing(type))
    stop("must pass proxy and type", call. = FALSE)
  
  if (!"echarts4rProxy" %in% class(proxy)) 
    stop("must pass echarts4rProxy object", call. = FALSE)
  
  data <- list(id = proxy$id, opts = list(type = type, ...))
  
  proxy$session$sendCustomMessage("e_dispatch_action_p", data)
}
