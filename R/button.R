.valid_events <- function(){
  c(
    "highlight",
    "downplay",
    "showtip",
    "hidetip",
    "zoom",
    "visual_map_range",
    "pie_select",
    "pie_unselect"
  )
}

#' Button
#' 
#' Add a button to your visualisation.
#' 
#' @inheritParams e_bar
#' @param event Name of event to trigger.
#' @param position Position of button, \code{top} or \code{bottom}.
#' @param tag A Valid \link[htmltools]{tags} function.
#' @param id A valid CSS id.
#' @param ... Content of the button, complient with \code{htmltools}.
#' 
#' @section Events:
#' \itemize{
#'   \item{\code{highlight}}
#'   \item{\code{downplay}}
#'   \item{\code{showtip}}
#'   \item{\code{hidetip}}
#'   \item{\code{zoom}}
#'   \item{\code{visual_map_range}}
#'   \item{\code{pie_select}}
#'   \item{\code{pie_unselect}}
#' }
#' 
#' @examples 
#' iris %>% 
#'   group_by(Species) %>% 
#'   e_charts(Sepal.Length) %>% 
#'   e_line(Sepal.Width) %>% 
#'   e_line(Petal.Length) %>% 
#'   e_highlight(series.name = "setosa", btn = "myBtn") %>% 
#'   e_button("highlight", "myBtn", "highlight stuff")
#' 
#' @export
e_button <- function(e, event, id, ..., position = "top", tag = htmltools::tags$button){
  
  if(missing(event) || missing(id))
    stop("missing event id", call. = FALSE)
  
  if(!event %in% .valid_events())
    stop("invalid event, see details", call. = FALSE)
  
  if(position == "top")
    htmlwidgets::prependContent(e, tag(id = id, ...))
  else
    htmlwidgets::appendContent(e, tag(id = id, ...))
}