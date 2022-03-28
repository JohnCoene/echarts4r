#' Facet
#'
#' Create facets for multiple plots.
#'
#' @inheritParams e_bar
#' @param rows,cols Number of rows and columns.
#' @param legend_pos Position of the legend
#' @param legend_space Space between legend and plot area.
#' 
#' @examples
#' group_size <- 20
#' n_groups <- 13
#' df <- data.frame("day" = rep(1:group_size, times=n_groups), 
#'                  "temperature" = runif(group_size * n_groups, 10, 40),
#'                  "location" = rep(LETTERS[1:n_groups], each=group_size))
#' 
#' df |> 
#'   group_by(location) |> 
#'   e_charts(day) |> 
#'   e_line(temperature) |> 
#'   e_facet(rows = 4, cols=4, legend_pos = "top", legend_space = 12) 
#'
#' @details Each serie, i.e.: \code{\link{e_bar}} will be plotted against a facet.
#'
#' @export 
e_facet <- function(
  e, 
  rows = 1, 
  cols = 1, 
  legend_pos = "top", 
  legend_space = 10
) {

  # number of series
  nseries <- length(e$x$opts$series)
  

  # Spacing options ---------------------------------------------------------
  # Extra room for axis labels
  # Possibly interesting as additional parameters, maybe reformat as a single vector.
  lab_space_left <- 5      # Space for axis labels
  lab_space_top <- 2   
  lab_space_bottom <- 5   
  lab_space_right <- 2   
  

  # Calculate spacings ------------------------------------------------------

  # introduce some spacing between panels for low dimensional grids
  v_facet_space = if (rows < 10) 10 - rows else 0       
  h_facet_space = if (cols < 10) 10 - cols else 0       
  # an option to overwrite the space between panels with another parameter for 
  #   for the e_facet function may be interesting.
  
  
  # Maximum space for facets (depends on legend position, and space for axis labels)
  if(legend_pos %in% c("top", "bottom")){
    w_max <- 100 - lab_space_left - lab_space_right
    h_max <- 100 - legend_space - lab_space_bottom - lab_space_top
  }else if(legend_pos %in% c("left", "right")){
    w_max <- 100 - legend_space - lab_space_left - lab_space_right
    h_max <- 100 - lab_space_bottom -lab_space_top
  }else{
    w_max <- 100 - lab_space_left - lab_space_right
    h_max <- 100 - lab_space_bottom - lab_space_top
  }

  # Total space for facets, taking between-panel spacing, legend space, and
  #   extra space for axis labels into account
  rows_h_max <- h_max - (v_facet_space * (rows-1)) 
  cols_w_max <- w_max - (h_facet_space * (cols-1))
  
  # Dimensions of each panel
  height <- rows_h_max / rows
  width <- cols_w_max / cols
  

  # Panel positions ---------------------------------------------------------
  
  # Offset only when legend is left or top
  top_offset <- 0
  left_offset <- 0
  if (legend_pos == "top") {
    top_offset <- legend_space
  }
  if (legend_pos == "left") {
    left_offset <- legend_space
  }
  
  # Generate a vector for positions from the top
  top_pos_values <- sapply(1:rows, function(x){
    lab_space_top + top_offset + ((x-1) * (height + v_facet_space))
  })
  top_positions <- rep(top_pos_values, each=cols)[1:nseries]
  
  # Generate a vector for positions from the left
  left_pos_values <- sapply(1:cols, function(x){
    lab_space_left + left_offset + ((x-1) * (width + h_facet_space))
  })
  left_positions <- rep(left_pos_values, times=rows)[1:nseries]
  

  # Create grid -------------------------------------------------------------
  for (i in 1:nseries) {
    # Create new grid elements
    e <- e |>
      e_grid(
        height = paste0(height, "%"),
        width = paste0(width, "%"),
        top = paste0(top_positions[i], "%"),
        left = paste0(left_positions[i], "%")
      ) 
    
    # Create new axes for the series, that mimic the first axes
    e$x$opts$xAxis[[i]] <- e$x$opts$xAxis[[1]]
    e$x$opts$yAxis[[i]] <- e$x$opts$yAxis[[1]]
    
    # Assign the new axes to the new grid element
    e$x$opts$xAxis[[i]]$gridIndex <- i - 1
    e$x$opts$yAxis[[i]]$gridIndex <- i - 1
    
    # Dictate to which grid panel the series will be assigned
    e$x$opts$series[[i]]$yAxisIndex <- i - 1
    e$x$opts$series[[i]]$xAxisIndex <- i - 1 
    
  }
  
  e
}
