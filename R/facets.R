#' Facet
#'
#' Create facets for multiple plots.
#'
#' @inheritParams e_bar
#' @param rows,cols Number of rows and columns. If both are `NULL` the number of rows and columns
#'     will be determined automatically. 
#' @param legend_pos Position of the legend. One of "top", "right", "bottom", "left". Determines 
#'     to which side the `legend_space` argument applies. 
#' @param legend_space Space between legend and plot area. The entered number will be used 
#'     as percentage.
#' @param margin_trbl Adjusts the size of the outside margin around the plotting area. Default is
#'     `c(t = 2, r = 2, b = 5, l = 2)`. Numbers are used as percentage of total plotting area. To
#'     change only e.g. two sides `c("r" = 8, "l" = 8)` could be used, other sides will use 
#'     defaults. 
#' @param h_panel_space,v_panel_space Horizontal and vertical spacing between the individual grid
#'     elements. Expects numeric input, which will be used as percentage of total plotting area.
#'     Default `NULL` will automatically add some panel spacing for low dimensional grids. 
#' 
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
#' 
#' @importFrom grDevices n2mfrow
#' 
#' 
#' @export 
#' @importFrom  grDevices n2mfrow

e_facet <- function(e, 
                    rows = NULL, 
                    cols = NULL, 
                    legend_pos = "top", 
                    legend_space = 10,
                    margin_trbl = c(t = 2, r = 2, b = 5, l = 2),
                    h_panel_space = NULL,
                    v_panel_space = NULL) {
    
    
    # Generate default values ---------------------------------------------------------------------
    
    # number of series
    nseries <- length(e$x$opts$series)

    if (is.null(rows) && is.null(cols)) {
        # Automatically generate grid dimensions
        dims <- n2mfrow(nseries)
        rows <- dims[2]
        cols <- dims[1]
    } else if (is.null(rows) || is.null(cols)) {
        stop("Must provide both the number of rows and columns.")
    } else {
        # Do series match the grid?
        if (nseries > rows * cols) {
            warning(paste0(nseries, " series exceed the number of panels created by ",
                           rows, " rows and ", cols, " columns."))
        }
    }
    
    # Specified margins
    spec_margin <- c("t", "r", "b", "l") %in% names(margin_trbl) # see which elements are present
    margin_trbl <- c(margin_trbl, # add missing elements
                     c(t = 2, r = 2, b = 5, l = 5)[!spec_margin])
    
    # Calculate spacings ------------------------------------------------------
    
    # introduce some spacing between panels for low dimensional grids
    if (is.null(v_panel_space)) {
        v_panel_space = if (rows < 10) 10 - rows else 0       
    } else {
        v_panel_space
    }
    
    if (is.null(h_panel_space)) {
        h_panel_space = if (cols < 10) 10 - cols else 0       
    } else {
        h_panel_space
    }
    
    
    # Maximum space for facets (depends on legend position, and space for axis labels)
    if (legend_pos %in% c("top", "bottom")) {
        w_max <- 100 - margin_trbl[["l"]] - margin_trbl[["r"]]
        h_max <- 100 - legend_space - margin_trbl[["b"]] - margin_trbl[["t"]]
    } else if (legend_pos %in% c("left", "right")){
        w_max <- 100 - legend_space - margin_trbl[["l"]] - margin_trbl[["r"]]
        h_max <- 100 - margin_trbl[["b"]] -margin_trbl[["t"]]
    } else {
        w_max <- 100 - margin_trbl[["l"]] - margin_trbl[["r"]]
        h_max <- 100 - margin_trbl[["b"]] - margin_trbl[["t"]]
    }
    
    # Total space for panels, taking between-panel spacing, legend space, and
    #   extra space for axis labels into account
    rows_h_max <- h_max - (v_panel_space * (rows-1)) 
    cols_w_max <- w_max - (h_panel_space * (cols-1))
    
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
        margin_trbl[["t"]] + top_offset + ((x-1) * (height + v_panel_space))
    })
    top_positions <- rep(top_pos_values, each=cols)[1:nseries]
    
    # Generate a vector for positions from the left
    left_pos_values <- sapply(1:cols, function(x){
        margin_trbl[["l"]] + left_offset + ((x-1) * (width + h_panel_space))
    })
    left_positions <- rep(left_pos_values, times=rows)[1:nseries]
    
    
    # Create grid -------------------------------------------------------------
    
    # Act differently depending on chart types
    chart_type <- e$x$opts$series[[1]]$type
    
    # No chart type found
    if (is.null(chart_type)) stop("No chart type found for first series")
    
    # When it is a pie chart
    if (chart_type == "pie") {
        for (i in 1:nseries) {
            
            e$x$opts$series[[i]]$height <- paste0(height, "%")
            e$x$opts$series[[i]]$width <- paste0(width, "%")
            e$x$opts$series[[i]]$top <- paste0(top_positions[i], "%")
            e$x$opts$series[[i]]$left <- paste0(left_positions[i], "%")
            e$x$opts$series[[i]]$avoidLabelOverlap <- F # I noticed that many labels stay hidden when this is on the default: TRUE
            
        }
    } 
    
    # For any other chart type, might not always produce desired results
    else {
        
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
    }
    
    e
}
