# group2series
map_grps_ <- function(data, timeline = FALSE){
  
  row.names(data) <- NULL
  
  if(dplyr::is.grouped_df(data)){
    
    # deparse groups to get grp column
    g.col <- dplyr::groups(data)
    g.col <- unlist(lapply(g.col, deparse))
    
    data <- dplyr::ungroup(data)
    data <- as.data.frame(data)
    row.names(data) <- NULL
    
    grps <- unique(data[,g.col]) # get unique grps
    
    # fun to filter grps
    filter_grp <- function(grps){
      data[data[, g.col] == grps,]
    }
    
    data <- Map(filter_grp, grps)
    names(data) <- grps
    
    # order groups for timeline consistency using timeline
    if(isTRUE(timeline))
      data <- data[order(names(data))] 
    
  } else {
    
    if(inherits(data, "data.frame"))
      data <- as.data.frame(data) # force data frame
    
    data <- list(data)
  }
  
  .no_nm <- function(x) {
    row.names(x) <- NULL
    return(x)
  }
  
  data <- lapply(data, .no_nm)
  
  return(data)
  
}