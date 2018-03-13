# build vector
.build_vector <- function(data, x){
  data %>%
    dplyr::select(!!x) %>%
    unlist() %>%
    unname()
}

.build_xy <- function(data, x, serie, size){
  
  if(!missing(size)){
    data %>%
      dplyr::select(!!serie, !!size) -> rhs
  } else {
    data %>%
      dplyr::select(!!serie) -> rhs
  }
  
  lhs <- data.frame(
    x = x
  )
  
  df <- cbind.data.frame(lhs, rhs)
  
  matrix <- unname(as.matrix(df))
  
  apply(matrix, 1, as.list)
}

.build_candle <- function(data, opening, closing, low, high){
  data %>%
    dplyr::select(
      !!opening, 
      !!closing, 
      !!low, 
      !!high
    ) -> data
  
  matrix <- unname(as.matrix(data))
  
  apply(matrix, 1, as.list)
}

.build_funnel <- function(data, x, y){
  data %>%
    dplyr::select(!!x, !!y) -> df
  
  names(df) <- c("value", "name")
  
  apply(df, 1, as.list)
}

.build_sankey_nodes <- function(data, source, target){
  
  nodes <- c(
    unlist(
      dplyr::select(data, !!source)
    ),
    unlist(
      dplyr::select(data, !!target)
    )
  )
  
  nodes <- data.frame(
    name = unique(nodes),
    stringsAsFactors = FALSE
  )
  
  apply(nodes, 1, as.list)
  
  
}

.build_sankey_edges <- function(data, source, target, values){
  data %>%
    dplyr::select(!!source, !!target, !!values) -> edges
  
  names(edges) <- c("source", "target", "value")
  
  apply(edges, 1, as.list)
}