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

.build_graph_nodes <- function(nodes, names, value, symbolSize, category){
  
  nodes %>%
    dplyr::select(
      !!names,
      !!value,
      !!symbolSize,
      !!category
    ) -> data
  
  names(data) <- c("name", "value", "symbolSize", "category")[1:ncol(data)]
  
  if(!is.null(data[["category"]]))
    data[["category"]] <- as.numeric(as.factor(data[["category"]])) - 1
  
  apply(data, 1, as.list)
}

.build_graph_edges <- function(edges, source, target){
  
  edges %>%
    dplyr::select(
      !!source,
      !!target
    ) -> data
  
  names(data) <- c("source", "target")
  
  apply(data, 1, as.list)
}

.build_graph_category <- function(nodes, cat){
  nodes %>%
    dplyr::select(
      !!cat
    ) -> data
  
  names(data) <- c("name")
  
  data[["name"]] <- as.numeric(as.factor(data[["name"]])) - 1
  
  apply(data, 1, as.list)
}

.graph_cat_legend <- function(nodes, cat){
  nodes %>%
    dplyr::select(
      !!cat
    ) %>% 
    unlist() %>% 
    unname() %>% 
    unique()
}

.build_boxplot <- function(data, serie){
  data %>%
    dplyr::select(
      !!serie
    ) %>%  
    unname() %>% 
    unlist() -> x
  
  boxplot.stats(x)$stats
}

.build_outliers <- function(data, serie){
  data %>%
    dplyr::select(
      !!serie
    ) %>%  
    unname() %>% 
    unlist() -> x
  
  boxplot.stats(x)$out
}