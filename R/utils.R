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
  
  df <- cbind(lhs, rhs)
  
  matrix <- unname(df)
  
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

.get_outliers <- function(data, serie){
  data %>%
    dplyr::select(
      !!serie
    ) %>%  
    unname() %>% 
    unlist() -> x
  
  boxplot.stats(x)$out
}

.build_outliers <- function(e, out){
  x <- length(e$x$opts$series[[1]]$data) - 1
  x <- rep(x, length(out))
  matrix <- cbind(x, out)
  apply(unname(matrix), 1, as.list)
}

.add_outliers <- function(e, serie){
  
  outliers <- .get_outliers(e$x$data, serie)
  outliers <- .build_outliers(e, outliers)
  
  scatter <- list(
    type = "scatter",
    data = outliers
  )
  
  if(length(e$x$opts$series) == 2)
    e$x$opts$series[[2]]$data <- append(e$x$opts$series[[2]]$data, outliers)
  else 
    e$x$opts$series <- append(e$x$opts$series, list(scatter))
  
  e
}

.build_3d <- function(data, x, y , z){
  data %>%
    dplyr::select(
      !!x,
      !!y,
      !!z
    ) %>%
    unname() -> data
  
  apply(data, 1, as.list)
}
