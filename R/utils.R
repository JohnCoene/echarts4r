.assign_axis <- function(x){
  x$mapping$include_x <- FALSE
  if(x$mapping$x_class == "character" || x$mapping$x_class == "factor"){
    x$opts$xAxis <- list(data = x$mapping$data[[x$mapping$x]], type = "category")
  } else if(x$mapping$x_class == "POSIXct" || x$mapping$x_class == "POSIXlt" || x$mapping$x_class == "Date") {
    x$opts$xAxis <- list(data = x$mapping$data[[x$mapping$x]], type = "time")
  } else {
    x$mapping$data <- x$mapping$data %>% 
      dplyr::arrange_(x$mapping$x)
    x$mapping$include_x <- TRUE
    x$opts$xAxis <- list(type = "value")
  }
  x
}

# redirect
.redirect_vect_xy <- function(e, serie){
  if(isTRUE(e$x$mapping$include_x)){
    .build_xy(e, serie)
  } else {
    .build_vect(e, serie)
  }
}

.build_xy <- function(e, serie){
  e$x$mapping$data %>% 
    dplyr::select_(e$x$mapping$x, serie) %>% 
    unname() %>% 
    apply(., 1, as.list)
}

.build_vect <- function(e, serie){
  e$x$mapping$data %>% 
    dplyr::select_(serie) %>% 
    .[[1]]
}

.build_xyz <- function(e, y, z){
  e$x$mapping$data %>% 
    dplyr::select_(e$x$mapping$x, y, z) %>%
    unname() %>% 
    apply(., 1, as.list)
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

.graph_cat_legend <- function(nodes){
  e$x$mapping$data[[e$x$mapping$x]]
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
  
  outliers <- .get_outliers(e$x$mapping$data, serie)
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

.build_3d <- function(data, y , z){
  e$x$mapping$data %>%
    dplyr::select_(
      y, z
    ) %>%
    unname() -> data
  
  data <- cbind(e$x$mapping$data[[e$x$mapping$x]], data)
  data <- unname(data)
  
  apply(data, 1, as.list)
}

.build_pie <- function(data, serie){
  e$x$mapping$data %>%
    dplyr::select_(
      value = serie
    ) -> data
  
  data <- cbind.data.frame(
    data,
    data.frame(
      name = e$x$mapping$data[[e$x$mapping$x]]
    )
  )
  
  apply(data, 1, as.list)
}

.build_tree <- function(data, parent, child){
  data %>%
    dplyr::select(
      !!parent,
      !!child
    ) -> df
  
  tree <- data.tree::FromDataFrameNetwork(df)
  data.tree::ToListExplicit(tree, unname = TRUE)
}

.build_treemap <- function(data, parent, child, value){
  data %>%
    dplyr::select(
      !!parent,
      !!child,
      !!value
    ) -> df
  
  tree <- data.tree::FromDataFrameNetwork(df)
  data.tree::ToListExplicit(tree, unname = TRUE)
}

.build_river <- function(e, serie, label){
  e$x$data %>%
    dplyr::select(
      !!serie
    ) -> data
  
  label <- data.frame(name = rep(label, nrow(data)))
  data <- cbind(e$X, data, label)
  
  apply(unname(data), 1, as.list)
}

.build_cal <- function(e, serie){
  e$x$data %>% 
    dplyr::select(!!serie) %>% 
    unname() -> rhs
  
  lhs <- as.character(e$x$opts$xAxis$data)
  
  df <- cbind(lhs, rhs)
  
  apply(unname(df), 1, as.list)
}