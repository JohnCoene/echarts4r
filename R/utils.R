globalVariables(c("e", "."))

.assign_axis <- function(x){
  x$mapping$include_x <- FALSE
  if(x$mapping$x_class == "character" || x$mapping$x_class == "factor"){
    x$opts$xAxis <- list(list(data = x$mapping$data[[x$mapping$x]], type = "category", boundaryGap = FALSE))
  } else if(x$mapping$x_class == "POSIXct" || x$mapping$x_class == "POSIXlt" || x$mapping$x_class == "Date") {
    x$opts$xAxis <- list(list(data = x$mapping$data[[x$mapping$x]], type = "time", boundaryGap = FALSE))
  } else {
    x$mapping$data <- x$mapping$data %>% 
      dplyr::arrange_(x$mapping$x)
    x$mapping$include_x <- TRUE
    x$opts$xAxis <- list(list(type = "value"))
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
    dplyr::select_(
      opening, 
      closing, 
      low, 
      high
    ) %>% 
    as.matrix(.) %>% 
    unname(.) -> matrix
  
  apply(matrix, 1, as.list)
}

.build_funnel <- function(data, x, y){
  data %>%
    dplyr::select_(value = x, name = y) -> df
  
  apply(df, 1, as.list)
}

.build_sankey_nodes <- function(data, source, target){
  
  nodes <- c(
    unlist(
      dplyr::select_(data, source)
    ),
    unlist(
      dplyr::select_(data, target)
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

.graph_cat_legend <- function(e){
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

.build_3d <- function(e, y , z){
  e$x$mapping$data %>%
    dplyr::select_(
      y, z
    ) %>%
    unname() -> data
  
  data <- cbind(e$x$mapping$data[[e$x$mapping$x]], data)
  data <- unname(data)
  
  apply(data, 1, as.list)
}

.build_pie <- function(e, serie){
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

.build_tree <- function(e, parent, child){
  e$x$mapping$data %>%
    dplyr::select(
      !!parent,
      !!child
    ) -> df
  
  tree <- data.tree::FromDataFrameNetwork(df)
  data.tree::ToListExplicit(tree, unname = TRUE)
}

.build_sun <- function(e, value){
  
  tree <- data.tree::FromDataFrameNetwork(e$x$mapping$data, check = "no-check")
  data.tree::ToListExplicit(tree, unname = TRUE)
}

.build_treemap <- function(e, parent, child, value){
  e$x$mapping$data %>%
    dplyr::select_(
      parent,
      child,
      value
    ) -> df
  
  tree <- data.tree::FromDataFrameNetwork(df)
  data.tree::ToListExplicit(tree, unname = TRUE)
}

.build_river <- function(e, serie, label){
  
  x <- e$x$mapping$data[[e$x$mapping$x]]
  label <- rep(label, length(x))
  
  e$x$mapping$data %>%
    dplyr::select_(serie) -> data
  
  data <- cbind(x, data, label)
  
  apply(unname(data), 1, as.list)
}

.build_cal <- function(e, serie){
  e$x$mapping$data %>% 
    dplyr::select_(serie) %>% 
    unname() -> rhs
  
  lhs <- as.character(.get_data(e, e$x$mapping$x))
  
  df <- cbind(lhs, rhs)
  
  apply(unname(df), 1, as.list)
}

.build_map <- function(e, serie, type = "world", name = NULL, t = "map", ...){
  e$x$opts$xAxis <- NULL # remove
  e$x$opts$yAxis <- NULL # remove
  
  # build JSON data
  data <- .build_xy(e, serie)
  
  serie <- list(
    name = name,
    type = t,
    mapType = type,
    data = data,
    ...
  )
  
  e$x$opts$series <- append(e$x$opts$series, list(serie))
  e
}

.get_class <- function(e, serie){
  class(.get_data(e, serie))
}

.get_type <- function(e, serie){
  cl <- .get_class(e, serie)
  
  if(cl == "character" || cl == "factor"){
    "category"
  } else if(cl == "POSIXct" || cl == "POSIXlt" || cl == "Date") {
    "time"
  } else {
    "value"
  }
}

.get_data <- function(e, serie){
  e$x$mapping$data %>% 
    dplyr::select_(serie) %>% 
    unname() %>% 
    .[[1]]
}

.set_y_axis <- function(e, serie, y.index){
  
  if(length(e$x$opts$yAxis) - 1 < y.index){
    type <- .get_type(e, serie)
    
    axis <- list(type = type)
    
    if(type != "value"){
      axis$data <- .get_data(e, serie)
    }
    
    e$x$opts$yAxis[[y.index + 1]] <- axis
  }
  e
}

.set_x_axis <- function(e, x.index){
  
  serie <- e$x$mapping$x
  
  if(length(e$x$opts$xAxis) - 1 < x.index){
    type <- .get_type(e, serie)
    
    axis <- list(type = type, show = TRUE, boundaryGap = FALSE)
    
    if(type != "value"){
      axis$data <- .get_data(e, serie)
    }
    
    e$x$opts$xAxis[[x.index + 1]] <- axis
  }
  e
}

.r2axis <- function(x){
  ifelse(x == "x", "xAxis", "yAxis")
}

.map_lines <- function(e, source.lon, source.lat, target.lon, target.lat){
  
  e$x$mapping$data %>% 
    dplyr::select_(
      source.lon, source.lat, target.lon, target.lat
    ) %>% 
    apply(., 1, function(x){
      x <- unname(x)
      list(
        c(x[1], x[2]),
        c(x[3], x[4])
      )
    }) 
}

.build_globexyz <- function(e, x, y, z){
  e$x$mapping$data %>%
    dplyr::select_(
      x, y, z
    ) %>%
    unname() -> df
  
  apply(df, 1, as.list)
}

.get_file <- function(file, convert){
  file <- system.file(file, package = "echarts4r")
  if(isTRUE(convert))
    paste0("data:image/png;base64,", base64enc::base64encode(file)) -> file
  file
}