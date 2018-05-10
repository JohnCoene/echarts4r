globalVariables(c("e", "."))

`%||%` <- function(x, y) {
  if (!is.null(x)) x else y
}

.assign_axis <- function(x){
  x$mapping$include_x <- FALSE
  cl <- x$mapping$x_class
  if(cl == "character" || cl == "factor"){
    x$opts$xAxis <- list(list(data = unique(x$data[[x$mapping$x]]), type = "category", boundaryGap = TRUE))
  } else if(cl == "POSIXct" || cl == "POSIXlt" || cl == "Date") {
    x$opts$xAxis <- list(list(data = unique(x$data[[x$mapping$x]]), type = "time", boundaryGap = TRUE))
  } else {
    x$data <- x$data %>% 
      dplyr::arrange_(x$mapping$x)
    x$mapping$include_x <- TRUE
    x$opts$xAxis <- list(list(type = "value"))
  }
  x
}

.rm_axis <- function(e, rm.x, axis){
  if(isTRUE(rm.x)){
    axis <- .r2axis(axis)
    e$x$opts[[axis]] <- NULL
  }
  e
}

.build_data <- function(e, ...){
  e$x$data %>% 
    dplyr::select_(...) %>% 
    unname(.) -> data
  
  apply(data, 1, function(x){
    list(value = unlist(x, use.names = FALSE))
  }) 
    
}

.add_bind <- function(e, l, bind, col = "name"){
  e$x$data %>% 
    dplyr::select_(bind) %>% unname() %>% unlist() -> bind
  
  for(i in 1:length(l)){
    l[[i]][[col]] <- bind[i]
  }
  l
}

.build_data_p <- function(data, ..., names = NULL, vector = FALSE){
  data %>% 
    dplyr::select_(...) %>% 
    purrr::set_names(names) -> data
  
  if(isTRUE(vector))
    unlist(data)
  else
    apply(data, 1, as.list)
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
    dplyr::select_(source, target, values) -> edges
  
  names(edges) <- c("source", "target", "value")
  
  apply(edges, 1, as.list)
}

.build_graph_nodes <- function(nodes, names, value, symbolSize, category){
  
  row.names(nodes) <- NULL
  
  nodes %>%
    dplyr::select(
      !!names,
      !!value,
      !!symbolSize,
      !!category
    ) -> data
  
  names(data) <- c("name", "value", "symbolSize", "category")[1:ncol(data)]
  
  x <- apply(data, 1, as.list)
  
  # for(i in 1:length(x)){
  #   x[[i]]$symbolSize <- as.numeric(paste(x[[i]]$symbolSize))
  #   x[[i]]$value <- as.numeric(paste(x[[i]]$value))
  #   x[[i]]$name <- trimws(as.character(x[[i]]$name))
  # }
  x
}

.build_graph_nodes_no_size <- function(nodes, names, value){
  
  row.names(nodes) <- NULL
  
  nodes %>%
    dplyr::select(
      !!names,
      !!value
    ) -> data
  
  names(data) <- c("name", "value")[1:ncol(data)]
  
  x <- apply(data, 1, as.list)
  
  # for(i in 1:length(x)){
  #   x[[i]]$value <- as.numeric(paste(x[[i]]$value))
  #   x[[i]]$name <- trimws(as.character(x[[i]]$name))
  # }
  x
}

.build_graph_nodes_no_cat <- function(nodes, names, value, symbolSize){
  
  row.names(nodes) <- NULL
  
  nodes %>%
    dplyr::select(
      !!names,
      !!value,
      !!symbolSize
    ) -> data
  
  names(data) <- c("name", "value", "symbolSize")[1:ncol(data)]
  
  x <- apply(data, 1, as.list)
  
  # for(i in 1:length(x)){
  #   x[[i]]$symbolSize <- as.numeric(paste(x[[i]]$symbolSize))
  #   x[[i]]$value <- as.numeric(paste(x[[i]]$value))
  #   x[[i]]$name <- trimws(as.character(x[[i]]$name))
  # }
  x
}

.build_graph_edges <- function(edges, source, target){
  
  row.names(edges) <- NULL
  
  edges %>%
    dplyr::select(
      !!source,
      !!target
    ) -> data
  
  names(data) <- c("source", "target")
  
  apply(data, 1, as.list) -> x
  # for(i in 1:length(x)){
  #   x[[i]]$source <- trimws(as.character(x[[i]]$source))
  #   x[[i]]$target <- trimws(as.character(x[[i]]$target))
  # }
  x
}

.build_graph_category <- function(nodes, cat){
  nodes %>%
    dplyr::select(
      name = !!cat
    ) %>% 
    unique() -> data
  
  apply(data, 1, as.list) -> x
  names(x) <- NULL
  x
}

.build_boxplot <- function(e, serie){
  x <- .get_data(e, serie)
  
  boxplot.stats(x)$stats
}

.get_outliers <- function(e, serie){
  x <- .get_data(e, serie)
  
  boxplot.stats(x)$out
}

.build_outliers <- function(e, out){
  x <- length(e$x$opts$series[[1]]$data) - 1
  x <- rep(x, length(out))
  matrix <- cbind(x, out)
  apply(unname(matrix), 1, as.list)
}

.add_outliers <- function(e, serie){
  
  outliers <- .get_outliers(e, serie)
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

.build_tree <- function(e, parent, child){
  e$x$data %>%
    dplyr::select(
      !!parent,
      !!child
    ) -> df
  
  .tree_that(df)
}

.tree_that <- function(df){
  tree <- data.tree::FromDataFrameNetwork(df)
  data.tree::ToListExplicit(tree, unname = TRUE)
}

.build_sun <- function(e, parent, child, value){
  e$x$data %>%
    dplyr::select_(
      parent,
      name = child,
      value = value
    ) -> data
  
  d3r::d3_nest(data, value_cols = "value", json = FALSE, root = NULL) -> x
  x <- x[["children"]][[1]]
  x$colname <- NULL
  jsonlite::toJSON(x, auto_unbox = T, pretty = FALSE)
}

.build_river <- function(e, serie, label){
  
  x <- .get_data(e, e$x$mapping$x)
  label <- rep(label, length(x))
  
  e$x$data %>%
    dplyr::select_(serie) -> data
  
  data <- cbind(x, data, label)
  
  apply(unname(data), 1, as.list)
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
  e$x$data %>% 
    dplyr::select_(serie) %>% 
    unname() %>% 
    .[[1]]
}

.set_any_axis <- function(e, serie, index, axis = "x"){
  
  raxis <- .r2axis(axis)
  
  if(length(e$x$opts[[raxis]]) - 1 < index){
    type <- .get_type(e, serie)
    
    axis <- list(type = type)
    
    if(type != "value"){
      axis$data <- .get_data(e, serie)
    }
    
    e$x$opts[[raxis]][[index + 1]] <- axis
  }
  e
}

.set_x_axis <- function(e, x.index){
  .set_any_axis(e, e$x$mapping$x, x.index, axis = "x")
}

.set_y_axis <- function(e, serie, y.index){
  .set_any_axis(e, serie, y.index, axis = "y")
}

.set_z_axis <- function(e, serie, z.index){
  .set_any_axis(e, serie, z.index, axis = "z")
}

.set_axis_3D <- function(e, axis, serie, index){
  
  ax <- .r2axis3D(axis)
  
  if(length(e$x$opts[[ax]]) - 1 < index){
    type <- .get_type(e, serie)
    
    axis <- list(type = type)
    
    if(type != "value")
      axis$data <- unique(.get_data(e, serie))
    
    e$x$opts[[ax]][[index + 1]] <- axis
  }
  e
}

.r2axis <- function(axis){
  paste0(axis, "Axis")
}

.r2axis3D <- function(axis){
  paste0(axis, "Axis3D")
}

.map_lines <- function(e, source.lon, source.lat, target.lon, target.lat){
  
  e$x$data %>% 
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

.get_file <- function(file, convert){
  file <- system.file(file, package = "echarts4r")
  if(isTRUE(convert))
    e_convert_texture(file) -> file
  file
}

.build_cartesian3D <- function(e, ...){
  e$x$data %>%
    dplyr::select_(
      ...
    ) %>%
    unname() -> df
  
  apply(df, 1, function(x){
    list(value = x)
  })
}


.build_height <- function(e, serie, color){
  
  #data <- .build_data(e, e$x$mapping$x, serie, names = c("name", "height"))
  e$x$data %>%
    dplyr::select_(
      name = e$x$mapping$x,
      height = serie
    ) -> data
  
  names(data) <- c("name", "height")
  
  apply(data, 1, as.list) -> l
  
  if(!missing(color)){
    color <- .get_data(e, color)
    
    for(i in 1:length(l)){
      is <- list(
        color = color[i]
      )
      l[[i]]$itemStyle <- is
    }
  }
  l
}

.correct_countries <- function(x){
  x <- gsub("^United States of America$", "United States", x)
  x <- gsub("^Viet Nam$", "Vietnam", x)
  x <- gsub("^United Kingdom of Great Britain and Northern Ireland$", "United Kingdom", x)
  x <- gsub("^Republic of Korea$", "Korea", x)
  x <- gsub("^Russian Federation$", "Russia", x)
  x
}

.get_index <- function(e, serie){
  purrr::map(e$x$opts$series, "name") %>% 
    unlist() %>% 
    grep(serie, .)
}

.build_model <- function(e, model, name, symbol){
  data <- broom::augment(model)
  
  data_keep <- e$x$data
  e <- e %>% e_data(data)
  
  vector <- .build_data(
    e, 
    names(data)[[2]],
    names(data)[[3]]
  )
  
  l <- list(
    name = name,
    type = "line",
    data = vector,
    symbol = symbol,
    smooth = TRUE
  )
  
  e <- e %>% e_data(data_keep)
  
  e$x$opts$series <- append(e$x$opts$series, list(l))
  
  e
}

.add_indicators <- function(e, r.index, max){
  
  if(!length(e$x$opts$radar))
    e$x$opts$radar <- list(list())
  
  x <- .get_data(e, e$x$mapping$x)
  
  indicators <- data.frame(
    name = x,
    max = rep(max, length(x))
  )
  
  indicators <- apply(indicators, 1, as.list)
  
  e$x$opts$radar[[r.index + 1]]$indicator <- indicators
  e
}