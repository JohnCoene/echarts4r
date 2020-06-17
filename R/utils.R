globalVariables(c("x", "e", ".", "acc", "epoch", "loss", "size", "val_acc", "val_loss"))

`%||%` <- function(x, y) {
  if (!is.null(x)) x else y
}

.arrange_data_x <- function(data, x, reorder = TRUE){
  vect <- data[[x]]
  
  if(reorder){
    if(inherits(vect, "numeric") || inherits(vect, "integer")){
      data <- data[order(data[[x]]), ]
    }
  }
  
  return(data)
}

.arrange_data_by_group <- function(data, x, reorder = TRUE){
  
  vect <- data[[1]][[x]]
  
  for(i in 1:length(data)){
    if(reorder)
      if(inherits(vect, "numeric") || inherits(vect, "integer"))
        data[[i]] <- data[[i]][order(data[[i]][[x]]), ]
  }
  
  return(data)
}

.assign_axis <- function(x, data){
  x$mapping$include_x <- FALSE
  cl <- x$mapping$x_class
  if(cl == "character" || cl == "factor"){
    labs <- unique(data[[x$mapping$x]])
    
    if(length(labs) == 1)
      labs <- list(labs)
    
    x$opts$xAxis <- list(list(data = labs, type = "category", boundaryGap = TRUE))
  } else if(cl == "POSIXct" || cl == "POSIXlt" || cl == "Date") {
    
    labs <- unique(data[[x$mapping$x]])
    
    if(length(labs) == 1)
      labs <- list(labs)
    
    x$opts$xAxis <- list(list(data = labs, type = "time", boundaryGap = TRUE))
  } else {
    x$mapping$include_x <- TRUE
    x$opts$xAxis <- list(list(type = "value"))
  }
  x
}

.rm_axis <- function(e, rm_x, axis){
  if(isTRUE(rm_x)){
    axis <- .r2axis(axis)
    
    if(!e$x$tl)
      e$x$opts[[axis]] <- NULL
    else
      e$x$opts$baseOption[[axis]] <- NULL
  }
  e
}

.build_data <- function(e, ...){
  e$x$data[[1]] %>% 
    dplyr::select(...) %>% 
    unname(.) -> data
  
  apply(data, 1, function(x){
    list(value = unlist(x, use.names = FALSE))
  }) 
  
}

.jitter <- function(x, factor = 0, amount = NULL){
  jit <- tryCatch(
    jitter(x, factor, amount),
    error = function(e) e
  )
  
  if(inherits(jit, "error"))
    x
  else
    jit
}

.build_data_size <- function(data, x, y, size, scale, symbol_size, factor = 0, amount = NULL){
  row.names(data) <- NULL
  
  data[["sizeECHARTS"]] <- as.numeric(data[[size]])
  
  if(!is.null(scale))
    data[["sizeECHARTS"]] <- scale(data[["sizeECHARTS"]]) * symbol_size
  
  data %>% 
    dplyr::select(x = x, y = y, size, "sizeECHARTS") %>% 
    dplyr::mutate(
      x = .jitter(x, factor, amount),
      y = .jitter(y, factor, amount)
    ) %>% 
    unname(.) -> data
  
  apply(data, 1, function(x){
    list(value = unlist(x, use.names = FALSE))
  }) 
  
}

.build_data_jitter <- function(data, x, y, factor = 0, amount = NULL){
  row.names(data) <- NULL
  data %>% 
    dplyr::select(x = x, y = y) %>% 
    dplyr::mutate(
      x = .jitter(x, factor, amount),
      y = .jitter(y, factor, amount)
    ) %>% 
    unname(.) -> data
  
  apply(data, 1, function(x){
    list(value = unlist(x, use.names = FALSE))
  }) 
  
}

.build_data2 <- function(data, ...){
  row.names(data) <- NULL
  data %>% 
    dplyr::select(...) %>% 
    unname(.) -> data
  
  apply(data, 1, function(x){
    list(value = unlist(x, use.names = FALSE))
  }) 
  
}

.add_bind2 <- function(e, l, bind, col = "name", i){
  e$x$data[[i]] %>% 
    dplyr::select(bind) %>% unname() %>% unlist() -> bind
  
  for(i in 1:length(l)){
    l[[i]][[col]] <- bind[i]
  }
  l
}

.add_bind <- function(e, l, bind, col = "name"){
  e$x$data[[1]] %>% 
    dplyr::select(bind) %>% unname() %>% unlist() -> bind
  
  for(i in 1:length(l)){
    l[[i]][[col]] <- bind[i]
  }
  l
}

.build_data_p <- function(data, ..., vector = FALSE, scale = NULL, symbol_size = 1){
  data %>% 
    dplyr::select(...) %>% 
    purrr::set_names(NULL) -> data
  
  if(!is.null(scale))
    data[[4]] <- scale(data[[3]]) * symbol_size
  else
    data[[4]] <- data[[3]]
  
  if(isTRUE(vector))
    unlist(data)
  else
    apply(data, 1, function(x){list(value = unlist(x, use.names = FALSE))}) 
}

.build_sankey_nodes <- function(data, source, target){
  
  nodes <- c(
    unlist(
      dplyr::select(data, source)
    ),
    unlist(
      dplyr::select(data, target)
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
    dplyr::select(source, target, values) -> edges
  
  names(edges) <- c("source", "target", "value")
  
  apply(edges, 1, as.list)
}

.build_graph_nodes <- function(nodes, names, value, symbolSize, category, symbol){
  
  row.names(nodes) <- NULL
  
  nodes %>%
    dplyr::select(
      !!names,
      !!value,
      !!symbolSize,
      !!category,
      !!symbol,
    ) -> data
  
  names(data) <- c("name", "value", "symbolSize", "category", "symbol")[1:ncol(data)]
  
  apply(data, 1, as.list)
}

.build_graph_nodes_no_size <- function(nodes, names, value, symbol){
  
  row.names(nodes) <- NULL
  
  nodes %>%
    dplyr::select(
      !!names,
      !!value,
      !!symbol,
    ) -> data
  
  names(data) <- c("name", "value", "symbol")[1:ncol(data)]
  
  apply(data, 1, as.list)
}

.build_graph_nodes_no_cat <- function(nodes, names, value, symbolSize, symbol){
  
  row.names(nodes) <- NULL
  
  nodes %>%
    dplyr::select(
      !!names,
      !!value,
      !!symbolSize,
      !!symbol
    ) -> data
  
  names(data) <- c("name", "value", "symbolSize", "symbol")[1:ncol(data)]
  
  apply(data, 1, as.list)
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

.build_boxplot <- function(e, serie, i){
  x <- .get_data(e, serie, i)
  
  boxplot.stats(x)$stats
}

.get_outliers <- function(e, serie, i){
  x <- .get_data(e, serie, i)
  
  boxplot.stats(x)$out
}

.build_outliers <- function(e, out){
  x <- length(e$x$opts$series[[1]]$data) - 1
  x <- rep(x, length(out))
  matrix <- cbind(x, out)
  apply(unname(matrix), 1, as.list)
}

.add_outliers <- function(e, serie, i){
  
  outliers <- .get_outliers(e, serie, i)
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

.build_tree <- function(e, ...){
  e$x$data[[1]] %>%
    dplyr::select(...
    ) -> df
  
  .tree_that(df)
}

.tree_that <- function(df){
  tree <- data.tree::FromDataFrameNetwork(df)
  data.tree::ToListExplicit(tree, unname = TRUE)
}

.build_sun <- function(e, parent, child, value, itemStyle = NULL){
  
  cols <- c("value")
  
  e$x$data[[1]] %>%
    dplyr::select(
      parent,
      name = child,
      value = value
    ) -> data
  
  if(!is.null(itemStyle)){
    e$x$data[[1]] %>%
      dplyr::select(
        itemStyle = itemStyle
      ) -> is
    
    data <- cbind.data.frame(data, is)
    cols <- append(cols, "itemStyle")
  }
  
  d3r::d3_nest(data, value_cols = cols, json = FALSE, root = NULL) -> x
  x <- x[["children"]][[1]]
  x$colname <- NULL
  
  if(!is.null(itemStyle)){
    x$children <- lapply(x$children, function(y){
      y$itemStyle <- lapply(y$itemStyle, as.list)
      return(y)
    })
  }
  
  jsonlite::toJSON(x, auto_unbox = TRUE, pretty = FALSE)
}

.build_river <- function(e, serie, label, i){
  
  x <- .get_data(e, e$x$mapping$x, i)
  label <- rep(label, length(x))
  
  e$x$data[[i]] %>%
    dplyr::select(serie) -> data
  
  data <- cbind(x, data, label)
  row.names(data) <- NULL
  
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

.get_data <- function(e, serie, i = 1){
  e$x$data[[i]] %>% 
    dplyr::select(serie) %>% 
    unname() %>% 
    .[[1]]
}

.set_any_axis <- function(e, serie, index, axis = "x", i = 1){
  
  raxis <- .r2axis(axis)
  
  update <- length(e$x$opts[[raxis]]) - 1 < index || length(e$x$opts$baseOption[[raxis]]) - 1 < index
  
  if(update){
    type <- .get_type(e, serie)
    
    ax <- list(type = type)
    
    if(type != "value"){
      axis_data <- .get_data(e, serie, i)
      
      if(length(axis_data) == 1)
        axis_data <- list(axis_data)
      
      ax$data <- axis_data
    }
    
    if(!e$x$tl)
      e$x$opts[[raxis]][[index + 1]] <- ax
    else
      e$x$opts$baseOption[[raxis]][[index + 1]] <- ax
  }
  
  e
}

.set_x_axis <- function(e, x_index, i){
  .set_any_axis(e, e$x$mapping$x, x_index, axis = "x", i)
}

.set_y_axis <- function(e, serie, y.index, i){
  .set_any_axis(e, serie, y.index, axis = "y", i)
}

.set_z_axis <- function(e, serie, z.index, i){
  .set_any_axis(e, serie, z.index, axis = "z", i)
}

.set_axis_3D <- function(e, axis, serie, index){
  
  ax <- .r2axis3D(axis)
  
  if(length(e$x$opts[[ax]]) - 1 < index){
    type <- .get_type(e, serie)
    
    axis <- list(type = type)
    
    if(type != "value")
      axis$data <- purrr::map(e$x$data, serie) %>% unlist() %>% unique()
    
    if(!e$x$tl)
      e$x$opts[[ax]][[index + 1]] <- axis
    else
      e$x$opts$baseOption[[ax]][[index + 1]] <- axis
  }
  e
}

.r2axis <- function(axis){
  paste0(axis, "Axis")
}

.r2axis3D <- function(axis){
  paste0(axis, "Axis3D")
}

.map_lines <- function(e, source.lon, source.lat, target.lon, target.lat, source.name, target.name, value, i){
  
  data <- e$x$data[[i]] %>% 
    dplyr::select(
      source.lon, source.lat, target.lon, target.lat
    ) %>% 
    apply(., 1, function(x){
      x <- unname(x)
      list(
        coords = list(
          c(x[1], x[2]),
          c(x[3], x[4])
        )
      )
    }) 
  if (!is.null(source.name)){
    data <- .add_bind2(e,data,source.name,col="source_name",i)
  }
  if (!is.null(target.name)){
    data <- .add_bind2(e,data,target.name,col="target_name",i)
  }
  if (!is.null(value)){
    data <- .add_bind2(e,data,value,col="value",i)
  }
  data
}

.build_cartesian3D <- function(e, ..., i = 1){
  e$x$data[[i]] %>%
    dplyr::select(
      ...
    ) %>%
    unname() -> df
  
  apply(df, 1, function(x){
    list(value = x)
  })
}


.build_height <- function(e, serie, color, j){
  
  #data <- .build_data(e, e$x$mapping$x, serie, names = c("name", "height"))
  e$x$data[[j]] %>%
    dplyr::select(
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
  series <- purrr::map(e$x$opts$series, "name") %>% unlist()
  purrr::map(serie, ~which(series == .)) %>% unlist() %>% unique()
}

.add_indicators <- function(e, r.index, max, radar = list()){
  
  if(!length(e$x$opts$radar))
    e$x$opts$radar <- list(list())
  
  x <- .get_data(e, e$x$mapping$x)
  
  indicators <- data.frame(
    name = x,
    max = rep(max, length(x))
  )
  
  indicators <- apply(indicators, 1, as.list)
  
  e$x$opts$radar[[r.index + 1]] <- radar
  e$x$opts$radar[[r.index + 1]]$indicator <- indicators
  e
}

.name_it <- function(e, serie, name, i){
  if(is.null(name)) # defaults to column name
    if(!is.null(names(e$x$data)[i])){
      nm <- names(e$x$data)[i]
    } else {
      nm <- serie
    }
  else 
    nm <- name
  
  return(nm)
}

"%||%" <- function(x, y) {
  if (is.null(x)) 
    x 
  else if(is.na(y))
    x
  else y
}

.list_depth <- function(this,thisdepth=0){
  if(!is.list(this)){
    return(thisdepth)
  }else{
    return(max(unlist(lapply(this,.list_depth,thisdepth=thisdepth+1))))    
  }
}

.e_graphic_elem <- function(e, elem, ...){
  
  if(!e$x$tl){
    if(length(e$x$opts$graphic) == 0)
      e$x$opts$graphic <- list(...)
  } else 
    if(length(e$x$opts$baseOption$graphic) == 0)
      e$x$opts$baseOption$graphic <- list(...)
  
  opts <- list(type = elem, ...)
  
  if(!e$x$tl)
    e$x$opts$graphic <- append(e$x$opts$graphic, opts)
  else 
    e$x$opts$baseOption$graphic <- append(e$x$opts$baseOption$graphic, opts)
  
  e
}

.get_locale <- function(){
  "en"
}
