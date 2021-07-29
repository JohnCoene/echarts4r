globalVariables(c("x", "e", ".", "acc", "epoch", "loss", "size", "val_acc", "val_loss"))

.arrange_data_x <- function(data, x, reorder = TRUE) {
  vect <- data[[x]]

  if (reorder) {
    if (inherits(vect, "numeric") || inherits(vect, "integer")) {
      data <- data[order(data[[x]]), ]
    }
  }

  return(data)
}

.arrange_data_by_group <- function(data, x, reorder = TRUE) {
  vect <- data[[1]][[x]]

  for (i in 1:length(data)) {
    if (reorder) {
      if (inherits(vect, "numeric") || inherits(vect, "integer")) {
        data[[i]] <- data[[i]][order(data[[i]][[x]]), ]
      }
    }
  }

  return(data)
}

.assign_axis <- function(x, data) {
  x$mapping$include_x <- FALSE
  cl <- x$mapping$x_class
  if (cl == "character" || cl == "factor") {
    labs <- unique(data[[x$mapping$x]])

    if (length(labs) == 1) {
      labs <- list(labs)
    }

    x$opts$xAxis <- list(list(data = labs, type = "category", boundaryGap = TRUE))
  } else if (cl == "POSIXct" || cl == "POSIXlt" || cl == "Date") {
    labs <- unique(data[[x$mapping$x]])

    if (length(labs) == 1) {
      labs <- list(labs)
    }

    x$opts$xAxis <- list(list(data = labs, type = "time", boundaryGap = TRUE))
  } else {
    x$mapping$include_x <- TRUE
    x$opts$xAxis <- list(list(type = "value"))
  }
  x
}

.rm_axis <- function(e, rm_x, axis) {
  if (isTRUE(rm_x)) {
    axis <- .r2axis(axis)

    if (!e$x$tl) {
      e$x$opts[[axis]] <- NULL
    } else {
      e$x$opts$baseOption[[axis]] <- NULL
    }
  }
  e
}

.build_data <- function(e, ...) {
  data <- e$x$data[[1]] |>
    dplyr::select(...)
  
  data <- unname(data)

  apply(data, 1, function(x) {
    list(value = unlist(x, use.names = FALSE))
  })
}

.jitter <- function(x, factor = 0, amount = NULL) {
  jit <- tryCatch(
    jitter(x, factor, amount),
    error = function(e) e
  )

  if (inherits(jit, "error")) {
    x
  } else {
    jit
  }
}

.build_data_size <- function(data, x, y, size, scale, symbol_size, factor = 0, amount = NULL) {
  row.names(data) <- NULL

  data[["sizeECHARTS"]] <- as.numeric(data[[size]])

  if (!is.null(scale)) {
    data[["sizeECHARTS"]] <- scale(data[["sizeECHARTS"]]) * symbol_size
  }

  data <- data |>
    dplyr::select(x = x, y = y, size, "sizeECHARTS") |>
    dplyr::mutate(
      x = .jitter(x, factor, amount),
      y = .jitter(y, factor, amount)
    )
  
  data <- unname(data)

  apply(data, 1, function(x) {
    list(value = unlist(x, use.names = FALSE))
  })
}

.build_data_jitter <- function(data, x, y, factor = 0, amount = NULL) {
  row.names(data) <- NULL
  
  data <- data |>
    dplyr::select(x = x, y = y) |>
    dplyr::mutate(
      x = .jitter(x, factor, amount),
      y = .jitter(y, factor, amount)
    ) 
    
    data <- unname(data)

  apply(data, 1, function(x) {
    list(value = unlist(x, use.names = FALSE))
  })
}

.build_data2 <- function(data, ...) {
  row.names(data) <- NULL
  data <- data |>
    dplyr::select(...)
  
  data <- unname(data)

  apply(data, 1, function(x) {
    list(value = unlist(x, use.names = FALSE))
  })
}

.add_bind2 <- function(e, l, bind, col = "name", i) {
  e$x$data[[i]] |>
    dplyr::select(bind) |>
    unname() |>
    unlist() -> bind

  for (i in 1:length(l)) {
    l[[i]][[col]] <- bind[i]
  }
  l
}

.add_bind <- function(e, l, bind, col = "name") {
  e$x$data[[1]] |>
    dplyr::select(bind) |>
    unname() |>
    unlist() -> bind

  for (i in 1:length(l)) {
    l[[i]][[col]] <- bind[i]
  }
  l
}

.build_data_p <- function(data, ..., vector = FALSE, scale = NULL, symbol_size = 1) {
  data |>
    dplyr::select(...) |>
    purrr::set_names(NULL) -> data

  if (!is.null(scale)) {
    data[[4]] <- scale(data[[3]]) * symbol_size
  } else {
    data[[4]] <- data[[3]]
  }

  if (isTRUE(vector)) {
    unlist(data)
  } else {
    apply(data, 1, function(x) {
      list(value = unlist(x, use.names = FALSE))
    })
  }
}

.build_sankey_nodes <- function(data, source, target) {
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

.build_sankey_edges <- function(data, source, target, values) {
  data |>
    dplyr::select(source, target, values) -> edges

  names(edges) <- c("source", "target", "value")

  apply(edges, 1, as.list)
}

.build_graph_nodes <- function(nodes, names, value, symbolSize, category, symbol) {
  row.names(nodes) <- NULL

  nodes |>
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

.build_graph_nodes_no_size <- function(nodes, names, value, symbol) {
  row.names(nodes) <- NULL

  nodes |>
    dplyr::select(
      !!names,
      !!value,
      !!symbol,
    ) -> data

  names(data) <- c("name", "value", "symbol")[1:ncol(data)]

  apply(data, 1, as.list)
}

.build_graph_nodes_no_cat <- function(nodes, names, value, symbolSize, symbol) {
  row.names(nodes) <- NULL

  nodes |>
    dplyr::select(
      !!names,
      !!value,
      !!symbolSize,
      !!symbol
    ) -> data

  names(data) <- c("name", "value", "symbolSize", "symbol")[1:ncol(data)]

  apply(data, 1, as.list)
}

.build_graph_edges <- function(edges, source, target, value, size) {
  row.names(edges) <- NULL
  
  if (is.null(size)) {
    
    edges |>
      dplyr::select(
        source = !!source,
        target = !!target,
        value = !!value
      ) -> data
    
    x <- apply(data, 1, as.list)
    
  } else {
    
    edges |>
      dplyr::select(
        source = !!source,
        target = !!target,
        value = !!value,
        size = !!size
      ) -> data
    
    x <- apply(data, 1, function(x) {
      list(
        source = unname(x["source"]),
        target = unname(x["target"]),
        value = {if (is.null(value)) "" else unname(x["value"])},
        symbolSize = c(5, 20),
        lineStyle = list(width = unname(x["size"]))
      )
    })
    
  }
  
  x
  
}

.build_graph_category <- function(nodes, cat) {
  nodes |>
    dplyr::select(
      name = !!cat
    ) |>
    unique() -> data

  apply(data, 1, as.list) -> x
  names(x) <- NULL
  x
}

.build_boxplot <- function(e, serie, i) {
  x <- .get_data(e, serie, i)

  boxplot.stats(x)$stats
}

.get_outliers <- function(e, serie, i) {
  x <- .get_data(e, serie, i)

  boxplot.stats(x)$out
}

.build_outliers <- function(e, out) {
  x <- length(e$x$opts$series[[1]]$data) - 1
  x <- rep(x, length(out))
  matrix <- cbind(x, out)
  apply(unname(matrix), 1, as.list)
}

.add_outliers <- function(e, serie, i) {
  outliers <- .get_outliers(e, serie, i)
  outliers <- .build_outliers(e, outliers)

  scatter <- list(
    type = "scatter",
    data = outliers
  )

  if (length(e$x$opts$series) == 2) {
    e$x$opts$series[[2]]$data <- append(e$x$opts$series[[2]]$data, outliers)
  } else {
    e$x$opts$series <- append(e$x$opts$series, list(scatter))
  }

  e
}

.build_tree <- function(e, ...) {
  jsonl <- e$x$data[[1]] # in case user's data is already in json-list format
  if ("data.frame" %in% class(jsonl)) {
    jsonl <- jsonlite::toJSON(jsonl, auto_unbox = TRUE, pretty = FALSE) # convert nested tibble to JSON
    jsonl <- jsonlite::fromJSON(jsonl, simplifyDataFrame = FALSE) # JSON to json-list
  }
  if (!jsonlite::validate(jsonlite::toJSON(jsonl))) {
    stop("invalid input data", call. = FALSE)
  }

  jsonl
}

#' Adding styles to hierarchical data by item names or levels
#' @author helgasoft.com
#'
#' @inheritParams e_bar
#' @param styles Style lists, expects a \code{vector}, defaults to \code{NULL}.
#' @param names Names of items to style, expects a \code{list}, defaults to \code{NULL}.
#' @param levels Hierarchical levels to style, expects a \code{list}, defaults to \code{NULL}.
#' @return updated hierarchy in json list format
#'
#' @noRd
#' @keywords internal
.build_sun <- function(e, styles = NULL, names = NULL, levels = NULL) {

  #' recursive json-list traversal, append style on matching level and/or name
  recu <- function(chld, level) {
    if (!is.null(levels)) {
      idLevel <- unlist(lapply(
        seq_along(levels),
        function(x, i) {
          if (level %in% x[[i]]) i
        },
        x = levels
      ))
    }
    if (!is.null(names)) {
      idName <- unlist(lapply(
        seq_along(names),
        function(x, i) {
          if (chld$name %in% x[[i]]) i
        },
        x = names
      ))
    }
    id <- if (length(idName) > 0) idName else idLevel # name supersedes level
    if (length(id) > 0) {
      chld$itemStyle <- styles[id]
    }
    if (!is.null(chld$children)) {
      chld$children <- lapply(
        chld$children,
        function(x) recu(x, level + 1)
      )
      level <- level - 1
    }
    chld
  }

  jsonl <- e$x$data[[1]] # in case user's data is already in json-list format
  if ("data.frame" %in% class(jsonl)) {
    jsonl <- jsonlite::toJSON(jsonl, auto_unbox = TRUE, pretty = FALSE) # convert nested tibble to JSON
    jsonl <- jsonlite::fromJSON(jsonl, simplifyDataFrame = FALSE) # JSON to json-list
  }
  if (!jsonlite::validate(jsonlite::toJSON(jsonl))) {
    stop("invalid input data", call. = FALSE)
  }
  if (!is.null(styles)) {
    jsonl <- lapply(jsonl, function(x) recu(x, level = 1))
  } # recursively add styles
  jsonl
}

.build_river <- function(e, serie, label, i) {
  x <- .get_data(e, e$x$mapping$x, i)
  label <- rep(label, length(x))

  e$x$data[[i]] |>
    dplyr::select(serie) -> data

  data <- cbind(x, data, label)
  row.names(data) <- NULL

  apply(unname(data), 1, as.list)
}

.get_class <- function(e, serie) {
  class(.get_data(e, serie))
}

.get_type <- function(e, serie) {
  cl <- .get_class(e, serie)

  if (cl == "character" || cl == "factor") {
    "category"
  } else if (cl == "POSIXct" || cl == "POSIXlt" || cl == "Date") {
    "time"
  } else {
    "value"
  }
}

.get_data <- function(e, serie, i = 1) {
  data <- e$x$data[[i]] |>
    dplyr::select(serie) |>
    unname()
  
  data[[1]]
}

.set_any_axis <- function(e, serie, index, axis = "x", i = 1) {
  raxis <- .r2axis(axis)

  update <- length(e$x$opts[[raxis]]) - 1 < index || length(e$x$opts$baseOption[[raxis]]) - 1 < index

  if (update) {
    type <- .get_type(e, serie)

    ax <- list(type = type)

    if (type != "value") {
      axis_data <- .get_data(e, serie, i)

      if (length(axis_data) == 1) {
        axis_data <- list(axis_data)
      }

      ax$data <- axis_data
    }

    if (!e$x$tl) {
      e$x$opts[[raxis]][[index + 1]] <- ax
    } else {
      e$x$opts$baseOption[[raxis]][[index + 1]] <- ax
    }
  }

  e
}

.set_x_axis <- function(e, x_index, i) {
  .set_any_axis(e, e$x$mapping$x, x_index, axis = "x", i)
}

.set_y_axis <- function(e, serie, y.index, i) {
  .set_any_axis(e, serie, y.index, axis = "y", i)
}

.set_z_axis <- function(e, serie, z.index, i) {
  .set_any_axis(e, serie, z.index, axis = "z", i)
}

.set_axis_3D <- function(e, axis, serie, index) {
  ax <- .r2axis3D(axis)

  if (length(e$x$opts[[ax]]) - 1 < index) {
    type <- .get_type(e, serie)

    axis <- list(type = type)

    if (type != "value") {
      axis$data <- purrr::map(e$x$data, serie) |>
        unlist() |>
        unique()
    }

    if (!e$x$tl) {
      e$x$opts[[ax]][[index + 1]] <- axis
    } else {
      e$x$opts$baseOption[[ax]][[index + 1]] <- axis
    }
  }
  e
}

.r2axis <- function(axis) {
  paste0(axis, "Axis")
}

.r2axis3D <- function(axis) {
  paste0(axis, "Axis3D")
}

.map_lines <- function(e, source.lon, source.lat, target.lon, target.lat, source.name, target.name, value, i) {
  data <- e$x$data[[i]] |>
    dplyr::select(
      source.lon,
      source.lat,
      target.lon,
      target.lat
    ) |>
    apply(1, function(x) {
      x <- unname(x)
      list(
        coords = list(
          c(x[1], x[2]),
          c(x[3], x[4])
        )
      )
    })
  if (!is.null(source.name)) {
    data <- .add_bind2(e, data, source.name, col = "source_name", i)
  }
  if (!is.null(target.name)) {
    data <- .add_bind2(e, data, target.name, col = "target_name", i)
  }
  if (!is.null(value)) {
    data <- .add_bind2(e, data, value, col = "value", i)
  }
  data
}

.build_cartesian3D <- function(e, ..., i = 1) {
  e$x$data[[i]] |>
    dplyr::select(
      ...
    ) |>
    unname() -> df

  apply(df, 1, function(x) {
    list(value = x)
  })
}


.build_height <- function(e, serie, color, j) {

  # data <- .build_data(e, e$x$mapping$x, serie, names = c("name", "height"))
  e$x$data[[j]] |>
    dplyr::select(
      name = e$x$mapping$x,
      height = serie
    ) -> data

  names(data) <- c("name", "height")

  apply(data, 1, as.list) -> l

  if (!missing(color)) {
    color <- .get_data(e, color)

    for (i in 1:length(l)) {
      is <- list(
        color = color[i]
      )
      l[[i]]$itemStyle <- is
    }
  }
  l
}

.correct_countries <- function(x) {
  dplyr::recode(
    x,
    "United States of America" = "United States",
    "Viet Nam" = "Vietnam",
    "United Kingdom of Great Britain and Northern Ireland" = "United Kingdom",
    "Republic of Korea" = "Korea",
    "Russian Federation" = "Russia",
    "Congo - Kinshasa" = "Dem. Rep. Congo",
    "Congo - Brazzaville" = "Congo",
    "Central African Republic" = "Central African Rep.",
    "South Sudan" = "S. Sudan",
    "North Korea" = "Dem. Rep. Korea",
    "South Korea" = "Korea",
    "Western Sahara" = "W. Sahara",
    "Myanmar (Burma)" = "Myanmar",
    "Laos" = "Lao PDR",
    "C\\u00f4te d\\u2019Ivoire" = "C\\u00f4te d\\'Ivoire",
    "Czechia" = "Czech Rep.",
    "Equatorial Guinea" = "Eq. Guinea",
    "Eswatini" = "Swaziland",
    "Falkland Islands" = "Falkland Is.",
    "South Georgia & South Sandwich Islands" = "S. Geo. and S. Sandw. Is.",
    "French Southern Territories" = "Fr. S. Antarctic Lands",
    "British Indian Ocean Territory" = "Br. Indian Ocean Ter.",
    "Solomon Islands" = "Solomon Is.",
    "Dominican Republic" = "Dominican Rep.",
    "Bosnia & Herzegovina" = "Bosnia and Herz.",
    "North Macedonia" = "Macedonia",
    "Heard & McDonald Islands" = "Heard I. and McDonald Is.",
    "Micronesia (Federated States of)" = "Micronesia",
    "Trinidad & Tobago" = "Trinidad and Tobago",
    "St. Vincent & Grenadines" = "St. Vin. and Gren.",
    "St. Lucia" = "Saint Lucia",
    "Antigua & Barbuda" = "Antigua",
    "U.S. Virgin Islands" = "U.S. Virgin Is.",
    "Faroe Islands" = "Faeroe Is.",
    "\\u00c5lland Islands" = "Aland"
  )
}

.get_index <- function(e, serie) {
  series <- if (e$x$tl) names(e$x$data) else purrr::map(e$x$opts$series, "name") |> unlist()
  purrr::map(serie, ~ which(series == .)) |>
    unlist() |>
    unique()
}

.add_indicators <- function(e, r.index, max, radar = list()) {
  if (!length(e$x$opts$radar)) {
    e$x$opts$radar <- list(list())
  }

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

.name_it <- function(e, serie, name, i) {
  if (is.null(name)) { # defaults to column name
    if (!is.null(names(e$x$data)[i])) {
      nm <- names(e$x$data)[i]
    } else {
      nm <- serie
    }
  } else {
    nm <- name
  }

  return(nm)
}

.list_depth <- function(this, thisdepth = 0) {
  if (!is.list(this)) {
    return(thisdepth)
  } else {
    return(max(unlist(lapply(this, .list_depth, thisdepth = thisdepth + 1))))
  }
}

.e_graphic_elem <- function(e, elem, ...) {
  if (!e$x$tl) {
    if (length(e$x$opts$graphic) == 0) {
      e$x$opts$graphic <- list(...)
    }
  } else
  if (length(e$x$opts$baseOption$graphic) == 0) {
    e$x$opts$baseOption$graphic <- list(...)
  }

  opts <- list(type = elem, ...)

  if (!e$x$tl) {
    e$x$opts$graphic <- append(e$x$opts$graphic, opts)
  } else {
    e$x$opts$baseOption$graphic <- append(e$x$opts$baseOption$graphic, opts)
  }

  e
}

.get_locale <- function() {
  "en"
}

#' Determines the extremum of a vector
#'
#' If the vector contains only 1 element, then one of the extremum is set to 0.
#'
#' @author Wei Su
#'
#' @param rng Numeric vector.
#' @return A list contains maximum and minimum value.
#'
#' @noRd
.get_validate_range <- function(rng) {
  max <- max(rng, na.rm = TRUE)
  min <- min(rng, na.rm = TRUE)
  if (!(max > min)) {
    if (rng[1] >= 0) {
      max <- rng[1]
      min <- 0
    } else {
      max <- 0
      min <- rng[1]
    }
  }
  return(list(max = max, min = min))
}

check_installed <- function(pkg) {
  has_it <- base::requireNamespace(pkg, quietly = TRUE)

  if (!has_it) {
    stop(sprintf("This function requires the package {%s}", pkg), call. = FALSE)
  }
}
