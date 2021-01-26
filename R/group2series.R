# group2series
map_grps_ <- function(data, timeline = FALSE) {
  row.names(data) <- NULL

  if (dplyr::is.grouped_df(data)) {
    splat <- dplyr::group_split(data)
    keys <- dplyr::group_keys(data)[[1]]
    splat <- purrr::map(splat, as.data.frame)
    names(splat) <- keys
    data <- splat
  } else {
    if (inherits(data, "data.frame")) {
      data <- as.data.frame(data)
    } # force data frame

    data <- list(data)
  }

  .no_nm <- function(x) {
    row.names(x) <- NULL
    return(x)
  }

  data <- lapply(data, .no_nm)

  return(data)
}
