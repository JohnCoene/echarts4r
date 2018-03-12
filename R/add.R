#' Bar chart
#'
#' @export
e_bar <- function(e, serie, name = NULL, ...){

  if(missing(serie))
    stop("must pass serie", call. = FALSE)

  if(is.null(name))
    name <- deparse(substitute(serie))

  vector <- .build_vector(e$x$data, dplyr::enquo(serie))

  serie <- list(
    name = name,
    type = "bar",
    data = vector
  )

  e$x$opts$legend$data <- append(e$x$opts$legend$data, list(name))

  e$x$opts$series <- append(e$x$opts$series, list(serie))
  e
}
