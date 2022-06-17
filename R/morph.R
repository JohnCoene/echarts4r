#' Morphing
#' 
#' __This is experimental__
#' 
#' Morph between graphs.
#' 
#' @param e,... Graphs (from `e_graph`).
#' @param callback JavaScript callback function as a
#' character string (vector of length 1).
#' This function has access to the `chart` object, as well
#' as `opts` an array containing the options of the charts
#' passed to `e` and `...`.
#' @param default Default chart to show.
#' 
#' @examples 
#' mtcars2 <- mtcars |> 
#'   head() |> 
#'   tibble::rownames_to_column("model")
#' 
#' e1 <- mtcars2 |> 
#'   e_charts(model) |> 
#'   e_bar(
#'     carb, 
#'     universalTransition = TRUE,
#'     animationDurationUpdate = 1000L
#'   )
#' 
#' e2 <- mtcars2 |> 
#'   e_charts(model) |> 
#'   e_pie(
#'     carb, 
#'     universalTransition = TRUE,
#'     animationDurationUpdate = 1000L
#'   )
#' 
#' cb <- "() => {
#'   let x = 0;
#'   setInterval(() => {
#'     x++
#'     chart.setOption(opts[x % 2], true);
#'   }, 3000);
#' }"
#' 
#' e_morph(e1, e2, callback = cb)
#' 
#' @export 
e_morph <- \(e, ..., callback, default = 1L) {
  warning("This is experimental")

  if(...length() < 1)
    stop("Must pass at least one chart")

  if(missing(callback))
    stop("Missing callback")

  opts <- lapply(list(e, ...), \(p) {
    p$x$opts
  }) |> 
    unname()

  e$x$opts <- list(opts)
  e$x$morphed <- list(
    callback = callback,
    default = default - 1L
  )

  return(e)
}