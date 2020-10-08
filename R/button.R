#' Button
#'
#' Add a button to your visualisation.
#'
#' @inheritParams e_bar
#' @param position Position of button, \code{top} or \code{bottom}.
#' @param tag A Valid \code{htmltools::tags} function.
#' @param id A valid CSS id.
#' @param ... Content of the button, complient with \code{htmltools}.
#'
#' @examples
#' iris %>%
#'   group_by(Species) %>%
#'   e_charts(Sepal.Length) %>%
#'   e_line(Sepal.Width) %>%
#'   e_line(Petal.Length) %>%
#'   e_highlight(series_name = "setosa", btn = "myBtn") %>%
#'   e_button("myBtn", "highlight stuff")
#' @export
e_button <- function(e, id, ..., position = "top", tag = htmltools::tags$button) {
  if (missing(id)) {
    stop("missing id", call. = FALSE)
  }

  if (!id %in% names(e$x$buttons)) {
    e$x$buttons[[id]] <- list()
  }

  if (position == "top") {
    htmlwidgets::prependContent(e, tag(id = id, ...))
  } else {
    htmlwidgets::appendContent(e, tag(id = id, ...))
  }
}
