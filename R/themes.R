#' Themes
#'
#' Add a custom theme or apply a pre-built one.
#'
#' @inheritParams e_bar
#' @param theme Theme, A json string or a see below.
#' @param name Name of theme.
#'
#' @section Functions:
#' \itemize{
#'  \item{\code{e_theme} - Use a default theme by name.}
#'  \item{\code{e_theme_custom} - Use a custom theme.}
#'  \item{\code{e_theme_register} - Register a theme globally in shiny or R markdown.}
#' }
#'
#' @details The function \code{e_theme_register} can be used to register the theme globally
#' in R markdown or shiny (UI). This is useful because 1) the \code{e_theme_custom} registers the
#' theme every time and is more computationally expensive.
#'
#' @examples
#' mtcars |>
#'   e_charts(mpg) |>
#'   e_line(disp) |>
#'   e_area(hp) |>
#'   e_x_axis(min = 10) -> p
#'
#' p |> e_theme("chalk")
#' p |> e_theme_custom('{"color":["#ff715e","#ffaf51"]}')
#' @seealso \href{https://echarts.apache.org/en/theme-builder.html}{create your own theme}.
#'
#' @rdname theme
#' @export
e_theme <- function(
  e, 
  name = c("auritus", "azul", "bee-inspired", "blue", "caravan", "carp", 
    "chalk", "cool", "dark-blue", "dark-bold", "dark-digerati", "dark-fresh-cut", 
    "dark-mushroom", "dark", "eduardo", "essos", "forest", "fresh-cut", 
    "fruit", "gray", "green", "halloween", "helianthus", "infographic", 
    "inspired", "jazz", "london", "macarons", "macarons2", "mint", 
    "purple-passion", "red-velvet", "red", "roma", "royal", "sakura", 
    "shine", "tech-blue", "vintage", "walden", "wef", "weforum", 
    "westeros", "wonderland"
  )
) {
  # cannot match.arg we need to support custom themes
  name <- name[1]

  # add name to register
  e$x$theme <- name

  # add dependency
  path <- system.file("htmlwidgets/lib/echarts-4.8.0/themes", package = "echarts4r")
  dep <- htmltools::htmlDependency(
    name = name,
    version = "1.0.0",
    src = c(file = path),
    script = paste0(name, ".js")
  )

  e$dependencies <- append(e$dependencies, list(dep))
  e
}

#' @rdname theme
#' @export
e_theme_custom <- function(e, theme, name = "custom") {
  if (missing(theme)) {
    stop("must pass `theme`", call. = FALSE)
  }

  if (grepl("\\.json", theme)) {
    theme <- readLines(theme) # read the file
    theme <- paste0(theme, collapse = "") # collapse to a string
  }

  e$x$theme2 <- TRUE
  e$x$customTheme <- theme
  e$x$theme <- "custom"
  e$x$theme_name <- name
  e
}

#' @rdname theme
#' @export
e_theme_register <- function(theme, name = "custom") {
  if (missing(theme)) {
    stop("must pass `theme`", call. = FALSE)
  }

  if (grepl("\\.json", theme)) {
    theme <- readLines(theme) # read the file
    theme <- paste0(theme, collapse = "") # collapse to a string
  }

  shiny::singleton(
    shiny::tags$head(
      shiny::tags$script(
        paste0("window.echarts.registerTheme('", name, "', ", theme, ");")
      )
    )
  )
}
