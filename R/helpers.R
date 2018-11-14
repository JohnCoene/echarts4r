#' Country names
#' 
#' Convert country names to echarts format.
#' 
#' @param data Data.frame in which to find column names.
#' @param input,output Input and output columns.
#' @param type Passed to \link[countrycode]{countrycode} \code{origin} parameter.
#' @param ... Any other parameter to pass to \link[countrycode]{countrycode}.
#' 
#' @details Taiwan and Hong Kong cannot be plotted.
#' 
#' @examples 
#' cns <- data.frame(country = c("US", "BE"))
#' 
#' # replace
#' e_country_names(cns, country)
#'   
#' # specify output
#' e_country_names(cns, country, country.name)
#'   
#' 
#' @rdname e_country_names
#' @export
e_country_names <- function(data, input, output, type = "iso2c", ...){
  
  if(missing(data) || missing(input))
    stop("must pass data and input", call. = FALSE)
  
  if(missing(output))
    output <- NULL
  else
    output <- deparse(substitute(output))
  
  e_country_names_(data, deparse(substitute(input)), output, type, ...)
}

#' @rdname e_country_names
#' @export
e_country_names_ <- function(data, input, output = NULL, type = "iso2c", ...){
  
  if(missing(data) || missing(input))
    stop("must pass data and input", call. = FALSE)
  
  src <- input
  cn <- countrycode::countrycode(data[[src]], origin = type, destination = "country.name", ...)
  
  if(is.null(output))
    output <- src
  
  data[[output]] <- .correct_countries(cn)
  data
}

#' Color range
#' 
#' Build manual color range
#' 
#' @param data Data.frame in which to find column names.
#' @param input,output Input and output columns.
#' @param colors Colors to pass to \code{\link{colorRampPalette}}.
#' @param ... Any other argument to pass to \code{\link{colorRampPalette}}.
#' 
#' @examples 
#' df <- data.frame(val = 1:10)
#' 
#' e_color_range(df, val, colors)
#'   
#' @rdname e_color_range
#' @export
e_color_range <- function(data, input, output, colors = c("#bf444c", "#d88273", "#f6efa6"), ...){
  
  if(missing(data) || missing(input) || missing(output))
    stop("must pass data, input and output", call. = FALSE)
  
  e_color_range_(data, deparse(substitute(input)), deparse(substitute(output)), colors, ...)
}

#' @rdname e_color_range
#' @export
e_color_range_ <- function(data, input, output, colors = c("#bf444c", "#d88273", "#f6efa6"), ...){
  
  if(missing(data) || missing(input) || missing(output))
    stop("must pass data, input and output", call. = FALSE)
  
  serie <- data[[input]]
  
  data[[output]] <- colorRampPalette(colors, ...)(length(serie))
  data
}

#' Get data
#' 
#' Get data passed to \code{\link{e_charts}}.
#' 
#' @inheritParams e_bar
#' 
#' @export
e_get_data <- function(e){
  e$x$data
}

#' Formatters
#' 
#' Simple formatters as helpers.
#' 
#' @inheritParams e_bar
#' @param axis Axis to apply formatter to.
#' @param suffix,prefix Suffix and prefix of label.
#' @param ... Any other arguments to pass to \code{\link{e_axis}}.
#' 
#' @examples 
#' # Y = %
#' df <- data.frame(
#'   x = 1:10,
#'   y = round(
#'     runif(10, 1, 100), 2
#'   ) 
#' )
#' 
#' df %>% 
#'   e_charts(x) %>% 
#'   e_line(y) %>% 
#'   e_format_y_axis(suffix = "%") %>%
#'   e_format_x_axis(prefix = "A") 
#' 
#' @rdname formatters
#' @export
e_format_axis <- function(e, axis = "y", suffix = NULL, prefix = NULL, ...){
  
  if(missing(e))
    stop("must pass e", call. = FALSE)
  
  if(is.null(suffix) && is.null(prefix))
    stop("missing formatting")
  
  label <- paste(prefix, "{value}", suffix)
  
  e <- e %>% 
    e_axis(
      axis = axis, 
      axisLabel = list(formatter = label),
      ...
    )
  
  e
}

#' @rdname formatters
#' @export
e_format_x_axis <- function(e, suffix = NULL, prefix = NULL, ...){
  
  e_format_axis(e, "x", suffix, prefix, ...)
}

#' @rdname formatters
#' @export
e_format_y_axis <- function(e, suffix = NULL, prefix = NULL, ...){
  
  e_format_axis(e, "y", suffix, prefix, ...)
}

#' Clean
#' 
#' Removes base \code{data.frame}.
#' 
#' @inheritParams e_bar
#' 
#' @details Removes the core database after all operations are exectuted, lightens up the load on final visualisation.
#' 
#' @examples 
#' df <- data.frame(
#'   x = 1:10,
#'   y = round(
#'     runif(10, 1, 100), 2
#'   ) 
#' )
#' 
#' df %>% 
#'   e_charts(x) %>% 
#'   e_line(y) %>% 
#'   e_format_y_axis(suffix = "%") %>%
#'   e_format_x_axis(prefix = "A") %>% 
#'   e_clean()
#' 
#' @export
e_clean <- function(e){
  warning("There is no need for this function any longer.")
  e$x$data <- NULL
  e
}

#' Format labels
#' 
#' @inheritParams e_bar
#' @param show Set to \code{TRUE} to show the labels.
#' @param position Position of labels, see 
#' \href{https://ecomfe.github.io/echarts-doc/public/en/option.html#series-line.label.position}{official documentation}
#'  for the full list of options.
#' @param ... Any other options see 
#' \href{https://ecomfe.github.io/echarts-doc/public/en/option.html#series-line.label}{documentation} for other options.
#' 
#' @examples 
#' mtcars %>% 
#'   e_chart(wt) %>% 
#'   e_scatter(qsec, cyl) %>% 
#'   e_labels(fontSize = 9)
#' 
#' @export
e_labels <- function(e, show = TRUE, position = "top", ...){
  
  if(missing(e))
    stop("must pass e", call. = FALSE)
  
  opts <- list(
    show = show,
    position = position,
    ...
  )
  
  for(i in 1:length(e$x$opts$series)){
    e$x$opts$series[[i]]$label <- opts
  }
  
  return(e)
  
}
