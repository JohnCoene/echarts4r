#' Wordcloud
#' 
#' Draw a wordcloud.
#' 
#' @inheritParams e_bar
#' @param word,freq Terms and their frequencies.
#' @param color Word color.
#' 
#' @examples 
#' words <- function(n = 5000) {
#'   a <- do.call(paste0, replicate(5, sample(LETTERS, n, TRUE), FALSE))
#'   paste0(a, sprintf("%04d", sample(9999, n, TRUE)), sample(LETTERS, n, TRUE))
#' }
#' 
#' tf <- data.frame(terms = words(100), 
#'   freq = rnorm(100, 55, 10)) %>% 
#'   dplyr::arrange(-freq)
#' 
#' tf %>% 
#'   e_color_range(freq, color) %>% 
#'   e_charts() %>% 
#'   e_cloud(terms, freq, color, shape = "circle", sizeRange = c(3, 15))
#' 
#' @seealso \href{official documentation}{https://github.com/ecomfe/echarts-wordcloud}
#' 
#' @export
e_cloud <- function(e, word, freq, color, ...){
  
  if(missing(e))
    stop("missing e", call. = FALSE)
  
  e$x$opts$xAxis <- NULL # remove
  e$x$opts$yAxis <- NULL # remove
  
  data <- .build_data(e, deparse(substitute(word)), deparse(substitute(freq)),
                      names = c("name", "value"))
  
  if(!missing(color)){
    color <- e$x$data[[deparse(substitute(color))]]
    for(i in 1:length(data)){
      col <- list(
        normal = list(
          color = color[i]
        )
      )
      data[[i]]$textStyle <- col
    }
  }
  
  serie <- list(
    type = "wordCloud",
    data = data,
    ...
  )
  
  e$x$opts$series <- append(e$x$opts$series, list(serie))
  
  e
  
}