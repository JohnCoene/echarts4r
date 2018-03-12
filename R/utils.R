# build vector
.build_vector <- function(data, x){
  data %>%
    dplyr::select(!!x) %>%
    unlist() %>%
    unname()
}

.build_xy <- function(data, x, serie, size){
  
  if(!missing(size)){
    data %>%
      dplyr::select(!!serie, !!size) -> rhs
  } else {
    data %>%
      dplyr::select(!!serie) -> rhs
  }
  
  lhs <- data.frame(
    x = x
  )
  
  df <- cbind.data.frame(lhs, rhs)
  
  matrix <- unname(as.matrix(df))
  
  apply(matrix, 1, as.list)
}

.build_candle <- function(data, opening, closing, low, high){
  data %>%
    dplyr::select(
      !!opening, 
      !!closing, 
      !!low, 
      !!high
    ) -> data
  
  matrix <- unname(as.matrix(data))
  
  apply(matrix, 1, as.list)
}