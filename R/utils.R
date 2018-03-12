# build vector
.build_vector <- function(data, x){
  data %>%
    dplyr::select(!!x) %>%
    unlist() %>%
    unname()
}
