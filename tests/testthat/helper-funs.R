# function to extract all numbers from text results from helper functions

num_parser <- function(ggstats.obj) {
  suppressWarnings(expr = readr::parse_number(
    x = as.character(ggstats.obj),
    na = "NA"
  )) %>%
    tibble::enframe(x = .) %>%
    dplyr::select(.data = ., value) %>%
    dplyr::filter(.data = ., !is.na(value)) %>%
    purrr::flatten_dbl(.x = .)
}
