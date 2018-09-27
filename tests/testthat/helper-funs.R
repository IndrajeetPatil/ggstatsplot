


# function to extract all numbers from text results from helper functions

# x <-
#   ggstatsplot::subtitle_ggbetween_anova_parametric(data = iris,
#                                                    x = Species,
#                                                    y = Sepal.Length)
# num_parser(x)

num_parser <- function(ggstats.obj) {
  suppressWarnings(expr = readr::parse_number(x = as.character(ggstats.obj), na = "NA")) %>%
    tibble::as.tibble(x = .) %>%
    dplyr::filter(.data = ., !is.na(value)) %>%
    purrr::flatten_dbl(.x = .)
}
