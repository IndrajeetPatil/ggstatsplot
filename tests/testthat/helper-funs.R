# function to extract all numbers from text results from helper functions
num_parser <- function(ggstats.obj) {
  suppressWarnings(readr::parse_number(
    x = as.character(ggstats.obj),
    na = "NA"
  )) %>%
    tibble::enframe(x = .) %>%
    dplyr::select(.data = ., value) %>%
    dplyr::filter(.data = ., !is.na(value)) %>%
    purrr::flatten_dbl(.x = .)
}

# function to compare lists
compare_list <- function(a, b) {
  # computing length of two lists
  a.length <- length(a)
  b.length <- length(b)

  # the two lists need to be of same length; check that
  if (a.length != b.length) {
    stop("a and b must be the same length", call. = FALSE)
  }

  # create a vector of results
  result <- rep(FALSE, a.length)

  # checking element-wise
  for (i in 1:a.length) {
    result[i] <- identical(a[[i]], b[[i]])
  }

  # remove the unnecessary objects created
  rm(a, b, a.length, b.length)

  # return the vector of logicals
  return(result)
}

# function to extract formals two functions that need to be compared
formals_comparator <- function(.f1, .f2) {
  df <- formals(.f1) %>%
    as.list() %>%
    tibble::enframe(x = ., value = "primary") %>%
    dplyr::full_join(
      x = .,
      y = formals(.f2) %>%
        as.list() %>%
        tibble::enframe(x = ., value = "grouped"),
      by = "name"
    ) %>%
    dplyr::filter(
      .data = .,
      !name %in% c(
        "data",
        "grouping.var",
        "...",
        "condition"
      )
    )

  # comparison list
  df_list <- compare_list(df$primary, df$grouped)

  # count the number of discrepancies between formals defaults
  discrepancies <-
    purrr::map_dfc(df_list, rlang::is_false) %>%
    tidyr::gather() %>%
    dplyr::summarise(error = sum(value))

  # retuen the dataframe
  return(discrepancies)
}
