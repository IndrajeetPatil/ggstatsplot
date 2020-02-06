# formals for primary and grouped functions ----------------------------

testthat::test_that(
  desc = "checking if formal defaults are the same across primary and grouped",
  code = {
    testthat::skip_on_cran()

    # checking if formal defaults are the same across primary and grouped
    df <- purrr::pmap_dfr(
      .l = list(
        .f1 = tibble::lst(
          ggstatsplot::ggbetweenstats,
          ggstatsplot::ggdotplotstats,
          ggstatsplot::ggbarstats,
          ggstatsplot::ggpiestats,
          ggstatsplot::gghistostats,
          ggstatsplot::ggcorrmat
        ),
        .f2 = list(
          ggstatsplot::grouped_ggbetweenstats,
          ggstatsplot::grouped_ggdotplotstats,
          ggstatsplot::grouped_ggbarstats,
          ggstatsplot::grouped_ggpiestats,
          ggstatsplot::grouped_gghistostats,
          ggstatsplot::grouped_ggcorrmat
        )
      ),
      .f = formals_comparator,
      .id = "function"
    )

    # there should be no discrepancies
    testthat::expect_equal(sum(df$error), 0L)
  }
)



testthat::test_that(
  desc = "argument_count is correct",
  code = {
    testthat::skip_on_cran()

    # compared to basic variant the grouped variant has-
    # additional: grouping.var, title.prefix, ...
    # less: title
    # so the difference should be 2 for all functions

    # creating a dataframe with namespace from package of interest
    ns_df <-
      getNamespaceExports(ns = "ggstatsplot") %>%
      tibble::enframe(x = .) %>%
      dplyr::select(.data = ., value) %>%
      dplyr::filter(.data = ., grepl("^gg|^grouped", value)) %>%
      dplyr::filter(.data = ., value != "ggcoefstats") %>%
      dplyr::mutate(
        .data = .,
        functions = paste("ggstatsplot::", value, sep = "")
      ) %>%
      dplyr::mutate(
        .data = .,
        version = dplyr::case_when(
          grepl("^grouped", value) ~ "grouped",
          TRUE ~ "basic"
        )
      ) %>%
      dplyr::mutate(.data = ., value = gsub("grouped_", "", value)) %>%
      dplyr::arrange(.data = ., value) %>%
      dplyr::mutate(.data = ., rowid = dplyr::row_number()) %>%
      dplyr::group_nest(.tbl = ., rowid) %>%
      dplyr::mutate(
        .data = .,
        n = data %>%
          purrr::map(
            .x = .,
            .f = ~ length(formals(eval(
              rlang::parse_expr(.$functions)
            )))
          )
      ) %>%
      tidyr::unnest(data = ., c(n, data)) %>%
      dplyr::select(.data = ., -functions, -rowid) %>%
      tidyr::spread(
        data = .,
        key = version,
        value = n,
        convert = TRUE,
        drop = TRUE
      ) %>%
      dplyr::mutate(
        .data = .,
        difference = grouped - (basic + 2)
      ) %>%
      tidyr::drop_na(.)

    # testing if formals are as expected
    testthat::expect_equal(sum(ns_df$difference), 0L)
    testthat::expect_equal(ns_df$basic + 2L, ns_df$grouped)
  }
)


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
      !name %in% c("data", "grouping.var", "...", "condition")
    )

  # comparison list
  df_list <- compare_list(df$primary, df$grouped)

  # count the number of discrepancies between formals defaults
  discrepancies <-
    purrr::map_dfc(.x = df_list, .f = rlang::is_false) %>%
    tidyr::gather(.) %>%
    dplyr::summarise(.data = ., error = sum(value))

  # retuen the dataframe
  return(discrepancies)
}
