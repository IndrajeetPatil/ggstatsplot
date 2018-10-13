context("argument_count")

testthat::test_that(
  desc = "argument_count is correct",
  code = {

    # compared to basic variant the grouped variant has-
    # additional: grouping.var, title.prefix, ...
    # less: title
    # so the difference should be 2 for all functions

    # creating a dataframe with namespace from package of interest
    ns_df <- getNamespaceExports(ns = "ggstatsplot") %>%
      tibble::as_data_frame(.) %>%
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
      dplyr::mutate(
        .data = .,
        value = stringr::str_remove(value, "grouped_")
      ) %>%
      dplyr::arrange(.data = ., value) %>%
      purrrlyr::by_row(
        .d = .,
        ..f = ~length(formals(eval(
          rlang::parse_expr(.$functions)
        ))),
        .collate = "cols",
        .to = "n"
      ) %>%
      dplyr::select(.data = ., -functions) %>%
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
      stats::na.omit(.)

    # sum of difference should be exactly 0
    testthat::expect_equal(
      object = sum(ns_df$difference),
      expected = 0L
    )
  }
)
