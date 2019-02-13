context(desc = "formals")

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
      .f = formals_comparator
    )

    # there should be no discrepancies
    testthat::expect_equal(sum(df$error), 0L)
  }
)
