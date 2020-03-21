# output: plot ---------------------------------------------------------------

testthat::test_that(
  desc = "grouped_ggcorrmat plots work",
  code = {
    testthat::skip_on_cran()

    # with grouping.var missing ---------------------------------------------

    testthat::expect_error(ggstatsplot::grouped_ggcorrmat(iris))

    # with cor.vars specified -----------------------------------------------

    # creating a smaller dataframe
    set.seed(123)
    movies_filtered <-
      ggstatsplot::movies_long %>%
      dplyr::filter(.data = ., mpaa != "NC-17") %>%
      dplyr::sample_frac(tbl = ., size = 0.25)

    # when arguments are entered as bare expressions
    set.seed(123)
    testthat::expect_true(inherits(
      suppressWarnings(ggstatsplot::grouped_ggcorrmat(
        data = movies_filtered,
        grouping.var = mpaa,
        cor.vars = length:votes,
        type = "p",
        messages = FALSE
      )),
      what = "gg"
    ))

    # when arguments are entered as character
    set.seed(123)
    testthat::expect_true(inherits(
      suppressWarnings(ggstatsplot::grouped_ggcorrmat(
        data = movies_filtered,
        grouping.var = "mpaa",
        cor.vars = c("length":"votes"),
        type = "np",
        messages = FALSE
      )),
      what = "gg"
    ))

    # without cor.vars specified -------------------------------------------

    # when arguments are entered as bare expressions
    set.seed(123)
    testthat::expect_true(inherits(
      suppressWarnings(ggstatsplot::grouped_ggcorrmat(
        data = movies_filtered,
        grouping.var = mpaa,
        type = "p",
        messages = FALSE
      )),
      what = "gg"
    ))

    # when arguments are entered as bare expressions
    set.seed(123)
    testthat::expect_true(inherits(
      suppressWarnings(ggstatsplot::grouped_ggcorrmat(
        data = movies_filtered,
        grouping.var = "mpaa",
        type = "r",
        messages = FALSE
      )),
      what = "gg"
    ))
  }
)

# output: stats ---------------------------------------------------------------

testthat::test_that(
  desc = "grouped_ggcorrmat stats work",
  code = {
    testthat::skip_on_cran()

    # without cor.vars specified --------------------------------------------

    # tidy dataframe
    set.seed(123)
    df1 <-
      ggstatsplot::grouped_ggcorrmat(
        data = ggplot2::msleep,
        grouping.var = vore,
        output = "r",
        k = 3,
        messages = FALSE
      )

    # testing dataframe
    testthat::expect_equal(dim(df1), c(60L, 11L))
  }
)
