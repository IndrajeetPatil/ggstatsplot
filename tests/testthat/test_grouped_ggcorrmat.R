context("grouped_ggcorrmat")

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
    movies_filtered <- ggstatsplot::movies_long %>%
      dplyr::filter(.data = ., mpaa != "NC-17") %>%
      dplyr::sample_frac(tbl = ., size = 0.25)

    # when arguments are entered as bare expressions
    set.seed(123)
    testthat::expect_true(inherits(
      suppressWarnings(ggstatsplot::grouped_ggcorrmat(
        data = movies_filtered,
        grouping.var = mpaa,
        cor.vars = length:votes,
        corr.method = "p",
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
        corr.method = "np",
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
        corr.method = "p",
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
        corr.method = "r",
        messages = TRUE
      )),
      what = "gg"
    ))
  }
)

# output: stats ---------------------------------------------------------------

testthat::test_that(
  desc = "grouped_ggcorrmat stats work",
  code = {

    # without cor.vars specified --------------------------------------------

    # tidy dataframe
    set.seed(123)
    df1 <- ggstatsplot::grouped_ggcorrmat(
      data = ggplot2::msleep,
      grouping.var = vore,
      output = "r",
      k = 3,
      messages = FALSE
    )

    # testing dataframe
    testthat::expect_equal(dim(df1), c(24L, 8L))
    testthat::expect_equal(df1$sleep_rem[1], 0.919, tolerance = 0.001)

    # with cor.vars specified -----------------------------------------------

    set.seed(123)
    df2 <- ggstatsplot::grouped_ggcorrmat(
      data = ggplot2::msleep,
      cor.vars = awake:bodywt,
      grouping.var = vore,
      output = "r",
      messages = FALSE
    )

    # edge case
    df3 <- ggstatsplot::grouped_ggcorrmat(
      data = ggplot2::msleep,
      grouping.var = vore,
      cor.vars = brainwt,
      output = "p"
    )

    # testing dataframe
    testthat::expect_equal(dim(df2), c(12L, 5L))
    testthat::expect_equal(
      df2$awake,
      c(
        1.00,
        0.52,
        0.44,
        1.00,
        0.39,
        0.38,
        1.00,
        -0.11,
        -0.26,
        1.00,
        0.34,
        0.32
      ),
      tolerance = 0.001
    )

    # testing edge case
    testthat::expect_equal(dim(df3), c(4L, 3L))
    testthat::expect_identical(unique(df3$variable), "brainwt")
    testthat::expect_equal(df3$brainwt, c(0, 0, 0, 0), tolerance = 0.001)
    testthat::expect_identical(
      as.character(levels(as.factor(na.omit(ggplot2::msleep$vore)))),
      df3$vore
    )
  }
)
