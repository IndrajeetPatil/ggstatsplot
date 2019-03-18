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
    testthat::skip_on_cran()

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

    # tidy dataframe
    set.seed(123)
    df11 <- ggstatsplot::grouped_ggcorrmat(
      data = dplyr::select(ggplot2::msleep, vore, awake:brainwt),
      grouping.var = vore,
      output = "ci",
      k = 3,
      messages = FALSE
    )

    # testing dataframe
    testthat::expect_equal(dim(df11), c(4L, 8L))
    testthat::expect_identical(df11$vore, c("carni", "herbi", "insecti", "omni"))
    testthat::expect_identical(
      df11$pair,
      c(
        "awake-brainwt",
        "awake-brainwt",
        "awake-brainwt",
        "awake-brainwt"
      )
    )
    testthat::expect_equal(df11$r,
      c(0.5243674, 0.3855934, -0.1117243, 0.3365608),
      tolerance = 0.01
    )
    testthat::expect_equal(df11$p,
      c(0.14727017, 0.09314295, 0.85804468, 0.18653185),
      tolerance = 0.01
    )
    testthat::expect_equal(df11$lower,
      c(-0.21442721, -0.06863955, -0.90480369, -0.17188998),
      tolerance = 0.01
    )
    testthat::expect_equal(df11$upper,
      c(0.8815091, 0.7074071, 0.8548003, 0.7034165),
      tolerance = 0.01
    )
    testthat::expect_equal(df11$lower.adj,
      c(-0.21442721, -0.06863955, -0.90480369, -0.17188998),
      tolerance = 0.01
    )
    testthat::expect_equal(df11$upper.adj,
      c(0.8815091, 0.7074071, 0.8548003, 0.7034165),
      tolerance = 0.01
    )

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

    # ci
    set.seed(123)
    df4 <- ggstatsplot::grouped_ggcorrmat(
      data = ggplot2::msleep,
      cor.vars = awake:bodywt,
      grouping.var = vore,
      output = "ci",
      messages = FALSE
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

    # testing CIs
    testthat::expect_equal(dim(df4), c(12L, 8L))
    testthat::expect_identical(unique(df4$vore), c("carni", "herbi", "insecti", "omni"))
    testthat::expect_equal(
      df4$r,
      c(
        0.5243674,
        0.4441851,
        0.8600201,
        0.3855934,
        0.3826471,
        0.9447948,
        -0.1117243,
        -0.2572068,
        0.9725380,
        0.3365608,
        0.3158033,
        0.6638581
      ),
      tolerance = 0.001
    )
  }
)
