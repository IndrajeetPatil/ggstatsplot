# context ------------------------------------------------------------
context(desc = "ggcorrmat")

# pearson's r without NAs ---------------------------------------------

testthat::test_that(
  desc = "checking ggcorrmat - without NAs - pearson's r",
  code = {
    # creating the plot
    set.seed(123)
    p <- ggstatsplot::ggcorrmat(
      data = iris,
      type = "p",
      title = "Iris dataset",
      subtitle = "By Edgar Anderson",
      sig.level = 0.001,
      p.adjust.method = "fdr",
      messages = FALSE
    )

    # checking data used to create a plot
    dat <- tibble::as_tibble(p$data) %>%
      dplyr::mutate_if(
        .tbl = .,
        .predicate = is.factor,
        .funs = ~ as.character(.)
      )

    # checking dimensions of data
    data_dims <- dim(dat)

    # testing everything is okay with data
    testthat::expect_equal(data_dims[1], 16L)
    testthat::expect_equal(data_dims[2], 7L)
    testthat::expect_equal(dat$coef[1], 1, tolerance = 1e-3)
    testthat::expect_equal(dat$coef[5], -0.12, tolerance = 1e-3)
    testthat::expect_equal(dat$coef[9], 0.87, tolerance = 1e-3)
    testthat::expect_equal(dat$coef[14], -0.37, tolerance = 1e-3)
    testthat::expect_equal(dat$Var1[3], "Petal.Length")
    testthat::expect_equal(dat$Var2[4], "Sepal.Length")
    testthat::expect_equal(dat$signif[1], 1L)
    testthat::expect_equal(dat$signif[2], 0L)
    testthat::expect_equal(dat$signif[4], 1L)
    testthat::expect_equal(dat$signif[5], 0L)

    # checking plot labels
    testthat::expect_identical(p$labels$title, "Iris dataset")
    testthat::expect_identical(p$labels$subtitle, "By Edgar Anderson")
    testthat::expect_identical(p$labels$caption, ggplot2::expr(atop(
      expr = paste(
        bold("X"),
        " = correlation non-significant at ",
        italic("p"),
        " < ",
        0.001,
        sep = ""
      ),
      " (Adjustment: Benjamini & Hochberg)"
    )))
    testthat::expect_null(p$labels$xlab, NULL)
    testthat::expect_null(p$labels$ylab, NULL)
  }
)

# robust r with NAs ---------------------------------------------------

testthat::test_that(
  desc = "checking ggcorrmat - with NAs - robust r",
  code = {
    # creating the plot
    set.seed(123)
    p <- ggstatsplot::ggcorrmat(
      data = ggplot2::msleep,
      type = "r",
      sig.level = 0.01,
      p.adjust.method = "hommel",
      caption.default = FALSE,
      messages = FALSE
    )

    # checking data used to create a plot
    dat <- tibble::as_tibble(p$data) %>%
      dplyr::mutate_if(
        .tbl = .,
        .predicate = is.factor,
        .funs = ~ as.character(.)
      )

    # checking dimensions of data
    data_dims <- dim(dat)

    # testing everything is okay with data
    testthat::expect_equal(data_dims[1], 36L)
    testthat::expect_equal(data_dims[2], 7L)
    testthat::expect_equal(dat$coef[2], 0.77, tolerance = 1e-3)
    testthat::expect_equal(dat$coef[7], 0.77, tolerance = 1e-3)
    testthat::expect_equal(dat$coef[9], -0.41, tolerance = 1e-3)
    testthat::expect_equal(dat$coef[36], 1.00, tolerance = 1e-3)
    testthat::expect_equal(dat$Var1[3], "sleep_cycle")
    testthat::expect_equal(dat$Var2[4], "sleep_total")
    testthat::expect_equal(dat$signif[1], 1L)
    testthat::expect_equal(dat$signif[9], 0L)
    testthat::expect_equal(dat$signif[14], 0L)
    testthat::expect_equal(dat$signif[15], 1L)

    # checking plot labels
    testthat::expect_null(p$labels$title, NULL)
    testthat::expect_null(p$labels$subtitle, NULL)
    testthat::expect_null(p$labels$caption, NULL)
    testthat::expect_null(p$labels$xlab, NULL)
    testthat::expect_null(p$labels$ylab, NULL)
  }
)


# spearman's rho with NAs ---------------------------------------------------

testthat::test_that(
  desc = "checking ggcorrmat - with NAs - spearman's rho",
  code = {
    # creating the plot
    set.seed(123)
    p <- ggstatsplot::ggcorrmat(
      data = ggplot2::msleep,
      cor.vars = sleep_total:awake,
      type = "np",
      sig.level = 0.01,
      p.adjust.method = "hommel",
      caption.default = FALSE,
      messages = FALSE
    )

    # checking data used to create a plot
    dat <- tibble::as_tibble(p$data) %>%
      dplyr::mutate_if(
        .tbl = .,
        .predicate = is.factor,
        .funs = ~ as.character(.)
      )

    # checking dimensions of data
    data_dims <- dim(dat)

    # testing everything is okay with data
    testthat::expect_equal(data_dims[1], 16L)
    testthat::expect_equal(data_dims[2], 7L)
    testthat::expect_equal(dat$coef[2], 0.76, tolerance = 1e-3)
    testthat::expect_equal(dat$coef[7], -0.33, tolerance = 1e-3)
    testthat::expect_equal(dat$coef[9], -0.49, tolerance = 1e-3)
    testthat::expect_equal(dat$coef[14], -0.76, tolerance = 1e-3)
    testthat::expect_equal(dat$Var1[15], "sleep_cycle")
    testthat::expect_equal(dat$Var2[16], "awake")
    testthat::expect_equal(dat$signif[1], 1L)
    testthat::expect_equal(dat$signif[7], 0L)
    testthat::expect_equal(dat$signif[10], 0L)
    testthat::expect_equal(dat$signif[11], 1L)
  }
)
