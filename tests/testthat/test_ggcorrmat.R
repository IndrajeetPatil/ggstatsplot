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
      colors = NULL,
      lab.col = "white",
      pch.col = "white",
      pch.cex = 14,
      messages = TRUE
    )

    # checking legend title
    pb <- ggplot2::ggplot_build(p)
    p_legend_title <- pb$plot$plot_env$legend.title

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
    testthat::expect_equal(data_dims, c(16L, 7L))
    testthat::expect_equal(dim(pb$data[[1]]), c(16L, 15L))
    testthat::expect_equal(length(pb$data), 3L)
    testthat::expect_equal(
      dat$coef,
      c(
        1.00,
        -0.12,
        0.87,
        0.82,
        -0.12,
        1.00,
        -0.43,
        -0.37,
        0.87,
        -0.43,
        1.00,
        0.96,
        0.82,
        -0.37,
        0.96,
        1.00
      ),
      tolerance = 1e-3
    )
    testthat::expect_equal(dat$Var1[3], "Petal.Length")
    testthat::expect_equal(dat$Var2[4], "Sepal.Length")
    testthat::expect_equal(dat$signif[1], 1L)
    testthat::expect_equal(dat$signif[2], 0L)
    testthat::expect_equal(dat$signif[4], 1L)
    testthat::expect_equal(dat$signif[5], 0L)

    # checking plot labels
    testthat::expect_identical(p$labels$title, "Iris dataset")
    testthat::expect_identical(p$labels$subtitle, "By Edgar Anderson")
    testthat::expect_identical(p_legend_title, ggplot2::expr(atop(
      atop(scriptstyle(bold("sample size:")), italic(n) ~
      "=" ~ 150L),
      atop(
        scriptstyle(bold("correlation:")),
        "pearson"
      )
    )))
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

    # checking layer data
    testthat::expect_equal(dim(pb$data[[1]]), c(16L, 15L))
    testthat::expect_equal(dim(pb$data[[2]]), c(16L, 15L))
    testthat::expect_equal(dim(pb$data[[3]]), c(2L, 10L))

    # check if number of cells is equal to number of correlations
    testthat::expect_equal(
      length(unique(dat$Var1))^2,
      max(pb$data[[1]]$group)
    )

    # checking unique number of correlations
    testthat::expect_equal(length(unique(pb$data[[1]]$fill)), 7L)

    # checking if aesthetic modifications worked
    testthat::expect_equal(pb$data[[3]]$shape[1], 4L)
    testthat::expect_equal(pb$data[[3]]$size[1], 14L)
    testthat::expect_identical(pb$data[[2]]$colour[1], "white")
    testthat::expect_identical(pb$data[[3]]$colour[1], "white")
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
      matrix.type = "upper",
      caption.default = FALSE,
      messages = FALSE
    )

    # checking legend title
    pb <- ggplot2::ggplot_build(p)
    p_legend_title <- pb$plot$plot_env$legend.title

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
    testthat::expect_equal(data_dims, c(15L, 7L))
    testthat::expect_equal(dim(pb$data[[1]]), c(15L, 15L))
    testthat::expect_equal(dim(pb$data[[2]]), c(15L, 15L))
    testthat::expect_equal(dim(pb$data[[3]]), c(1L, 10L))
    testthat::expect_equal(length(pb$data), 3L)
    testthat::expect_equal(
      dat$coef,
      c(
        0.77,
        -0.52,
        -0.41,
        -1.00,
        -0.77,
        0.52,
        -0.57,
        -0.40,
        0.88,
        0.57,
        -0.53,
        -0.42,
        0.73,
        0.53,
        0.87
      ),
      tolerance = 1e-3
    )
    testthat::expect_equal(dat$Var1[3], "sleep_rem")
    testthat::expect_equal(dat$Var2[4], "awake")
    testthat::expect_equal(
      dat$signif,
      c(1, 1, 0, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1)
    )

    # checking plot labels
    testthat::expect_null(p$labels$title, NULL)
    testthat::expect_null(p$labels$subtitle, NULL)
    testthat::expect_null(p$labels$caption, NULL)
    testthat::expect_null(p$labels$xlab, NULL)
    testthat::expect_null(p$labels$ylab, NULL)
    testthat::expect_identical(p_legend_title, ggplot2::expr(atop(
      atop(
        atop(scriptstyle(bold("sample size:")), italic(n)[min] ~
        "=" ~ 30),
        atop(
          italic(n)[median] ~ "=" ~ 56,
          italic(n)[max] ~ "=" ~ 83
        )
      ), atop(
        scriptstyle(bold("correlation:")),
        "robust"
      )
    )))

    # check if number of cells is *not* equal to number of correlations
    testthat::expect_false(
      length(unique(dat$Var1))^2 == max(pb$data[[1]]$group)
    )
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
    testthat::expect_equal(data_dims, c(16L, 7L))
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

# checking sample sizes ---------------------------------------------------

testthat::test_that(
  desc = "checking sample sizes",
  code = {
    # dataframe with sample sizes
    set.seed(123)
    df <- ggstatsplot::ggcorrmat(
      data = ggplot2::msleep,
      cor.vars = sleep_total:awake,
      type = "r",
      output = "n",
      p.adjust.method = "fdr",
      messages = FALSE
    )

    testthat::expect_identical(
      df$variable,
      c("sleep_total", "sleep_rem", "sleep_cycle", "awake")
    )
    testthat::expect_equal(df$sleep_total, c(83L, 61L, 32L, 83L))
    testthat::expect_equal(df$sleep_rem, c(61L, 61L, 32L, 61L))
    testthat::expect_equal(df$sleep_cycle, c(32L, 32L, 32L, 32L))
    testthat::expect_equal(df$awake, c(83L, 61L, 32L, 83L))
    testthat::expect_true(inherits(df, what = "tbl_df"))
  }
)

# checking p-values ---------------------------------------------------

testthat::test_that(
  desc = "checking p-values",
  code = {
    # dataframe with sample sizes
    set.seed(123)
    df <- ggstatsplot::ggcorrmat(
      data = ggplot2::msleep,
      cor.vars = sleep_total:awake,
      type = "np",
      output = "p",
      p.adjust.method = "none",
      messages = FALSE
    )

    testthat::expect_equal(df$sleep_cycle[1], 0.00453, tolerance = 0.001)
    testthat::expect_equal(df$sleep_cycle[2], 0.0614, tolerance = 0.001)
    testthat::expect_true(inherits(df, what = "tbl_df"))
  }
)

# checking confidence intervals ----------------------------------------------

testthat::test_that(
  desc = "checking confidence intervals",
  code = {
    # dataframe with sample sizes
    set.seed(123)
    df <- ggstatsplot::ggcorrmat(
      data = ggplot2::msleep,
      cor.vars = sleep_total:awake,
      type = "p",
      output = "ci",
      p.adjust.method = "none",
      messages = FALSE
    )

    testthat::expect_true(inherits(df, what = "tbl_df"))
    testthat::expect_equal(
      df$r,
      c(
        0.7517550,
        -0.4737127,
        -0.9999986,
        -0.3381235,
        -0.7517713,
        0.4737127
      ),
      tolerance = 0.001
    )
    testthat::expect_equal(
      df$lower,
      c(
        0.6166756,
        -0.7058189,
        -0.9999991,
        -0.6143809,
        -0.8438428,
        0.1497554
      ),
      tolerance = 0.001
    )
    testthat::expect_equal(
      df$upper,
      c(
        0.84383201,
        -0.14975542,
        -0.99999779,
        0.01198335,
        -0.61669876,
        0.70581894
      ),
      tolerance = 0.001
    )
  }
)

# checking messages -------------------------------------------------------

testthat::test_that(
  desc = "checking messages",
  code = {
    # capturing the message
    set.seed(123)
    p_message1 <- capture.output(ggstatsplot::ggcorrmat(
      data = ggplot2::msleep,
      cor.vars = sleep_total:awake,
      type = "r",
      output = "ci",
      cor.vars.names = "awake",
      sig.level = 0.01,
      p.adjust.method = "hommel",
      caption.default = FALSE,
      messages = TRUE
    ))

    p_message2 <- capture.output(ggstatsplot::ggcorrmat(
      data = ggplot2::msleep,
      cor.vars = sleep_total:awake,
      output = "p",
      p.adjust.method = "hommel",
      caption.default = FALSE,
      messages = TRUE
    ))

    # checking messages
    testthat::expect_match(
      p_message1[1],
      "Number of variable names does not equal"
    )
    testthat::expect_match(
      p_message1[2],
      "Confidence intervals currently not supported"
    )
    testthat::expect_match(
      p_message2[2],
      "the upper triangle: p-values adjusted for multiple comparisons"
    )
    testthat::expect_match(
      p_message2[3],
      "the lower triangle: unadjusted p-values"
    )
  }
)
