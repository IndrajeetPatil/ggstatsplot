# context ----------------------------------------------------------------
context(desc = "ggcorrmat")

# cor.vars works with different methods of inputs ------------------------

testthat::test_that(
  desc = "cor.vars works with different methods of inputs",
  code = {
    testthat::skip_on_cran()

    set.seed(123)

    # method 1
    df1 <- ggstatsplot::ggcorrmat(
      data = ggplot2::msleep,
      cor.vars = "sleep_total":"bodywt",
      p.adjust.method = "BH",
      output = "ci"
    )

    # method 2
    df2 <- ggstatsplot::ggcorrmat(
      data = ggplot2::msleep,
      cor.vars = c("sleep_total":"bodywt"),
      p.adjust.method = "BH",
      output = "ci"
    )

    # method 3
    df3 <- ggstatsplot::ggcorrmat(
      data = ggplot2::msleep,
      cor.vars = sleep_total:bodywt,
      p.adjust.method = "BH",
      output = "ci"
    )

    # method 4
    df4 <- ggstatsplot::ggcorrmat(
      data = ggplot2::msleep,
      cor.vars = c(sleep_total:bodywt),
      p.adjust.method = "BH",
      output = "ci"
    )

    # check dimensions
    testthat::expect_equal(dim(df1), c(15L, 7L))
    testthat::expect_equal(dim(df2), c(15L, 7L))
    testthat::expect_equal(dim(df3), c(15L, 7L))
    testthat::expect_equal(dim(df4), c(15L, 7L))
  }
)

# pearson's r without NAs ------------------------------------------------

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
      digits = 4,
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
        1.0000,
        -0.1176,
        0.8718,
        0.8179,
        -0.1176,
        1.0000,
        -0.4284,
        -0.3661,
        0.8718,
        -0.4284,
        1.0000,
        0.9629,
        0.8179,
        -0.3661,
        0.9629,
        1.0000
      ),
      tolerance = 1e-3
    )
    testthat::expect_equal(dat$Var1[3], "Petal.Length")
    testthat::expect_equal(dat$Var2[4], "Sepal.Length")
    testthat::expect_equal(dat$signif[1], 1L)
    testthat::expect_equal(dat$signif[2], 0L)
    testthat::expect_equal(dat$signif[4], 1L)
    testthat::expect_equal(dat$signif[5], 0L)
    testthat::expect_identical(
      p$plot_env$colors,
      c("#1B9E77", "#D95F02", "#7570B3")
    )

    # checking layers
    testthat::expect_equal(pb$plot$layers[[3]]$aes_params$shape, 4L)
    testthat::expect_equal(pb$plot$layers[[3]]$aes_params$size, 14L)
    testthat::expect_identical(pb$plot$layers[[3]]$aes_params$colour, "white")

    # checking plot labels
    testthat::expect_identical(p$labels$title, "Iris dataset")
    testthat::expect_identical(p$labels$subtitle, "By Edgar Anderson")
    testthat::expect_identical(p_legend_title, ggplot2::expr(atop(
      atop(scriptstyle(bold("sample size:")), italic(n) ~
      "=" ~ 150L),
      atop(
        scriptstyle(bold("correlation:")),
        "Pearson"
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
      "Adjustment (p-value): Benjamini & Hochberg"
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

    # checking if correlation variable names are being read properly
    set.seed(123)
    df2 <- ggstatsplot::ggcorrmat(
      data = ggplot2::msleep,
      cor.vars = sleep_total:awake,
      cor.vars.names = c("sleep (total)", "sleep (REM)", "sleep (cycle)", "awake"),
      type = "r",
      output = "p",
      p.adjust.method = "fdr",
      messages = FALSE
    )

    testthat::expect_identical(
      df2$variable,
      c("sleep (total)", "sleep (REM)", "sleep (cycle)", "awake")
    )
  }
)

# robust r with NAs ---------------------------------------------------

testthat::test_that(
  desc = "checking ggcorrmat - with NAs - robust r",
  code = {
    testthat::skip_on_cran()

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
    testthat::expect_identical(
      p$plot_env$colors,
      c("#E69F00", "white", "#009E73")
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
        "robust (% bend)"
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
    testthat::skip_on_cran()

    # creating the plot
    set.seed(123)
    p <- ggstatsplot::ggcorrmat(
      data = ggplot2::msleep,
      cor.vars = sleep_total:awake,
      type = "np",
      sig.level = 0.01,
      p.adjust.method = "hommel",
      caption.default = FALSE,
      messages = FALSE,
      colors = NULL,
      package = "wesanderson",
      palette = "Rushmore1"
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
    testthat::expect_identical(
      p$plot_env$colors,
      c("#E1BD6D", "#EABE94", "#0B775E")
    )
    testthat::expect_identical(p_legend_title, ggplot2::expr(atop(atop(
      atop(scriptstyle(bold("sample size:")), italic(n)[min] ~
      "=" ~ 32),
      atop(
        italic(n)[median] ~ "=" ~ 61,
        italic(n)[max] ~ "=" ~ 83
      )
    ), atop(
      scriptstyle(
        bold("correlation:")
      ),
      "Spearman"
    ))))
  }
)

# Kendall tau with NAs + ggplot modification ----------------------------------

testthat::test_that(
  desc = "checking ggcorrmat - with NAs - Kendall tau",
  code = {
    testthat::skip_on_cran()

    # creating the plot
    set.seed(123)
    p <- ggstatsplot::ggcorrmat(
      data = ggplot2::msleep,
      corr.method = "kendall",
      matrix.type = "lower",
      ggtheme = ggplot2::theme_classic()
    ) +
      ggplot2::scale_y_discrete(position = "right")

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
    testthat::expect_equal(dat$coef[2], -0.35, tolerance = 1e-3)
    testthat::expect_equal(dat$coef[7], -0.59, tolerance = 1e-3)
    testthat::expect_equal(dat$Var1[15], "bodywt")
    testthat::expect_equal(dat$Var2[10], "sleep_cycle")
    testthat::expect_equal(dat$signif[1], 1L)
    testthat::expect_equal(dat$signif[2], 0L)
    testthat::expect_equal(dat$signif[7], 1L)
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
        "Kendall"
      )
    )))

    # checking layers
    testthat::expect_equal(pb$plot$layers[[3]]$aes_params$shape, 4L)
    testthat::expect_equal(pb$plot$layers[[3]]$aes_params$size, 11L)
    testthat::expect_identical(pb$plot$layers[[3]]$aes_params$colour, "black")

    # checking panel parameters
    testthat::expect_equal(pb$layout$panel_params[[1]]$x.range,
      c(0.4, 5.6),
      tolerance = 0.01
    )
    testthat::expect_equal(pb$layout$panel_params[[1]]$y.range,
      c(0.4, 5.6),
      tolerance = 0.01
    )
    testthat::expect_identical(
      pb$layout$panel_params[[1]]$x.arrange,
      c("secondary", "primary")
    )
    testthat::expect_identical(
      pb$layout$panel_params[[1]]$y.arrange,
      c("secondary", "primary")
    )
    testthat::expect_identical(
      pb$layout$panel_params[[1]]$x.labels,
      c("sleep_rem", "sleep_cycle", "awake", "brainwt", "bodywt")
    )
    testthat::expect_identical(
      pb$layout$panel_params[[1]]$y.labels,
      c("sleep_total", "sleep_rem", "sleep_cycle", "awake", "brainwt")
    )
  }
)


# checking sample sizes ---------------------------------------------------

testthat::test_that(
  desc = "checking sample sizes",
  code = {
    testthat::skip_on_cran()

    # dataframe with sample sizes
    set.seed(123)
    df <- ggstatsplot::ggcorrmat(
      data = ggplot2::msleep,
      cor.vars = sleep_total:awake,
      type = "r",
      return = "n",
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
    testthat::skip_on_cran()

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
    testthat::skip_on_cran()

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
    testthat::expect_identical(
      df$pair,
      c(
        "sleep_total-sleep_rem",
        "sleep_total-sleep_cycle",
        "sleep_total-awake",
        "sleep_rem-sleep_cycle",
        "sleep_rem-awake",
        "sleep_cycle-awake"
      )
    )
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
    testthat::skip_on_cran()

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
      "No. of variable names doesn't equal"
    )
    testthat::expect_match(
      p_message1[2],
      "Confidence intervals not supported"
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
