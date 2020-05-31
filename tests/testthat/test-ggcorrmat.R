
# cor.vars works with different methods of inputs ------------------------

testthat::test_that(
  desc = "cor.vars works with different methods of inputs",
  code = {
    testthat::skip_on_cran()

    # method 1
    set.seed(123)
    df1 <-
      ggstatsplot::ggcorrmat(
        data = ggplot2::msleep,
        type = "p",
        cor.vars = "sleep_total":"awake",
        cor.vars.names = c("1", "2", "3", "4"),
        p.adjust.method = "BH",
        output = "aaa"
      )

    # method 2
    set.seed(123)
    df2 <-
      suppressWarnings(ggstatsplot::ggcorrmat(
        data = ggplot2::msleep,
        type = "np",
        cor.vars = c("sleep_total":"awake"),
        p.adjust.method = "none",
        output = "bbbb"
      ))

    # method 3
    set.seed(123)
    df3 <-
      ggstatsplot::ggcorrmat(
        data = ggplot2::msleep,
        type = "r",
        cor.vars = sleep_total:awake,
        p.adjust.method = "holm",
        output = "ci"
      )

    # method 4
    set.seed(123)
    df4 <-
      suppressWarnings(ggstatsplot::ggcorrmat(
        data = ggplot2::msleep,
        type = "bf",
        cor.vars = c(sleep_total:awake),
        p.adjust.method = "fdr",
        output = "hdhfhfh"
      ))

    # check dataframes
    testthat::expect_equal(dim(df1), c(6L, 10L))
    testthat::expect_equal(dim(df2), c(6L, 9L))
    testthat::expect_equal(dim(df3), c(6L, 10L))
    testthat::expect_equal(dim(df4), c(6L, 12L))

    testthat::expect_identical(unique(df1$parameter1), c("1", "2", "3"))
  }
)

# pearson's r without NAs ------------------------------------------------

testthat::test_that(
  desc = "checking ggcorrmat - without NAs - pearson's r",
  code = {
    testthat::skip_on_cran()

    # creating the plot
    set.seed(123)
    p <-
      ggstatsplot::ggcorrmat(
        data = iris,
        type = "p",
        title = "Iris dataset",
        subtitle = "By Edgar Anderson",
        ggstatsplot.layer = FALSE,
        sig.level = 0.001,
        p.adjust.method = "fdr",
        colors = NULL,
        k = 4,
        ggcorrplot.args = list(
          lab_col = "white",
          pch.col = "white",
          pch.cex = 14
        ),
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
      unclass(p$plot_env$colors),
      c("#1B9E77FF", "#D95F02FF", "#7570B3FF")
    )

    # checking layers
    testthat::expect_equal(pb$plot$layers[[3]]$aes_params$shape, "cross")
    testthat::expect_equal(pb$plot$layers[[3]]$aes_params$size, 14L)
    testthat::expect_identical(pb$plot$layers[[3]]$aes_params$colour, "white")

    # checking plot labels
    testthat::expect_identical(p$labels$title, "Iris dataset")
    testthat::expect_identical(p$labels$subtitle, "By Edgar Anderson")
    testthat::expect_identical(
      p_legend_title,
      ggplot2::expr(atop(
        atop(scriptstyle(bold("sample size:")), italic(n) ~
        "=" ~ 150L),
        atop(
          scriptstyle(bold("correlation:")),
          "Pearson"
        )
      ))
    )
    testthat::expect_identical(
      p$labels$caption,
      ggplot2::expr(atop(
        displaystyle(NULL),
        expr = paste(
          bold("X"),
          " = non-significant at ",
          italic("p"),
          " < ",
          0.001,
          " (Adjustment: ",
          "Benjamini & Hochberg",
          ")"
        )
      ))
    )
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
    testthat::expect_equal(pb$data[[3]]$shape[1], "cross")
    testthat::expect_equal(pb$data[[3]]$size[1], 14L)
    testthat::expect_identical(pb$data[[2]]$colour[1], "white")
    testthat::expect_identical(pb$data[[3]]$colour[1], "white")
  }
)

# robust r with NAs ---------------------------------------------------

testthat::test_that(
  desc = "checking ggcorrmat - with NAs - robust r",
  code = {
    testthat::skip_on_cran()

    # creating the plot
    set.seed(123)
    p <-
      ggstatsplot::ggcorrmat(
        data = ggplot2::msleep,
        type = "r",
        sig.level = 0.01,
        p.adjust.method = "hommel",
        matrix.type = "upper",
        messages = FALSE
      ) +
      labs(caption = NULL)

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
      c(1, 1, 0, 1, 1, 1, 1, 0, 1, 1, 1, 1, 1, 1, 1)
    )
    testthat::expect_identical(
      p$plot_env$colors,
      c("#E69F00", "white", "#009E73")
    )

    # checking plot labels
    testthat::expect_identical(
      p_legend_title,
      ggplot2::expr(atop(atop(
        atop(scriptstyle(bold("sample size:")), italic(n)[min] ~
        "=" ~ 30L),
        atop(
          italic(n)[mode] ~ "=" ~ 32L,
          italic(n)[max] ~ "=" ~ 83L
        )
      ), atop(
        scriptstyle(
          bold("correlation:")
        ),
        "robust (% bend)"
      )))
    )

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
    p <-
      suppressWarnings(ggstatsplot::ggcorrmat(
        data = ggplot2::msleep,
        cor.vars = sleep_total:awake,
        cor.vars.names = "sleep_total",
        type = "np",
        sig.level = 0.01,
        p.adjust.method = "hommel",
        caption.default = FALSE,
        messages = FALSE,
        colors = NULL,
        package = "wesanderson",
        palette = "Rushmore1"
      ))

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

    # checking aesthetics
    testthat::expect_identical(
      unique(pb$data[[1]]$fill),
      c("#0B775E", "#57896B", "#E6BE81", "#E1BD6D", "#E7BE87", "#E3BD77", "#8E9C79")
    )

    testthat::expect_identical(
      p_legend_title,
      ggplot2::expr(atop(atop(
        atop(scriptstyle(bold("sample size:")), italic(n)[min] ~
        "=" ~ 32L),
        atop(
          italic(n)[mode] ~ "=" ~ 32L,
          italic(n)[max] ~ "=" ~ 83L
        )
      ), atop(
        scriptstyle(
          bold("correlation:")
        ),
        "Spearman"
      )))
    )
  }
)
