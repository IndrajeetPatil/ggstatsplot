context("ggcoefstats")

# t-statistic --------------------------------------------------

testthat::test_that(
  desc = "ggcoefstats with lm model",
  code = {
    set.seed(123)

    # model
    mod <- stats::lm(data = mtcars, formula = wt ~ mpg * am)

    # plot
    p <-
      ggstatsplot::ggcoefstats(
        x = mod,
        conf.level = 0.99,
        exclude.intercept = FALSE,
        k = 3
      )

    # plot build
    pb <- ggplot2::ggplot_build(p)

    # tidy dataframe from the function
    tidy_df <- p$plot_env$tidy_df

    # dataframe from `broom` package
    broom_df <- broom::tidy(
      x = mod,
      conf.int = TRUE,
      conf.level = 0.99
    )

    testthat::expect_equal(tidy_df$estimate, broom_df$estimate, tolerance = 1e-3)
    testthat::expect_equal(tidy_df$std.error, broom_df$std.error, tolerance = 1e-3)
    testthat::expect_equal(tidy_df$conf.low, broom_df$conf.low, tolerance = 1e-3)
    testthat::expect_equal(tidy_df$conf.high, broom_df$conf.high, tolerance = 1e-3)
    testthat::expect_equal(tidy_df$p.value, broom_df$p.value, tolerance = 1e-3)

    testthat::expect_identical(tidy_df$significance, c("***", "***", "*", "ns"))
    testthat::expect_identical(
      tidy_df$p.value.formatted,
      c("< 0.001", "< 0.001", "0.014", "0.064")
    )

    # checking panel parameters
    testthat::expect_equal(pb$layout$panel_params[[1]]$x.range,
      c(-4.292139, 8.302817),
      tolerance = 0.001
    )
    testthat::expect_identical(
      pb$layout$panel_params[[1]]$x.labels,
      c("-4", "0", "4", "8")
    )
    testthat::expect_equal(pb$layout$panel_params[[1]]$y.range,
      c(0.4, 4.6),
      tolerance = 0.001
    )
    testthat::expect_identical(
      pb$layout$panel_params[[1]]$y.labels,
      c("(Intercept)", "mpg", "am", "mpg:am")
    )
  }
)


# z-statistic --------------------------------------------------

testthat::test_that(
  desc = "ggcoefstats with glmer model",
  code = {
    library(lme4)

    # model
    set.seed(123)
    mod <-
      lme4::glmer(
        formula = cbind(incidence, size - incidence) ~ period + (1 | herd),
        data = cbpp,
        family = binomial
      )

    # plot
    set.seed(123)
    p <-
      ggstatsplot::ggcoefstats(
        x = mod,
        conf.level = 0.90,
        exclude.intercept = FALSE
      )

    # plot build
    pb <- ggplot2::ggplot_build(p)

    # tidy dataframe from the function
    tidy_df <- p$plot_env$tidy_df
    pb_df <- pb$data[[4]]

    # dataframe from `broom` package
    set.seed(123)
    broom_df <- broom.mixed::tidy(
      x = mod,
      conf.int = TRUE,
      conf.level = 0.90,
      effects = "fixed"
    )

    testthat::expect_equal(tidy_df$estimate, broom_df$estimate, tolerance = 1e-3)
    testthat::expect_equal(tidy_df$std.error, broom_df$std.error, tolerance = 1e-3)
    testthat::expect_equal(tidy_df$conf.low, broom_df$conf.low, tolerance = 1e-3)
    testthat::expect_equal(tidy_df$conf.high, broom_df$conf.high, tolerance = 1e-3)
    testthat::expect_equal(tidy_df$p.value, broom_df$p.value, tolerance = 1e-3)

    testthat::expect_identical(tidy_df$significance, c("***", "**", "***", "***"))
    testthat::expect_identical(
      tidy_df$statistic,
      trimws(as.character(format(broom_df$statistic, digits = 3)))
    )
    testthat::expect_identical(tidy_df$label, pb_df$label)

    # checking panel parameters
    testthat::expect_equal(pb$layout$panel_params[[1]]$x.range,
      c(-2.3876522, 0.1136977),
      tolerance = 0.001
    )
    testthat::expect_identical(
      pb$layout$panel_params[[1]]$x.labels,
      c("-2.0", "-1.5", "-1.0", "-0.5", "0.0")
    )
    testthat::expect_equal(pb$layout$panel_params[[1]]$y.range,
      c(0.4, 4.6),
      tolerance = 0.001
    )
    testthat::expect_identical(
      pb$layout$panel_params[[1]]$y.labels,
      c("(Intercept)", "period2", "period3", "period4")
    )
  }
)

# f-statistic - partial eta-squared -------------------------------------

testthat::test_that(
  desc = "ggcoefstats with aov model",
  code = {
    set.seed(123)

    # model
    mod <- stats::aov(data = mtcars, formula = wt ~ mpg * am)

    # plot
    p <-
      ggstatsplot::ggcoefstats(
        x = mod,
        exclude.intercept = FALSE,
        k = 2,
        ylab = "effect"
      )

    # plot build
    pb <- ggplot2::ggplot_build(p)

    # tidy dataframe from the function
    tidy_df <- p$plot_env$tidy_df

    testthat::expect_equal(tidy_df$estimate,
      c(0.8093822, 0.2068347, 0.1176152),
      tolerance = 1e-3
    )
    testthat::expect_equal(tidy_df$df1[1], 1L)
    testthat::expect_equal(tidy_df$df2[1], 28L)
    testthat::expect_equal(tidy_df$conf.low,
      c(0.64723930, 0.01103056, 0.00000000),
      tolerance = 1e-3
    )
    testthat::expect_equal(tidy_df$conf.high,
      c(0.8724070, 0.4306446, 0.3409322),
      tolerance = 1e-3
    )
    testthat::expect_equal(tidy_df$p.value,
      c(1.378306e-11, 1.156944e-02, 6.355055e-02),
      tolerance = 1e-5
    )

    testthat::expect_identical(p$labels$x, "partial eta-squared")
    testthat::expect_identical(p$labels$y, "effect")
    testthat::expect_identical(
      p$labels$caption,
      ggplot2::expr(paste(
        "AIC = ", "43", ", BIC = ", "50",
        ", log-likelihood = ", "-17"
      ))
    )
    testthat::expect_null(p$labels$title, NULL)
    testthat::expect_null(p$labels$subtitle, NULL)

    testthat::expect_identical(tidy_df$significance, c("***", "*", "ns"))
    testthat::expect_identical(
      tidy_df$p.value.formatted,
      c("< 0.001", "0.012", "0.064")
    )
    testthat::expect_identical(
      tidy_df$p.value.formatted2,
      c("<= 0.001", "==0.012", "==0.064")
    )
    testthat::expect_identical(
      tidy_df$label,
      c(
        "list(~italic(F)(1*\",\"*28)==118.89, ~italic(p)<= 0.001, ~italic(eta)[p]^2==0.81)",
        "list(~italic(F)(1*\",\"*28)==7.30, ~italic(p)==0.012, ~italic(eta)[p]^2==0.21)",
        "list(~italic(F)(1*\",\"*28)==3.73, ~italic(p)==0.064, ~italic(eta)[p]^2==0.12)"
      )
    )

    # checking panel parameters
    testthat::expect_equal(pb$layout$panel_params[[1]]$x.range,
      c(-0.04362035, 0.91602734),
      tolerance = 0.001
    )
    testthat::expect_identical(
      pb$layout$panel_params[[1]]$x.labels,
      c("0.00", "0.25", "0.50", "0.75")
    )
    testthat::expect_equal(pb$layout$panel_params[[1]]$y.range,
      c(0.4, 3.6),
      tolerance = 0.001
    )
    testthat::expect_identical(
      pb$layout$panel_params[[1]]$y.labels,
      c("mpg", "am", "mpg:am")
    )
  }
)

# f-statistic - partial omega-squared -------------------------------------

testthat::test_that(
  desc = "ggcoefstats with aov model",
  code = {
    set.seed(123)

    # model
    mod <- stats::aov(
      data = ggplot2::msleep,
      formula = sleep_rem ~ vore * brainwt,
      na.action = na.omit
    )

    # plot
    p <-
      ggstatsplot::ggcoefstats(
        x = mod,
        exclude.intercept = FALSE,
        sort = "ascending",
        effsize = "omega",
        partial = FALSE,
        title = "mammalian sleep",
        subtitle = "Source: `ggplot2` package",
        k = 3
      )

    # built plot
    pb <- ggplot2::ggplot_build(p)

    # tidy dataframe from the function
    tidy_df <- p$plot_env$tidy_df

    testthat::expect_identical(p$labels$x, "omega-squared")
    testthat::expect_identical(p$labels$y, "term")
    testthat::expect_identical(
      p$labels$caption,
      ggplot2::expr(paste(
        "AIC = ", "126", ", BIC = ", "142",
        ", log-likelihood = ", "-54"
      ))
    )
    testthat::expect_identical(p$labels$title, "mammalian sleep")
    testthat::expect_identical(p$labels$subtitle, "Source: `ggplot2` package")

    testthat::expect_equal(pb$data[[2]]$x, tidy_df$estimate, tolerance = 0.001)
    testthat::expect_equal(pb$data[[2]]$xmin, tidy_df$conf.low, tolerance = 0.001)
    testthat::expect_equal(pb$data[[2]]$xmax, tidy_df$conf.high, tolerance = 0.001)
    testthat::expect_equal(pb$data[[2]]$y, c(3L, 1L, 2L))

    testthat::expect_identical(tidy_df$label, pb$data[[4]]$label)

    testthat::expect_equal(tidy_df$estimate,
      c(0.26531121, 0.01431375, 0.12509318),
      tolerance = 1e-3
    )
    testthat::expect_equal(tidy_df$df1[1], 3L)
    testthat::expect_equal(tidy_df$df2[1], 35L)
    testthat::expect_equal(tidy_df$p.value,
      c(0.000584, 0.163, 0.0148),
      tolerance = 1e-3
    )

    testthat::expect_identical(tidy_df$significance, c("***", "ns", "*"))
    testthat::expect_identical(
      tidy_df$p.value.formatted,
      c("0.001", "0.163", "0.015")
    )
    testthat::expect_identical(
      tidy_df$p.value.formatted2,
      c("==0.001", "==0.163", "==0.015")
    )

    # checking panel parameters
    testthat::expect_equal(pb$layout$panel_params[[1]]$x.range,
      c(-0.02576079, 0.54097650),
      tolerance = 0.001
    )
    testthat::expect_identical(
      pb$layout$panel_params[[1]]$x.labels,
      c("0.0", "0.1", "0.2", "0.3", "0.4", "0.5")
    )
    testthat::expect_equal(pb$layout$panel_params[[1]]$y.range,
      c(0.4, 3.6),
      tolerance = 0.001
    )
    testthat::expect_identical(
      pb$layout$panel_params[[1]]$y.labels,
      c("brainwt", "vore:brainwt", "vore")
    )
  }
)

# f-statistic and eta- and omega-squared -------------------------------------

testthat::test_that(
  desc = "ggcoefstats with non-partial variants of effect size for f-statistic",
  code = {
    set.seed(123)

    # model
    mod <- stats::aov(
      data = ggplot2::msleep,
      formula = sleep_rem ~ brainwt * vore
    )

    # plot
    p1 <-
      ggstatsplot::ggcoefstats(
        x = mod,
        exclude.intercept = FALSE,
        k = 2,
        ylab = "effect",
        effsize = "eta",
        partial = FALSE
      )

    p2 <-
      ggstatsplot::ggcoefstats(
        x = mod,
        exclude.intercept = FALSE,
        k = 2,
        ylab = "effect",
        effsize = "omega",
        partial = FALSE
      )

    # plot build
    pb1 <- ggplot2::ggplot_build(p1)
    pb2 <- ggplot2::ggplot_build(p2)

    # checking label
    testthat::expect_identical(
      pb1$data[[4]]$label,
      c(
        "list(~italic(F)(1*\",\"*35)==3.72, ~italic(p)==0.062, ~italic(eta)^2==0.05)",
        "list(~italic(F)(3*\",\"*35)==6.83, ~italic(p)==0.001, ~italic(eta)^2==0.29)",
        "list(~italic(F)(3*\",\"*35)==4.01, ~italic(p)==0.015, ~italic(eta)^2==0.17)"
      )
    )

    testthat::expect_identical(
      pb2$data[[4]]$label,
      c(
        "list(~italic(F)(1*\",\"*35)==3.72, ~italic(p)==0.062, ~italic(omega)^2==0.04)",
        "list(~italic(F)(3*\",\"*35)==6.83, ~italic(p)==0.001, ~italic(omega)^2==0.24)",
        "list(~italic(F)(3*\",\"*35)==4.01, ~italic(p)==0.015, ~italic(omega)^2==0.13)"
      )
    )
  }
)

# dataframe as input ----------------------------------------------------

testthat::test_that(
  desc = "ggcoefstats works with data frames",
  code = {
    set.seed(123)

    # creating dataframe
    df1 <- tibble::tribble(
      ~term, ~statistic, ~estimate, ~conf.low, ~conf.high, ~p.value, ~df.residual,
      "level2", 0.158, 0.0665, -0.778, 0.911, 0.875, 5L,
      "level1", 1.33, 0.542, -0.280, 1.36, 0.191, 10L,
      "level3", 1.24, 0.045, 0.030, 0.65, 0.001, 12L
    )
    df2 <- dplyr::select(.data = df1, -p.value)
    df3 <- dplyr::select(.data = df1, -statistic)
    df4 <- dplyr::select(.data = df1, -df.residual)

    # plotting the dataframe
    p1 <- ggstatsplot::ggcoefstats(x = df1, statistic = "t", sort = "none")
    p2 <- ggstatsplot::ggcoefstats(x = df1, statistic = "z", sort = "descending")
    p3 <- ggstatsplot::ggcoefstats(x = df2, statistic = "t")
    p4 <- ggstatsplot::ggcoefstats(x = df3, statistic = "t") +
      ggplot2::scale_y_discrete(labels = c("x1", "x2", "x3")) +
      ggplot2::labs(x = "beta", y = NULL)
    p5 <- ggstatsplot::ggcoefstats(x = df4, statistic = "t")

    # build plots
    pb1 <- ggplot2::ggplot_build(p1)
    pb2 <- ggplot2::ggplot_build(p2)
    pb3 <- ggplot2::ggplot_build(p3)
    pb4 <- ggplot2::ggplot_build(p4)
    pb5 <- ggplot2::ggplot_build(p5)

    # stats labels
    testthat::expect_identical(
      pb1$data[[4]]$label,
      c(
        "list(~italic(beta)==0.07, ~italic(t)(5)==0.16, ~italic(p)==0.875)",
        "list(~italic(beta)==0.54, ~italic(t)(10)==1.33, ~italic(p)==0.191)",
        "list(~italic(beta)==0.04, ~italic(t)(12)==1.24, ~italic(p)==0.001)"
      )
    )
    testthat::expect_identical(
      pb5$data[[4]]$label,
      c(
        "list(~italic(beta)==0.07, ~italic(t)==0.16, ~italic(p)==0.875)",
        "list(~italic(beta)==0.54, ~italic(t)==1.33, ~italic(p)==0.191)",
        "list(~italic(beta)==0.04, ~italic(t)==1.24, ~italic(p)==0.001)"
      )
    )

    testthat::expect_equal(pb2$data[[3]]$y, c(2L, 1L, 3L))
    testthat::expect_identical(
      pb2$data[[4]]$label,
      c(
        "list(~italic(beta)==0.07, ~italic(z)==0.16, ~italic(p)==0.875)",
        "list(~italic(beta)==0.54, ~italic(z)==1.33, ~italic(p)==0.191)",
        "list(~italic(beta)==0.04, ~italic(z)==1.24, ~italic(p)==0.001)"
      )
    )

    # checking number of data layers
    testthat::expect_equal(length(pb1$data), 4L)
    testthat::expect_equal(length(pb2$data), 4L)
    testthat::expect_equal(length(pb3$data), 3L)
    testthat::expect_equal(length(pb4$data), 3L)

    # confidence intervals used for each layer should be the same as df
    testthat::expect_equal(pb3$data[[2]]$xmin, df1$conf.low)
    testthat::expect_equal(pb3$data[[2]]$xmax, df2$conf.high)
    testthat::expect_equal(pb2$data[[2]]$xmin, df3$conf.low)
    testthat::expect_equal(pb2$data[[2]]$xmax, df3$conf.high)

    # checking labels (order)
    testthat::expect_identical(
      pb1$layout$panel_params[[1]]$y.labels,
      c("level2", "level1", "level3")
    )

    testthat::expect_identical(
      pb2$layout$panel_params[[1]]$y.labels,
      c("level1", "level2", "level3")
    )

    testthat::expect_identical(
      pb4$layout$panel_params[[1]]$y.labels,
      c("x1", "x2", "x3")
    )

    # annotations
    testthat::expect_identical(p4$labels$x, "beta")
    testthat::expect_null(p4$labels$y, NULL)
    testthat::expect_null(p4$labels$title, NULL)
    testthat::expect_null(p4$labels$subtitle, NULL)
    testthat::expect_null(p4$labels$caption, NULL)
  }
)

# check confidence intervals ----------------------------------------------

testthat::test_that(
  desc = "check computing confidence intervals",
  code = {
    set.seed(123)

    # creating broom dataframes
    mod <- stats::lm(data = iris, formula = Sepal.Length ~ Species)
    df1 <- broom::tidy(
      x = mod,
      conf.int = TRUE,
      conf.level = 0.95
    )
    df2 <- broom::tidy(
      x = mod,
      conf.int = TRUE,
      conf.level = 0.50
    )

    # computed dataframes
    tidy_df1 <-
      ggstatsplot::ggcoefstats(
        x = dplyr::select(df1, -conf.low, -conf.high),
        exclude.intercept = FALSE,
        statistic = "t",
        output = "tidy"
      )
    tidy_df2 <-
      ggstatsplot::ggcoefstats(
        x = dplyr::select(df2, -conf.low, -conf.high),
        exclude.intercept = FALSE,
        statistic = "t",
        output = "tidy",
        conf.level = 0.50
      )

    # checking confidence intervals
    testthat::expect_equal(df1$conf.low, tidy_df1$conf.low, tolerance = 0.001)
    testthat::expect_equal(df2$conf.high, tidy_df2$conf.high, tolerance = 0.001)
  }
)

# unsupported model objects -----------------------------------------------

testthat::test_that(
  desc = "unsupported model objects",
  code = {
    set.seed(123)
    testthat::expect_error(
      ggstatsplot::ggcoefstats(x = stats::kmeans(
        x = dplyr::select(iris, -Species),
        centers = 3
      ))
    )
  }
)
