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
  }
)


# z-statistic --------------------------------------------------

testthat::test_that(
  desc = "ggcoefstats with glmer model",
  code = {
    set.seed(123)
    library(lme4)

    # model
    mod <-
      lme4::glmer(
        formula = cbind(incidence, size - incidence) ~ period + (1 | herd),
        data = cbpp,
        family = binomial
      )

    # plot
    p <-
      ggstatsplot::ggcoefstats(
        x = mod,
        conf.level = 0.90,
        exclude.intercept = FALSE
      )

    # tidy dataframe from the function
    tidy_df <- p$plot_env$tidy_df

    # dataframe from `broom` package
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
      as.character(round(broom_df$statistic, 2))
    )
    testthat::expect_identical(
      tidy_df$label,
      c(
        "list(~italic(beta)==-1.40, ~italic(z)==-6.05, ~italic(p)<= 0.001)",
        "list(~italic(beta)==-0.99, ~italic(z)==-3.27, ~italic(p)==0.001)",
        "list(~italic(beta)==-1.13, ~italic(z)==-3.49, ~italic(p)<= 0.001)",
        "list(~italic(beta)==-1.58, ~italic(z)==-3.74, ~italic(p)<= 0.001)"
      )
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
  }
)
