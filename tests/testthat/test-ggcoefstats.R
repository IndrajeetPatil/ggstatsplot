# z-statistic --------------------------------------------------

testthat::test_that(
  desc = "ggcoefstats with glmer model",
  code = {
    testthat::skip_on_cran()
    library(lme4)

    # model
    set.seed(123)
    mod <-
      lme4::glmer(
        formula = cbind(incidence, size - incidence) ~ period + (1 | herd),
        data = lme4::cbpp,
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
    testthat::expect_equal(
      pb$data[[4]],
      structure(
        list(
          x = c(
            -1.39834286447115,
            -0.991924974975739,
            -1.12821621594334,
            -1.57974541364919
          ),
          y = 1:4,
          label = c(
            "list(~widehat(italic(beta))==-1.40, ~italic(z)==-6.05, ~italic(p)<= 0.001)",
            "list(~widehat(italic(beta))==-0.99, ~italic(z)==-3.27, ~italic(p)== 0.001)",
            "list(~widehat(italic(beta))==-1.13, ~italic(z)==-3.49, ~italic(p)<= 0.001)",
            "list(~widehat(italic(beta))==-1.58, ~italic(z)==-3.74, ~italic(p)<= 0.001)"
          ),
          PANEL = structure(c(1L, 1L, 1L, 1L), class = "factor", .Label = "1"),
          group = structure(1:4, n = 4L),
          colour = structure(c(
            "#1B9E77FF", "#D95F02FF", "#7570B3FF", "#E7298AFF"
          ), class = "colors"),
          fill = c("white", "white", "white", "white"),
          size = c(3, 3, 3, 3),
          angle = c(0, 0, 0, 0),
          alpha = c(NA, NA, NA, NA),
          family = c("", "", "", ""),
          fontface = c(1, 1, 1, 1),
          lineheight = c(1.2, 1.2, 1.2, 1.2),
          hjust = c(0.5, 0.5, 0.5, 0.5),
          vjust = c(0.5, 0.5, 0.5, 0.5)
        ),
        row.names = c(NA, -4L),
        class = "data.frame"
      )
    )
  }
)

# t-statistic --------------------------------------------------

testthat::test_that(
  desc = "ggcoefstats with lm model",
  code = {
    testthat::skip_on_cran()
    set.seed(123)

    # model
    mod <- stats::lm(data = mtcars, formula = wt ~ mpg * am)

    # plot
    p <-
      ggstatsplot::ggcoefstats(
        x = mod,
        conf.level = 0.99,
        exclude.intercept = FALSE,
        only.significant = TRUE,
        k = 3
      )

    # plot build
    pb <- ggplot2::ggplot_build(p)

    # checking panel parameters
    testthat::expect_equal(
      pb$layout$panel_params[[1]]$y$breaks,
      structure(c("(Intercept)", "mpg", "am", "mpg:am"), pos = 1:4)
    )

    # checking layered data
    testthat::expect_equal(dim(pb$data[[1]]), c(1L, 7L))
    testthat::expect_equal(dim(pb$data[[2]]), c(4L, 13L))
    testthat::expect_equal(dim(pb$data[[3]]), c(4L, 10L))

    # checking ggrepel label layer
    testthat::expect_identical(
      pb$data[[4]]$label,
      c(
        "list(~widehat(italic(beta))==6.438, ~italic(t)(28)==13.765, ~italic(p)<= 0.001)",
        "list(~widehat(italic(beta))==-0.156, ~italic(t)(28)==-5.840, ~italic(p)<= 0.001)",
        "list(~widehat(italic(beta))==-1.809, ~italic(t)(28)==-2.615, ~italic(p)== 0.014)",
        NA_character_
      )
    )
    testthat::expect_identical(
      unclass(pb$data[[4]]$colour),
      c("#1B9E77FF", "#D95F02FF", "#7570B3FF", "#E7298AFF")
    )

    # testing specific geoms
    testthat::expect_equal(
      pb$data[[1]],
      structure(
        list(
          xintercept = 0,
          PANEL = structure(1L, .Label = "1", class = "factor"),
          group = -1L,
          colour = "black",
          size = 1,
          linetype = "dashed",
          alpha = NA
        ),
        row.names = c(NA, -1L),
        class = "data.frame"
      )
    )
  }
)


# f-statistic and partial eta- and omega-squared -----------------------------

testthat::test_that(
  desc = "ggcoefstats with partial variants of effect size for f-statistic",
  code = {
    testthat::skip_on_cran()

    ## partial eta-squared

    set.seed(123)

    # model
    mod <- stats::aov(data = mtcars, formula = wt ~ mpg * am)

    # plot
    p <-
      ggstatsplot::ggcoefstats(
        x = mod,
        exclude.intercept = FALSE,
        effsize = "eta",
        partial = TRUE,
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
      ggplot2::expr(atop(
        displaystyle(NULL),
        expr = paste(
          "AIC = ",
          "43",
          ", BIC = ",
          "50"
        )
      ))
    )
    testthat::expect_null(p$labels$title, NULL)
    testthat::expect_null(p$labels$subtitle, NULL)

    testthat::expect_identical(tidy_df$significance, c("***", "*", "ns"))
    testthat::expect_identical(
      tidy_df$p.value.formatted,
      c("<= 0.001", "== 0.012", "== 0.064")
    )
    testthat::expect_identical(
      tidy_df$label,
      c(
        "list(~italic(F)(1*\",\"*28)==118.89, ~italic(p)<= 0.001, ~widehat(italic(eta)[p]^2)==0.81)",
        "list(~italic(F)(1*\",\"*28)==7.30, ~italic(p)== 0.012, ~widehat(italic(eta)[p]^2)==0.21)",
        "list(~italic(F)(1*\",\"*28)==3.73, ~italic(p)== 0.064, ~widehat(italic(eta)[p]^2)==0.12)"
      )
    )

    ## partial omega-squared

    set.seed(123)

    # model
    mod <-
      stats::aov(
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
        partial = TRUE,
        title = "mammalian sleep",
        subtitle = "Source: `ggplot2` package",
        caption = substitute(paste(italic("Note"), ": From `tidyverse`")),
        package = "wesanderson",
        palette = "BottleRocket2",
        caption.summary = FALSE,
        k = 3
      )

    # built plot
    pb <- ggplot2::ggplot_build(p)

    # tidy dataframe from the function
    tidy_df <- p$plot_env$tidy_df

    # tests
    testthat::expect_identical(p$labels$x, "partial omega-squared")
    testthat::expect_identical(p$labels$y, "term")
    testthat::expect_identical(
      p$labels$caption,
      ggplot2::expr(paste(italic("Note"), ": From `tidyverse`"))
    )
    testthat::expect_identical(p$labels$title, "mammalian sleep")
    testthat::expect_identical(p$labels$subtitle, "Source: `ggplot2` package")

    testthat::expect_equal(pb$data[[2]]$x, tidy_df$estimate, tolerance = 0.001)
    testthat::expect_equal(pb$data[[2]]$xmin, tidy_df$conf.low, tolerance = 0.001)
    testthat::expect_equal(pb$data[[2]]$xmax, tidy_df$conf.high, tolerance = 0.001)
    testthat::expect_equal(pb$data[[2]]$y, c(3L, 1L, 2L))

    testthat::expect_identical(tidy_df$label, pb$data[[4]]$label)

    testthat::expect_equal(tidy_df$estimate,
      c(0.30828881, 0.02348073, 0.17365008),
      tolerance = 0.001
    )
    testthat::expect_equal(tidy_df$df1[1], 3L)
    testthat::expect_equal(tidy_df$df2[1], 35L)
    testthat::expect_equal(tidy_df$p.value,
      c(0.0005838887, 0.1626797382, 0.0148476585),
      tolerance = 0.001
    )

    testthat::expect_identical(
      tidy_df$label,
      c(
        "list(~italic(F)(3*\",\"*35)==7.388, ~italic(p)== 0.001, ~widehat(italic(omega)[p]^2)==0.308)",
        "list(~italic(F)(1*\",\"*35)==2.034, ~italic(p)== 0.163, ~widehat(italic(omega)[p]^2)==0.023)",
        "list(~italic(F)(3*\",\"*35)==4.012, ~italic(p)== 0.015, ~widehat(italic(omega)[p]^2)==0.174)"
      )
    )
    testthat::expect_identical(tidy_df$significance, c("***", "ns", "*"))
    testthat::expect_identical(
      tidy_df$p.value.formatted,
      c("== 0.001", "== 0.163", "== 0.015")
    )
  }
)

# f-statistic and eta- and omega-squared -------------------------------------

testthat::test_that(
  desc = "ggcoefstats with non-partial variants of effect size for f-statistic",
  code = {
    testthat::skip_on_cran()

    # model
    set.seed(123)
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
        "list(~italic(F)(1*\",\"*35)==3.72, ~italic(p)== 0.062, ~widehat(italic(eta)^2)==0.05)",
        "list(~italic(F)(3*\",\"*35)==6.83, ~italic(p)== 0.001, ~widehat(italic(eta)^2)==0.29)",
        "list(~italic(F)(3*\",\"*35)==4.01, ~italic(p)== 0.015, ~widehat(italic(eta)^2)==0.17)"
      )
    )

    testthat::expect_identical(
      pb2$data[[4]]$label,
      c(
        "list(~italic(F)(1*\",\"*35)==3.72, ~italic(p)== 0.062, ~widehat(italic(omega)^2)==0.04)",
        "list(~italic(F)(3*\",\"*35)==6.83, ~italic(p)== 0.001, ~widehat(italic(omega)^2)==0.24)",
        "list(~italic(F)(3*\",\"*35)==4.01, ~italic(p)== 0.015, ~widehat(italic(omega)^2)==0.13)"
      )
    )
  }
)

# check merMod output ----------------------------------------------

testthat::test_that(
  desc = "check merMod output",
  code = {
    testthat::skip_on_cran()

    # setup
    set.seed(123)
    library(lme4)

    # model
    mod1 <-
      lme4::glmer(
        cbind(incidence, size - incidence) ~ period + (1 | herd),
        data = cbpp,
        family = binomial()
      )

    set.seed(123)
    d <- data.frame(
      y = rpois(1000, lambda = 3),
      x = runif(1000),
      f = factor(sample(
        1:10,
        size = 1000, replace = TRUE
      ))
    )

    set.seed(123)
    mod2 <- lme4::glmer(y ~ x + (1 | f), data = d, family = poisson)

    # broom output
    set.seed(123)
    broom_df1 <- broomExtra::tidy(
      x = mod1,
      conf.int = TRUE,
      conf.level = 0.99,
      effects = "fixed"
    )

    set.seed(123)
    broom_df2 <- broomExtra::tidy(
      x = mod2,
      conf.int = TRUE,
      conf.level = 0.50,
      effects = "fixed"
    )

    # ggstatsplot output
    set.seed(123)
    tidy_df1 <- ggstatsplot::ggcoefstats(
      x = mod1,
      conf.int = TRUE,
      conf.level = 0.99,
      output = "tidy",
      exclude.intercept = FALSE
    )

    set.seed(123)
    tidy_df2 <- ggstatsplot::ggcoefstats(
      x = mod2,
      conf.int = TRUE,
      conf.level = 0.50,
      output = "tidy",
      exclude.intercept = FALSE
    )

    # testing glmer
    testthat::expect_equal(broom_df1$conf.low, tidy_df1$conf.low, tolerance = 0.001)
    testthat::expect_equal(broom_df2$conf.low, tidy_df2$conf.low, tolerance = 0.001)
    testthat::expect_equal(broom_df1$conf.high, tidy_df1$conf.high, tolerance = 0.001)
    testthat::expect_equal(broom_df2$conf.high, tidy_df2$conf.high, tolerance = 0.001)
    testthat::expect_equal(broom_df1$estimate, tidy_df1$estimate, tolerance = 0.001)
    testthat::expect_equal(broom_df2$estimate, tidy_df2$estimate, tolerance = 0.001)
    testthat::expect_equal(broom_df1$std.error, tidy_df1$std.error, tolerance = 0.001)
    testthat::expect_equal(broom_df2$std.error, tidy_df2$std.error, tolerance = 0.001)
    testthat::expect_equal(broom_df1$p.value, tidy_df1$p.value, tolerance = 0.001)
    testthat::expect_equal(broom_df2$p.value, tidy_df2$p.value, tolerance = 0.001)
  }
)

# dataframe as input ----------------------------------------------------

testthat::test_that(
  desc = "ggcoefstats works with data frames",
  code = {
    testthat::skip_on_cran()
    set.seed(123)

    # creating dataframe
    df1 <- tibble::tribble(
      ~term, ~statistic, ~estimate, ~conf.low, ~conf.high, ~p.value, ~df.error,
      "level2", 0.158, 0.0665, -0.778, 0.911, 0.875, 5L,
      "level1", 1.33, 0.542, -0.280, 1.36, 0.191, 10L,
      "level3", 1.24, 0.045, 0.030, 0.65, 0.001, 12L
    )
    df2 <- dplyr::select(.data = df1, -p.value)
    df3 <- dplyr::select(.data = df1, -statistic)
    df3$p.value <- as.factor(df3$p.value)
    df4 <- dplyr::select(.data = df1, -df.error)
    df5 <- tibble::add_column(df1, std.error = c(0.015, 0.2, 0.09))
    df6 <- dplyr::select(.data = df5, -term, -estimate, -std.error)

    # repeated term dataframe
    df7 <- tibble::tribble(
      ~term, ~statistic, ~estimate, ~conf.low, ~conf.high, ~p.value, ~df.error,
      "x", 0.158, 0.0665, -0.778, 0.911, 0.875, 5L,
      "x", 1.33, 0.542, -0.280, 1.36, 0.191, 10L,
      "x", 1.24, 0.045, 0.030, 0.65, 0.001, 12L
    )

    # check that term column is generated
    df8 <- tibble::tribble(
      ~statistic, ~estimate, ~conf.low, ~conf.high, ~p.value, ~df.error,
      0.158, 0.0665, -0.778, 0.911, 0.875, 5L,
      1.33, 0.542, -0.280, 1.36, 0.191, 10L,
      1.24, 0.045, 0.030, 0.65, 0.001, 12L
    )

    testthat::expect_identical(
      colnames(ggstatsplot::ggcoefstats(df8, output = "tidy"))[[1]],
      "term"
    )

    # expect errors
    testthat::expect_message(ggstatsplot::ggcoefstats(x = df1))
    testthat::expect_error(ggstatsplot::ggcoefstats(
      x = df6,
      meta.analytic.effect = TRUE
    ))
    testthat::expect_message(ggstatsplot::ggcoefstats(x = df7))

    # plotting the dataframe
    p1 <- ggstatsplot::ggcoefstats(x = df1, statistic = "t", sort = "none")
    p2 <- ggstatsplot::ggcoefstats(x = df1, statistic = "z", sort = "descending")
    p3 <- ggstatsplot::ggcoefstats(x = df2, statistic = "t")
    p4 <- ggstatsplot::ggcoefstats(x = df3, statistic = "t") +
      ggplot2::scale_y_discrete(labels = c("x1", "x2", "x3")) +
      ggplot2::labs(x = "location", y = NULL)
    p5 <- ggstatsplot::ggcoefstats(x = df4, statistic = "t")
    set.seed(123)
    p6 <-
      suppressWarnings(ggstatsplot::ggcoefstats(
        x = df5,
        statistic = "t",
        k = 3,
        meta.analytic.effect = TRUE,
        bf.message = TRUE,
        messages = FALSE
      ))
    p7 <-
      suppressWarnings(ggstatsplot::ggcoefstats(
        x = df5,
        statistic = "t",
        k = 3,
        meta.analytic.effect = TRUE,
        meta.type = "bf",
        caption = "mnp",
        bf.message = TRUE,
        messages = FALSE
      ))

    # build plots
    pb1 <- ggplot2::ggplot_build(p1)
    pb2 <- ggplot2::ggplot_build(p2)
    pb3 <- ggplot2::ggplot_build(p3)
    pb4 <- ggplot2::ggplot_build(p4)
    pb5 <- ggplot2::ggplot_build(p5)
    pb6 <- ggplot2::ggplot_build(p6)
    pb7 <- ggplot2::ggplot_build(p7)

    # stats labels
    testthat::expect_identical(
      pb1$data[[4]]$label,
      c(
        "list(~widehat(italic(beta))==0.07, ~italic(t)(5)==0.16, ~italic(p)== 0.875)",
        "list(~widehat(italic(beta))==0.54, ~italic(t)(10)==1.33, ~italic(p)== 0.191)",
        "list(~widehat(italic(beta))==0.04, ~italic(t)(12)==1.24, ~italic(p)== 0.001)"
      )
    )
    testthat::expect_identical(
      pb5$data[[4]]$label,
      c(
        "list(~widehat(italic(beta))==0.07, ~italic(t)==0.16, ~italic(p)== 0.875)",
        "list(~widehat(italic(beta))==0.54, ~italic(t)==1.33, ~italic(p)== 0.191)",
        "list(~widehat(italic(beta))==0.04, ~italic(t)==1.24, ~italic(p)== 0.001)"
      )
    )

    testthat::expect_equal(pb2$data[[3]]$y, c(2L, 1L, 3L))
    testthat::expect_identical(
      pb2$data[[4]]$label,
      c(
        "list(~widehat(italic(beta))==0.07, ~italic(z)==0.16, ~italic(p)== 0.875)",
        "list(~widehat(italic(beta))==0.54, ~italic(z)==1.33, ~italic(p)== 0.191)",
        "list(~widehat(italic(beta))==0.04, ~italic(z)==1.24, ~italic(p)== 0.001)"
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

    # annotations
    testthat::expect_identical(p4$labels$x, "location")
    testthat::expect_null(p4$labels$y, NULL)
    testthat::expect_null(p4$labels$title, NULL)
    testthat::expect_null(p4$labels$subtitle, NULL)

    # checking meta-analysis
    testthat::expect_error(ggstatsplot::ggcoefstats(
      x = df1,
      statistic = "t",
      meta.analytic.effect = TRUE
    ))

    testthat::expect_identical(pb7$plot$labels$caption, "mnp")

    # subtitle
    set.seed(123)
    meta_info <-
      suppressWarnings(ggstatsplot::ggcoefstats(
        x = df5,
        statistic = "t",
        k = 3,
        caption.summary = FALSE,
        meta.analytic.effect = TRUE,
        bf.message = TRUE,
        output = "caption",
        messages = TRUE
      ))
    testthat::expect_identical(as.character(meta_info$expr)[6], "0.267")
  }
)

# dataframe as input (with NAs) --------------------------------------------

testthat::test_that(
  desc = "ggcoefstats works with data frames (with NAs)",
  code = {
    testthat::skip_on_cran()
    set.seed(123)

    # creating dataframe
    df <-
      tibble::tribble(
        ~term, ~statistic, ~estimate, ~std.error, ~p.value,
        "level2", 0.158, 0.0665, 0.911, 0.875,
        "level1", NA, 0.542, NA, NA,
        "level3", 1.24, 0.045, 0.65, 0.001
      )

    # coefficient plot
    p <-
      ggstatsplot::ggcoefstats(
        x = df,
        statistic = "t",
        meta.analytic.effect = TRUE,
        bf.message = TRUE,
        messages = FALSE
      )

    # build the plot
    pb <- ggplot2::ggplot_build(p)

    # checking annotations
    testthat::expect_null(p$labels$caption, NULL)
    testthat::expect_null(p$labels$subtitle, NULL)

    # labels
    testthat::expect_identical(
      pb$data[[4]]$label,
      c(
        "list(~widehat(italic(beta))==0.07, ~italic(t)==0.16, ~italic(p)== 0.875)",
        "list(~widehat(italic(beta))==0.04, ~italic(t)==1.24, ~italic(p)== 0.001)"
      )
    )
  }
)

# check confidence intervals ----------------------------------------------

testthat::test_that(
  desc = "check computing confidence intervals",
  code = {
    testthat::skip_on_cran()

    # creating broom dataframes
    set.seed(123)
    mod <- stats::lm(data = iris, formula = Sepal.Length ~ Species)
    df1 <- broomExtra::tidy(
      x = mod,
      conf.int = TRUE,
      conf.level = 0.95
    )
    df2 <- broomExtra::tidy(
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
    tidy_df3 <-
      ggstatsplot::ggcoefstats(
        x = dplyr::select(df2, -conf.low, -conf.high, -std.error),
        exclude.intercept = FALSE,
        statistic = "t",
        output = "tidy",
        conf.level = 0.50
      )

    # checking confidence intervals
    testthat::expect_equal(df1$conf.low, tidy_df1$conf.low, tolerance = 0.001)
    testthat::expect_equal(df2$conf.high, tidy_df2$conf.high, tolerance = 0.001)
    testthat::expect_identical(tidy_df3$conf.low[1], NA_character_)
    testthat::expect_identical(tidy_df3$conf.high[1], NA_character_)
    testthat::expect_is(tidy_df1, "tbl_df")
    testthat::expect_is(tidy_df2, "tbl_df")
  }
)

# check if glance works ----------------------------------------------

testthat::test_that(
  desc = "check if glance works",
  code = {
    testthat::skip_on_cran()
    library(lme4)

    # creating broom and ggstatsplot output
    # lm
    set.seed(123)
    mod1 <- stats::lm(data = iris, formula = Sepal.Length ~ Species)
    glance_df1 <- ggstatsplot::ggcoefstats(x = mod1, output = "glance")

    # lmer
    set.seed(123)
    mod2 <-
      lme4::lmer(
        formula = Reaction ~ Days + (Days | Subject),
        data = sleepstudy
      )
    glance_df2 <- ggstatsplot::ggcoefstats(x = mod2, output = "glance")

    # checking if they are equal
    testthat::expect_true(all(c("aic", "bic") %in% names(glance_df1)))
    testthat::expect_true(all(c("aic", "bic") %in% names(glance_df2)))
  }
)


# check if augment works ----------------------------------------------

testthat::test_that(
  desc = "check if augment works",
  code = {
    testthat::skip_on_cran()

    # set up
    library(lme4)

    # linear model
    set.seed(123)
    mod1 <- stats::lm(
      formula = mpg ~ wt + qsec,
      data = mtcars
    )
    df1.broom <- broomExtra::augment(mod1)
    df1.ggstats <- ggstatsplot::ggcoefstats(x = mod1, output = "augment")

    # model with F-statistic
    set.seed(123)
    mod3 <- stats::aov(
      data = ggplot2::msleep,
      formula = sleep_rem ~ brainwt * vore
    )
    df3.broom <- tibble::as_tibble(broomExtra::augment(mod3))
    df3.ggstats <- ggstatsplot::ggcoefstats(x = mod3, output = "augment")

    # checking if they are equal
    testthat::expect_identical(df1.broom, df1.ggstats)
    testthat::expect_identical(df3.broom, df3.ggstats)
    testthat::expect_true(inherits(df1.ggstats, what = "tbl_df"))
    testthat::expect_true(inherits(df3.ggstats, what = "tbl_df"))
  }
)

# testing aesthetic modifications --------------------------------------------

testthat::test_that(
  desc = "testing aesthetic modifications",
  code = {
    testthat::skip_on_cran()

    # model
    set.seed(123)
    mod <-
      stats::lm(
        formula = rating ~ belief * outcome * question * item,
        data = ggstatsplot::intent_morality
      )

    # plot
    p <-
      ggstatsplot::ggcoefstats(
        x = mod,
        exclude.intercept = FALSE,
        point.args = list(size = 6, shape = 5, color = "red")
      )

    # plot build
    pb <- ggplot2::ggplot_build(p)

    # checking layered data
    testthat::expect_identical(unique(pb$data[[4]]$colour), "black")
    testthat::expect_identical(unique(pb$data[[3]]$colour), "red")
    testthat::expect_equal(unique(pb$data[[3]]$shape), 5L)
    testthat::expect_equal(unique(pb$data[[3]]$size), 6L)
  }
)

# unsupported model objects -----------------------------------------------

testthat::test_that(
  desc = "unsupported model objects",
  code = {

    # mod-1
    testthat::expect_error(ggstatsplot::ggcoefstats(x = list(x = "1", y = 2L)))

    # mod-2
    mod2 <- stats::aov(
      formula = value ~ attribute * measure + Error(id / (attribute * measure)),
      data = ggstatsplot::iris_long
    )

    # plot
    p <- ggstatsplot::ggcoefstats(mod2)
    pb <- ggplot2::ggplot_build(p)

    # test failures
    testthat::expect_error(ggstatsplot::ggcoefstats(stats::acf(lh, plot = FALSE)))
    testthat::expect_null(pb$plot$labels$subtitle, NULL)
    testthat::expect_null(pb$plot$labels$caption, NULL)
  }
)
