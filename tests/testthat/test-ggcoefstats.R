# z-statistic --------------------------------------------------

testthat::test_that(
  desc = "ggcoefstats with glm with z",
  code = {
    testthat::skip_on_cran()
    set.seed(123)

    # having a look at the Titanic dataset
    df <- as.data.frame(Titanic)

    # model
    mod <-
      stats::glm(
        formula = Survived ~ Sex + Age,
        data = df,
        weights = df$Freq,
        family = stats::binomial(link = "logit")
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
          x = c(-0.780044678205957, 2.29400668890043, -0.556439259603544),
          y = structure(1:3, class = c("mapped_discrete", "numeric")),
          label = c(
            "list(~widehat(italic(beta))==-0.78, ~italic(z)==-3.47, ~italic(p)==0.001)",
            "list(~widehat(italic(beta))==2.29, ~italic(z)==19.13, ~italic(p)==1.54e-81)",
            "list(~widehat(italic(beta))==-0.56, ~italic(z)==-2.44, ~italic(p)==0.014)"
          ),
          PANEL = structure(c(1L, 1L, 1L), .Label = "1", class = "factor"),
          group = structure(1:3, n = 3L),
          colour = structure(c(
            "#1B9E77FF",
            "#D95F02FF", "#7570B3FF"
          ), class = "colors"),
          fill = c(
            "white",
            "white", "white"
          ),
          size = c(3, 3, 3),
          angle = c(0, 0, 0),
          alpha = c(NA, NA, NA),
          family = c("", "", ""),
          fontface = c(
            1,
            1, 1
          ),
          lineheight = c(1.2, 1.2, 1.2),
          hjust = c(
            0.5, 0.5,
            0.5
          ),
          vjust = c(0.5, 0.5, 0.5)
        ),
        row.names = c(NA, -3L),
        class = "data.frame"
      )
    )
  }
)

# chi^2-statistic --------------------------------------------------

testthat::test_that(
  desc = "ggcoefstats with coxph.panel model",
  code = {
    testthat::skip_on_cran()

    # model
    df <-
      structure(
        list(
          term = c("age", "sex"),
          estimate = c(
            0.0170335066199796,
            -0.511668342705175
          ),
          std.error = c(0.00923266440539569, 0.167678592139827),
          conf.low = c(-0.00106218309594089, -0.840312344277616),
          conf.high = c(
            0.0351291963359,
            -0.183024341132734
          ),
          statistic = c(3.40372002622092, 9.31154544604583),
          df.error = c(225L, 225L),
          p.value = c(
            0.0650495624855354,
            0.002277143223301
          )
        ),
        row.names = c(NA, -2L),
        pretty_names = c(
          age = "age",
          sex = "sex"
        ),
        ci = 0.95,
        exponentiate = FALSE,
        ordinal_model = FALSE,
        model_class = c(
          "coxph.penal",
          "coxph"
        ),
        digits = 2,
        ci_digits = 2,
        p_digits = 3,
        object_name = "x",
        class = c(
          "tbl_df",
          "tbl", "data.frame"
        )
      )

    # plot
    set.seed(123)
    p <- ggstatsplot::ggcoefstats(df, statistic = "chi")

    # plot build
    pb <- ggplot2::ggplot_build(p)

    # tidy dataframe from the function
    testthat::expect_equal(
      pb$data[[4]],
      structure(
        list(
          x = c(0.0170335066199796, -0.511668342705175),
          y = structure(1:2, class = c("mapped_discrete", "numeric")),
          label = c(
            "list(~widehat(italic(beta))==0.02, ~italic(chi)^2==3.40, ~italic(p)==0.065)",
            "list(~widehat(italic(beta))==-0.51, ~italic(chi)^2==9.31, ~italic(p)==0.002)"
          ),
          PANEL = structure(c(1L, 1L), .Label = "1", class = "factor"),
          group = structure(1:2, n = 2L),
          colour = structure(c(
            "#1B9E77FF",
            "#D95F02FF"
          ), class = "colors"),
          fill = c("white", "white"),
          size = c(3, 3),
          angle = c(0, 0),
          alpha = c(NA, NA),
          family = c(
            "",
            ""
          ),
          fontface = c(1, 1),
          lineheight = c(1.2, 1.2),
          hjust = c(
            0.5,
            0.5
          ),
          vjust = c(0.5, 0.5)
        ),
        row.names = c(NA, -2L),
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
    # testthat::expect_identical(
    #   pb$data[[4]]$label,
    #   c(
    #     "list(~widehat(italic(beta))==6.438, ~italic(t)(28)==13.765, ~italic(p)==5.48e-14)",
    #     "list(~widehat(italic(beta))==-0.156, ~italic(t)(28)==-5.840, ~italic(p)==2.81e-06)",
    #     "list(~widehat(italic(beta))==-1.809, ~italic(t)(28)==-2.615, ~italic(p)==0.014)",
    #     NA_character_
    #   )
    # )
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
          group = structure(-1L, n = 1L),
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
      tidy_df$label,
      c(
        "list(~italic(F)(1*\",\"*28)==118.89, ~italic(p)==1.38e-11, ~widehat(italic(eta)[p]^2)==0.81)",
        "list(~italic(F)(1*\",\"*28)==7.30, ~italic(p)==0.012, ~widehat(italic(eta)[p]^2)==0.21)",
        "list(~italic(F)(1*\",\"*28)==3.73, ~italic(p)==0.064, ~widehat(italic(eta)[p]^2)==0.12)"
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
      ggplot2::expr(atop(
        displaystyle(paste(italic("Note"), ": From `tidyverse`")),
        expr = paste(
          "AIC = ", "126", ", BIC = ",
          "142"
        )
      ))
    )
    testthat::expect_identical(p$labels$title, "mammalian sleep")
    testthat::expect_identical(p$labels$subtitle, "Source: `ggplot2` package")

    testthat::expect_equal(pb$data[[2]]$x, tidy_df$estimate, tolerance = 0.001)
    testthat::expect_equal(pb$data[[2]]$xmin, tidy_df$conf.low, tolerance = 0.001)
    testthat::expect_equal(pb$data[[2]]$xmax, tidy_df$conf.high, tolerance = 0.001)
    testthat::expect_equal(
      pb$data[[2]]$y,
      structure(c(3L, 1L, 2L), class = c("mapped_discrete", "numeric"))
    )

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
        "list(~italic(F)(3*\",\"*35)==7.388, ~italic(p)==0.001, ~widehat(italic(omega)[p]^2)==0.308)",
        "list(~italic(F)(1*\",\"*35)==2.034, ~italic(p)==0.163, ~widehat(italic(omega)[p]^2)==0.023)",
        "list(~italic(F)(3*\",\"*35)==4.012, ~italic(p)==0.015, ~widehat(italic(omega)[p]^2)==0.174)"
      )
    )
    testthat::expect_identical(tidy_df$significance, c("***", "ns", "*"))
  }
)

# dataframe as input ----------------------------------------------------

testthat::test_that(
  desc = "ggcoefstats works with data frames",
  code = {
    testthat::skip_on_cran()
    set.seed(123)

    # creating dataframe
    df1 <-
      tibble::tribble(
        ~term,
        ~statistic,
        ~estimate,
        ~conf.low,
        ~conf.high,
        ~p.value,
        ~df.error,
        "level2",
        0.158,
        0.0665,
        -0.778,
        0.911,
        0.875,
        5L,
        "level1",
        1.33,
        0.542,
        -0.280,
        1.36,
        0.191,
        10L,
        "level3",
        1.24,
        0.045,
        0.030,
        0.65,
        0.001,
        12L
      )
    df2 <- dplyr::select(.data = df1, -p.value)
    df3 <- dplyr::select(.data = df1, -statistic)
    df3$p.value <- as.factor(df3$p.value)
    df4 <- dplyr::select(.data = df1, -df.error)
    df5 <- tibble::add_column(df1, std.error = c(0.015, 0.2, 0.09))
    df6 <- dplyr::select(.data = df5, -term, -estimate, -std.error)

    # repeated term dataframe
    df7 <-
      tibble::tribble(
        ~term,
        ~statistic,
        ~estimate,
        ~conf.low,
        ~conf.high,
        ~p.value,
        ~df.error,
        "x",
        0.158,
        0.0665,
        -0.778,
        0.911,
        0.875,
        5L,
        "x",
        1.33,
        0.542,
        -0.280,
        1.36,
        0.191,
        10L,
        "x",
        1.24,
        0.045,
        0.030,
        0.65,
        0.001,
        12L
      )

    # check that term column is generated
    df8 <-
      tibble::tribble(
        ~statistic,
        ~estimate,
        ~conf.low,
        ~conf.high,
        ~p.value,
        ~df.error,
        0.158,
        0.0665,
        -0.778,
        0.911,
        0.875,
        5L,
        1.33,
        0.542,
        -0.280,
        1.36,
        0.191,
        10L,
        1.24,
        0.045,
        0.030,
        0.65,
        0.001,
        12L
      )

    testthat::expect_identical(
      colnames(ggstatsplot::ggcoefstats(df8, output = "tidy"))[[7]],
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
    p1 <-
      ggstatsplot::ggcoefstats(
        x = df1,
        statistic = "t",
        sort = "none"
      )
    p2 <-
      ggstatsplot::ggcoefstats(
        x = df1,
        statistic = "z",
        sort = "descending"
      )
    p3 <- ggstatsplot::ggcoefstats(x = df2, statistic = "t")
    p4 <- ggstatsplot::ggcoefstats(x = df3, statistic = "T") +
      ggplot2::scale_y_discrete(labels = c("x1", "x2", "x3")) +
      ggplot2::labs(x = "location", y = NULL)
    p5 <- ggstatsplot::ggcoefstats(x = df4, statistic = "T-value")
    set.seed(123)
    p6 <-
      suppressWarnings(
        ggstatsplot::ggcoefstats(
          x = df5,
          statistic = "t",
          k = 3,
          meta.analytic.effect = TRUE,
          bf.message = TRUE,
          messages = FALSE
        )
      )
    p7 <-
      suppressWarnings(
        ggstatsplot::ggcoefstats(
          x = df5,
          statistic = "T",
          k = 3,
          meta.analytic.effect = TRUE,
          meta.type = "bf",
          caption = "mnp",
          bf.message = TRUE,
          messages = FALSE
        )
      )

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
        "list(~widehat(italic(beta))==0.07, ~italic(t)(5)==0.16, ~italic(p)==0.875)",
        "list(~widehat(italic(beta))==0.54, ~italic(t)(10)==1.33, ~italic(p)==0.191)",
        "list(~widehat(italic(beta))==0.04, ~italic(t)(12)==1.24, ~italic(p)==0.001)"
      )
    )
    testthat::expect_identical(
      pb5$data[[4]]$label,
      c(
        "list(~widehat(italic(beta))==0.07, ~italic(t)==0.16, ~italic(p)==0.875)",
        "list(~widehat(italic(beta))==0.54, ~italic(t)==1.33, ~italic(p)==0.191)",
        "list(~widehat(italic(beta))==0.04, ~italic(t)==1.24, ~italic(p)==0.001)"
      )
    )

    testthat::expect_equal(
      pb2$data[[3]]$y,
      structure(c(2L, 1L, 3L), class = c("mapped_discrete", "numeric"))
    )
    testthat::expect_identical(
      pb2$data[[4]]$label,
      c(
        "list(~widehat(italic(beta))==0.07, ~italic(z)==0.16, ~italic(p)==0.875)",
        "list(~widehat(italic(beta))==0.54, ~italic(z)==1.33, ~italic(p)==0.191)",
        "list(~widehat(italic(beta))==0.04, ~italic(z)==1.24, ~italic(p)==0.001)"
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

    # caption
    set.seed(123)
    meta_info <-
      suppressWarnings(
        ggstatsplot::ggcoefstats(
          x = df5,
          statistic = "t",
          k = 3,
          meta.analytic.effect = TRUE,
          bf.message = TRUE,
          output = "caption",
          messages = TRUE
        )
      )
    testthat::expect_identical(as.character(meta_info$expr)[19], "81.42%")
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
    df1 <-
      broomExtra::tidy(
        x = mod,
        conf.int = TRUE,
        conf.level = 0.95
      )
    df2 <-
      broomExtra::tidy(
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

    # creating broom and ggstatsplot output
    # lm
    set.seed(123)
    mod1 <- stats::lm(data = iris, formula = Sepal.Length ~ Species)
    glance_df1 <- ggstatsplot::ggcoefstats(x = mod1, output = "glance")

    # checking if they are equal
    testthat::expect_true(all(c("aic", "bic") %in% names(glance_df1)))
  }
)


# check if augment works ----------------------------------------------

testthat::test_that(
  desc = "check if augment works",
  code = {
    testthat::skip_on_cran()

    # linear model
    set.seed(123)
    mod1 <- stats::lm(
      formula = mpg ~ wt + qsec,
      data = mtcars
    )
    df1.broom <- broomExtra::augment(mod1)
    df1.ggstats <-
      ggstatsplot::ggcoefstats(x = mod1, output = "augment")

    # model with F-statistic
    set.seed(123)
    mod3 <- stats::aov(
      data = ggplot2::msleep,
      formula = sleep_rem ~ brainwt * vore
    )
    df3.broom <- tibble::as_tibble(broomExtra::augment(mod3))
    df3.ggstats <-
      ggstatsplot::ggcoefstats(x = mod3, output = "augment")

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
        formula = wt ~ am * cyl * vs,
        data = mtcars
      )

    # plot
    suppressWarnings(suppressMessages(
      p <-
        ggstatsplot::ggcoefstats(
          x = mod,
          exclude.intercept = FALSE,
          exponentiate = TRUE,
          point.args = list(
            size = 6,
            shape = 5,
            color = "red"
          ),
          package = "ggsci",
          palette = "alternating_igv"
        )
    ))

    # plot build
    pb <- ggplot2::ggplot_build(p)

    # checking layered data
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
