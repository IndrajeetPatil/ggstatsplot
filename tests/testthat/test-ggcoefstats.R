# z-statistic --------------------------------------------------

test_that(
  desc = "ggcoefstats with glm with z",
  code = {
    skip_on_cran()
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
      ggcoefstats(
        x = mod,
        conf.level = 0.90,
        exclude.intercept = FALSE
      )

    # plot build
    pb <- ggplot2::ggplot_build(p)

    # tidy dataframe from the function
    expect_equal(
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

    expect_equal(
      pb$data[[2]],
      structure(
        list(
          x = c(-0.780044678205957, 2.29400668890043, -0.556439259603544),
          xmin = c(-1.15213248524039, 2.09855242875921, -0.929998880210387),
          xmax = c(-0.412406648700957, 2.49329224456418, -0.180433778592503),
          y = structure(1:3, class = c("mapped_discrete", "numeric")),
          PANEL = structure(c(1L, 1L, 1L), .Label = "1", class = "factor"),
          group = structure(1:3, n = 3L),
          ymin = structure(c(
            1, 2,
            3
          ), class = c("mapped_discrete", "numeric")),
          ymax = structure(c(
            1,
            2, 3
          ), class = c("mapped_discrete", "numeric")),
          colour = c(
            "black",
            "black", "black"
          ),
          size = c(0.5, 0.5, 0.5),
          linetype = c(
            1,
            1, 1
          ),
          height = c(0, 0, 0),
          alpha = c(NA, NA, NA)
        ),
        row.names = c(
          NA,
          -3L
        ),
        class = "data.frame"
      )
    )

    expect_equal(
      pb$data[[3]],
      structure(
        list(
          x = c(-0.780044678205957, 2.29400668890043, -0.556439259603544),
          y = structure(1:3, class = c("mapped_discrete", "numeric")),
          PANEL = structure(c(1L, 1L, 1L), .Label = "1", class = "factor"),
          group = structure(1:3, n = 3L),
          shape = c(19, 19, 19),
          colour = c(
            "blue",
            "blue", "blue"
          ),
          size = c(3, 3, 3),
          fill = c(NA, NA, NA),
          alpha = c(NA, NA, NA),
          stroke = c(0.5, 0.5, 0.5)
        ),
        row.names = c(
          NA,
          -3L
        ),
        class = "data.frame"
      )
    )

    expect_equal(
      pb$data[[4]],
      structure(
        list(
          x = c(-0.780044678205957, 2.29400668890043, -0.556439259603544),
          y = structure(1:3, class = c("mapped_discrete", "numeric")),
          label = c(
            "list(~widehat(italic(beta))==-0.78, ~italic(z)==-3.47, ~italic(p)=='0.001')",
            "list(~widehat(italic(beta))==2.29, ~italic(z)==19.13, ~italic(p)=='1.54e-81')",
            "list(~widehat(italic(beta))==-0.56, ~italic(z)==-2.44, ~italic(p)=='0.014')"
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
          vjust = c(0.5, 0.5, 0.5),
          point.size = c(1, 1, 1),
          segment.linetype = c(1, 1, 1),
          segment.size = c(
            0.5, 0.5,
            0.5
          ),
          segment.curvature = c(0, 0, 0),
          segment.angle = c(
            90,
            90, 90
          ),
          segment.ncp = c(1, 1, 1),
          segment.shape = c(
            0.5,
            0.5, 0.5
          ),
          segment.square = c(TRUE, TRUE, TRUE),
          segment.squareShape = c(
            1,
            1, 1
          ),
          segment.inflect = c(FALSE, FALSE, FALSE),
          segment.debug = c(
            FALSE,
            FALSE, FALSE
          )
        ),
        row.names = c(NA, -3L),
        class = "data.frame"
      )
    )
  }
)

# chi^2-statistic --------------------------------------------------

test_that(
  desc = "ggcoefstats with coxph.panel model",
  code = {
    skip_on_cran()

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
    p <- ggcoefstats(df, statistic = "chi")

    # plot build
    pb <- ggplot2::ggplot_build(p)

    # tidy dataframe from the function
    expect_equal(
      pb$data[[4]],
      structure(
        list(
          x = c(0.0170335066199796, -0.511668342705175),
          y = structure(1:2, class = c("mapped_discrete", "numeric")),
          label = c(
            "list(~widehat(italic(beta))==0.02, ~italic(chi)^2~(225)==3.40, ~italic(p)=='0.065')",
            "list(~widehat(italic(beta))==-0.51, ~italic(chi)^2~(225)==9.31, ~italic(p)=='0.002')"
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
          vjust = c(0.5, 0.5),
          point.size = c(1, 1),
          segment.linetype = c(
            1,
            1
          ),
          segment.size = c(0.5, 0.5),
          segment.curvature = c(
            0,
            0
          ),
          segment.angle = c(90, 90),
          segment.ncp = c(1, 1),
          segment.shape = c(
            0.5,
            0.5
          ),
          segment.square = c(TRUE, TRUE),
          segment.squareShape = c(
            1,
            1
          ),
          segment.inflect = c(FALSE, FALSE),
          segment.debug = c(
            FALSE,
            FALSE
          )
        ),
        row.names = c(NA, -2L),
        class = "data.frame"
      )
    )
  }
)

# t-statistic --------------------------------------------------

test_that(
  desc = "ggcoefstats with lm model",
  code = {
    skip_on_cran()
    set.seed(123)

    # model
    mod <- stats::lm(data = mtcars, formula = wt ~ mpg * am)

    # plot
    p <-
      ggcoefstats(
        x = mod,
        conf.level = 0.99,
        exclude.intercept = TRUE,
        only.significant = TRUE,
        k = 3
      )

    # plot build
    pb <- ggplot2::ggplot_build(p)

    # checking panel parameters
    expect_equal(
      pb$layout$panel_params[[1]]$y$breaks,
      structure(c("mpg", "am", "mpg:am"), pos = 1:3)
    )

    # checking layered data
    expect_equal(dim(pb$data[[1]]), c(1L, 7L))
    expect_equal(dim(pb$data[[2]]), c(3L, 13L))
    expect_equal(dim(pb$data[[3]]), c(3L, 10L))

    # checking ggrepel label layer
    expect_identical(
      pb$data[[4]]$label,
      c(
        "list(~widehat(italic(beta))==-0.156, ~italic(t)(28)==-5.840, ~italic(p)=='2.81e-06')",
        "list(~widehat(italic(beta))==-1.809, ~italic(t)(28)==-2.615, ~italic(p)=='0.014')",
        NA_character_
      )
    )
    expect_identical(
      unclass(pb$data[[4]]$colour),
      c("#1B9E77FF", "#D95F02FF", "#7570B3FF")
    )

    # testing specific geoms
    expect_equal(
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

test_that(
  desc = "ggcoefstats with partial variants of effect size for f-statistic",
  code = {
    skip_on_cran()

    ## partial eta-squared

    set.seed(123)

    # model
    mod <- stats::aov(data = mtcars, formula = wt ~ mpg * am)

    # plot
    p <-
      ggcoefstats(
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

    expect_equal(tidy_df$estimate,
      c(0.8093822, 0.2068347, 0.1176152),
      tolerance = 1e-3
    )
    expect_equal(tidy_df$df[1], 1L)
    expect_equal(tidy_df$df.error[1], 28L)
    expect_equal(tidy_df$p.value,
      c(1.378306e-11, 1.156944e-02, 6.355055e-02),
      tolerance = 1e-5
    )

    expect_identical(p$labels$x, "partial eta-squared")
    expect_identical(p$labels$y, "effect")
    expect_identical(
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
    expect_null(p$labels$title, NULL)
    expect_null(p$labels$subtitle, NULL)
    expect_identical(
      tidy_df$label,
      c(
        "list(~italic(F)(1*\",\"*28)==118.89, ~italic(p)=='1.38e-11', ~widehat(italic(eta)[p]^2)==0.81)",
        "list(~italic(F)(1*\",\"*28)==7.30, ~italic(p)=='0.012', ~widehat(italic(eta)[p]^2)==0.21)",
        "list(~italic(F)(1*\",\"*28)==3.73, ~italic(p)=='0.064', ~widehat(italic(eta)[p]^2)==0.12)"
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
      ggcoefstats(
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
    expect_identical(p$labels$x, "partial omega-squared")
    expect_identical(p$labels$y, "term")
    expect_identical(
      p$labels$caption,
      ggplot2::expr(atop(
        displaystyle(paste(italic("Note"), ": From `tidyverse`")),
        expr = paste("AIC = ", "126", ", BIC = ", "142")
      ))
    )
    expect_identical(p$labels$title, "mammalian sleep")
    expect_identical(p$labels$subtitle, "Source: `ggplot2` package")

    expect_equal(pb$data[[2]]$x, tidy_df$estimate, tolerance = 0.001)
    expect_equal(pb$data[[2]]$xmin, tidy_df$conf.low, tolerance = 0.001)
    expect_equal(pb$data[[2]]$xmax, tidy_df$conf.high, tolerance = 0.001)
    expect_equal(
      pb$data[[2]]$y,
      structure(c(3L, 1L, 2L), class = c("mapped_discrete", "numeric"))
    )

    expect_identical(tidy_df$label, pb$data[[4]]$label)

    expect_equal(tidy_df$estimate,
      c(0.30828881, 0.02348073, 0.17365008),
      tolerance = 0.001
    )
    expect_equal(tidy_df$df[1], 3L)
    expect_equal(tidy_df$df.error[1], 35L)
    expect_equal(tidy_df$p.value,
      c(0.0005838887, 0.1626797382, 0.0148476585),
      tolerance = 0.001
    )

    expect_identical(
      tidy_df$label,
      c(
        "list(~italic(F)(3*\",\"*35)==7.388, ~italic(p)=='0.001', ~widehat(italic(omega)[p]^2)==0.308)",
        "list(~italic(F)(1*\",\"*35)==2.034, ~italic(p)=='0.163', ~widehat(italic(omega)[p]^2)==0.023)",
        "list(~italic(F)(3*\",\"*35)==4.012, ~italic(p)=='0.015', ~widehat(italic(omega)[p]^2)==0.174)"
      )
    )
  }
)

# dataframe as input ----------------------------------------------------

test_that(
  desc = "ggcoefstats works with data frames",
  code = {
    skip_on_cran()
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

    expect_identical(
      colnames(ggcoefstats(df8, output = "tidy"))[[7]],
      "term"
    )

    # expect errors
    expect_message(ggcoefstats(x = df1))
    expect_error(ggcoefstats(
      x = df6,
      meta.analytic.effect = TRUE
    ))
    expect_message(ggcoefstats(x = df7))

    # plotting the dataframe
    p1 <-
      ggcoefstats(
        x = df1,
        statistic = "t",
        sort = "none"
      )
    p2 <-
      ggcoefstats(
        x = df1,
        statistic = "z",
        sort = "descending"
      )
    p3 <- ggcoefstats(x = df2, statistic = "t")
    p4 <- ggcoefstats(x = df3, statistic = "T") +
      ggplot2::scale_y_discrete(labels = c("x1", "x2", "x3")) +
      ggplot2::labs(x = "location", y = NULL)
    p5 <- ggcoefstats(x = df4, statistic = "T-value")
    set.seed(123)
    p6 <-
      suppressWarnings(
        ggcoefstats(
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
        ggcoefstats(
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
    expect_identical(
      pb1$data[[4]]$label,
      c(
        "list(~widehat(italic(beta))==0.07, ~italic(t)(5)==0.16, ~italic(p)=='0.875')",
        "list(~widehat(italic(beta))==0.54, ~italic(t)(10)==1.33, ~italic(p)=='0.191')",
        "list(~widehat(italic(beta))==0.04, ~italic(t)(12)==1.24, ~italic(p)=='0.001')"
      )
    )
    expect_identical(
      pb5$data[[4]]$label,
      c(
        "list(~widehat(italic(beta))==0.07, ~italic(t)(Inf)==0.16, ~italic(p)=='0.875')",
        "list(~widehat(italic(beta))==0.54, ~italic(t)(Inf)==1.33, ~italic(p)=='0.191')",
        "list(~widehat(italic(beta))==0.04, ~italic(t)(Inf)==1.24, ~italic(p)=='0.001')"
      )
    )

    expect_equal(
      pb2$data[[3]]$y,
      structure(c(2L, 1L, 3L), class = c("mapped_discrete", "numeric"))
    )
    expect_identical(
      pb2$data[[4]]$label,
      c(
        "list(~widehat(italic(beta))==0.07, ~italic(z)==0.16, ~italic(p)=='0.875')",
        "list(~widehat(italic(beta))==0.54, ~italic(z)==1.33, ~italic(p)=='0.191')",
        "list(~widehat(italic(beta))==0.04, ~italic(z)==1.24, ~italic(p)=='0.001')"
      )
    )

    # checking number of data layers
    expect_equal(length(pb1$data), 4L)
    expect_equal(length(pb2$data), 4L)
    expect_equal(length(pb3$data), 3L)
    expect_equal(length(pb4$data), 3L)

    # confidence intervals used for each layer should be the same as df
    expect_equal(pb3$data[[2]]$xmin, df1$conf.low)
    expect_equal(pb3$data[[2]]$xmax, df2$conf.high)
    expect_equal(pb2$data[[2]]$xmin, df3$conf.low)
    expect_equal(pb2$data[[2]]$xmax, df3$conf.high)

    # annotations
    expect_identical(p4$labels$x, "location")
    expect_null(p4$labels$y, NULL)
    expect_null(p4$labels$title, NULL)
    expect_null(p4$labels$subtitle, NULL)

    # checking meta-analysis
    expect_error(ggcoefstats(
      x = df1,
      statistic = "t",
      meta.analytic.effect = TRUE
    ))

    expect_identical(pb7$plot$labels$caption, "mnp")

    # caption
    set.seed(123)
    meta_info <-
      suppressWarnings(
        ggcoefstats(
          x = df5,
          statistic = "t",
          k = 3,
          meta.analytic.effect = TRUE,
          bf.message = TRUE,
          output = "caption",
          messages = TRUE
        )
      )
    expect_identical(as.character(meta_info$expr)[19], "81.42%")
  }
)

# check tidy output ----------------------------------------------

test_that(
  desc = "check tidy output",
  code = {
    skip_on_cran()

    library(ggstatsplot)

    set.seed(123)
    m <- aov(yield ~ N * P * K + Error(block), npk)
    m2 <- aov(yield ~ N * P * K, npk)

    # computed dataframes
    set.seed(123)
    tidy_df1 <- ggcoefstats(m, output = "tidy")

    set.seed(123)
    tidy_df2 <- ggcoefstats(m2, output = "tidy")

    # checking entire objects
    expect_equal(
      tidy_df1,
      structure(list(
        group = c(
          "block", "Within", "Within", "Within",
          "Within", "Within", "Within"
        ), term = structure(1:7, .Label = c(
          "N:P:K",
          "N", "P", "K", "N:P", "N:K", "P:K"
        ), class = "factor"), df = c(
          1,
          1, 1, 1, 1, 1, 1
        ), statistic = c(
          0.483218701027338, 12.2587342136509,
          0.544129816860359, 6.16568920231712, 1.37829669341201, 2.14597200733998,
          0.0311949051919548
        ), p.value = c(
          0.525236141197407, 0.00437181182579937,
          0.474904092674435, 0.0287950535002327, 0.263165282877168, 0.168647878500492,
          0.862752085685407
        ), estimate = c(
          0.107783878782583, 0.505332805318122,
          0.0433772469517178, 0.33941399820551, 0.103024826328657, 0.151701983167116,
          0.00259283516207463
        ), conf.low = c(
          0, 0.084350699680618, 0, 0,
          0, 0, 0
        ), conf.high = c(
          0.638747800504964, 0.740593750884156,
          0.377609203815996, 0.641576291838933, 0.454996866706633, 0.502438966717436,
          0.223149127768657
        ), df.error = c(4, 12, 12, 12, 12, 12, 12),
        estimate.type = c(
          "partial eta-squared", "partial eta-squared",
          "partial eta-squared", "partial eta-squared", "partial eta-squared",
          "partial eta-squared", "partial eta-squared"
        ), label = c(
          "list(~italic(F)(1*\",\"*4)==0.48, ~italic(p)=='0.525', ~widehat(italic(eta)[p]^2)==0.11)",
          "list(~italic(F)(1*\",\"*12)==12.26, ~italic(p)=='0.004', ~widehat(italic(eta)[p]^2)==0.51)",
          "list(~italic(F)(1*\",\"*12)==0.54, ~italic(p)=='0.475', ~widehat(italic(eta)[p]^2)==0.04)",
          "list(~italic(F)(1*\",\"*12)==6.17, ~italic(p)=='0.029', ~widehat(italic(eta)[p]^2)==0.34)",
          "list(~italic(F)(1*\",\"*12)==1.38, ~italic(p)=='0.263', ~widehat(italic(eta)[p]^2)==0.10)",
          "list(~italic(F)(1*\",\"*12)==2.15, ~italic(p)=='0.169', ~widehat(italic(eta)[p]^2)==0.15)",
          "list(~italic(F)(1*\",\"*12)==0.03, ~italic(p)=='0.863', ~widehat(italic(eta)[p]^2)==0.00)"
        )
      ),
      row.names = c(NA, -7L),
      class = c("tbl_df", "tbl", "data.frame")
      ),
      tolerance = 0.001
    )

    expect_equal(
      tidy_df2,
      structure(list(
        term = structure(1:7, .Label = c(
          "N", "P", "K", "N:P", "N:K", "P:K", "N:P:K"
        ), class = "factor"), df = c(
          1, 1,
          1, 1, 1, 1, 1
        ), statistic = c(
          6.1607605408411, 0.273458372323257,
          3.09863433554389, 0.692678031381803, 1.07848163066032, 0.0156773397344615,
          1.20433432333835
        ), p.value = c(
          0.0245421094142749, 0.608187501010219,
          0.0974576803101534, 0.417504736737997, 0.314477857657635, 0.901917664764329,
          0.288698985559183
        ), estimate = c(
          0.278003118597268, 0.0168039494781475,
          0.162243764716575, 0.0414959199524239, 0.063148566364598, 0.000978874599050923,
          0.0700017972624853
        ), conf.low = c(
          0.000467178720183505, 0, 0,
          0, 0, 0, 0
        ), conf.high = c(
          0.568063130817655, 0.273991917185563,
          0.472609487705818, 0.328449842009168, 0.3622247504399, 0.146641480018096,
          0.371691379747126
        ), df.error = c(16, 16, 16, 16, 16, 16, 16),
        estimate.type = c(
          "partial eta-squared", "partial eta-squared",
          "partial eta-squared", "partial eta-squared", "partial eta-squared",
          "partial eta-squared", "partial eta-squared"
        ), label = c(
          "list(~italic(F)(1*\",\"*16)==6.16, ~italic(p)=='0.025', ~widehat(italic(eta)[p]^2)==0.28)",
          "list(~italic(F)(1*\",\"*16)==0.27, ~italic(p)=='0.608', ~widehat(italic(eta)[p]^2)==0.02)",
          "list(~italic(F)(1*\",\"*16)==3.10, ~italic(p)=='0.097', ~widehat(italic(eta)[p]^2)==0.16)",
          "list(~italic(F)(1*\",\"*16)==0.69, ~italic(p)=='0.418', ~widehat(italic(eta)[p]^2)==0.04)",
          "list(~italic(F)(1*\",\"*16)==1.08, ~italic(p)=='0.314', ~widehat(italic(eta)[p]^2)==0.06)",
          "list(~italic(F)(1*\",\"*16)==0.02, ~italic(p)=='0.902', ~widehat(italic(eta)[p]^2)==0.00)",
          "list(~italic(F)(1*\",\"*16)==1.20, ~italic(p)=='0.289', ~widehat(italic(eta)[p]^2)==0.07)"
        )
      ), row.names = c(NA, -7L), class = c("tbl_df", "tbl", "data.frame")),
      tolerance = 0.001
    )
  }
)

# check if glance works ----------------------------------------------

test_that(
  desc = "check if glance works",
  code = {
    skip_on_cran()

    # lm
    set.seed(123)
    mod1 <- stats::lm(data = iris, formula = Sepal.Length ~ Species)
    glance_df1 <- ggcoefstats(x = mod1, output = "glance")

    # checking if they are present
    expect_true(all(c("aic", "bic") %in% names(glance_df1)))
  }
)

# CIs missing and palette change message -------------------------------------

test_that(
  desc = "CIs missing and palette change message",
  code = {
    skip_on_cran()

    df <-
      structure(list(
        term = c(
          "(Intercept)", "CuCu035", "CuCu175",
          "Time", "I(Time^2)", "I(Time^3)", "CuCu035:Time", "CuCu175:Time",
          "CuCu035:I(Time^2)", "CuCu175:I(Time^2)", "CuCu035:I(Time^3)",
          "CuCu175:I(Time^3)"
        ), estimate = c(
          21.8578677803274, 0.526733674732889,
          0.0427835422515209, 2.8851574385696, 0.614042414524674, -0.0262947719084166,
          -0.40542426165259, 0.85694067753032, 0.0182862906307392, -0.0960713284650128,
          0.000546698056168512, 0.00269961148409459
        ), std.error = c(
          0.693144489488905,
          0.941360576277464, 1.01964740267859, 0.360257238194542, 0.0701643241562136,
          0.00382178339260795, 0.637125037623185, 0.615624385440062, 0.11543261090741,
          0.110799697486582, 0.00598959833358501, 0.00565019761578672
        ),
        statistic = c(
          994.415853036382, 0.313090691524612, 0.00176057059261262,
          64.1377326818143, 76.5885855307124, 47.337648230833, 0.404920835633417,
          1.93762571038971, 0.0250954043758227, 0.751814059246073,
          0.00833104846354157, 0.228283887066837
        ), p.value = c(
          0, 0.57578977738106,
          0.966531259919043, 1.11022302462516e-15, 0, 5.97533134083506e-12,
          0.524558812161593, 0.16392656280827, 0.874129565558699, 0.38590249597292,
          0.927274418017278, 0.632799230168403
        )
      ), class = c(
        "tbl_df",
        "tbl", "data.frame"
      ), row.names = c(NA, -12L))


    p <- ggcoefstats(df, statistic = "chi")

    pb <- ggplot2::ggplot_build(p)

    expect_equal(length(pb$data), 3L)
  }
)
