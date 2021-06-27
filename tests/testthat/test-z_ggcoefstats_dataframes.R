# dataframe as input ----------------------------------------------------

test_that(
  desc = "ggcoefstats works with data frames",
  code = {
    skip_on_cran()
    skip_if_not_installed("metafor")
    skip_if_not_installed("metaBMA")
    set.seed(123)

    options(tibble.width = Inf)

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
    df2 <- dplyr::select(df1, -p.value)
    df3 <- dplyr::select(df1, -statistic)
    df3$p.value <- as.factor(df3$p.value)
    df4 <- dplyr::select(df1, -df.error)
    df5 <- tibble::add_column(df1, std.error = c(0.015, 0.2, 0.09))
    df6 <- dplyr::select(df5, -term, -estimate, -std.error)

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
    expect_error(ggcoefstats(
      x = df6,
      meta.analytic.effect = TRUE
    ))
    expect_error(ggcoefstats(x = df7))

    # plotting the dataframe
    set.seed(123)
    p1 <-
      ggcoefstats(
        x = df1,
        statistic = "t",
        sort = "none"
      )

    set.seed(123)
    p2 <-
      ggcoefstats(
        x = df1,
        statistic = "z",
        sort = "descending"
      )

    set.seed(123)
    p3 <- ggcoefstats(x = df2, statistic = "t")

    set.seed(123)
    p4 <- ggcoefstats(x = df3, statistic = "T") +
      ggplot2::scale_y_discrete(labels = c("x1", "x2", "x3")) +
      ggplot2::labs(x = "location", y = NULL)

    set.seed(123)
    p6 <-
      suppressWarnings(
        ggcoefstats(
          x = df5,
          statistic = "t",
          k = 2L,
          meta.analytic.effect = TRUE,
          bf.message = TRUE
        )
      )

    set.seed(123)
    p7 <-
      suppressWarnings(
        ggcoefstats(
          x = df5,
          statistic = "T",
          k = 2L,
          meta.analytic.effect = TRUE,
          meta.type = "bayes",
          caption = "mnp",
          bf.message = TRUE
        )
      )

    # build plots
    pb1 <- ggplot2::ggplot_build(p1)
    pb2 <- ggplot2::ggplot_build(p2)
    pb3 <- ggplot2::ggplot_build(p3)
    pb4 <- ggplot2::ggplot_build(p4)
    pb6 <- ggplot2::ggplot_build(p6)
    pb7 <- ggplot2::ggplot_build(p7)

    # checking data
    set.seed(123)
    expect_snapshot(list(
      pb1$data, pb2$data, pb3$data, pb4$data,
      pb6$data, pb7$data
    ))

    # labels
    expect_snapshot(list(
      pb1$plot$labels, pb2$plot$labels, pb3$plot$labels,
      pb4$plot$labels
    ))

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
          k = 2,
          meta.analytic.effect = TRUE,
          bf.message = TRUE,
          output = "caption"
        )
      )

    expect_identical(
      as.character(meta_info)[2],
      "\"log\"[\"e\"] * \"(BF\"[\"01\"] * \") = \" * \"1.23\" * \", \""
    )
  }
)
