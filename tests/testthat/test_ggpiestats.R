# context ------------------------------------------------------------
context(desc = "ggpiestats")

# one sample proportion test -----------------------------------------

testthat::test_that(
  desc = "checking one sample proportion test",
  code = {
    testthat::skip_on_cran()

    # creating the plot
    set.seed(123)
    p <- ggstatsplot::ggpiestats(
      data = ggplot2::msleep,
      main = vore,
      bf.message = TRUE,
      title = "mammalian sleep",
      legend.title = "vorarephilia",
      caption = "From ggplot2 package",
      perc.k = 2,
      nboot = 25,
      slice.label = "both",
      messages = FALSE
    )

    # built plot
    pb <- ggplot2::ggplot_build(p)

    # checking data used to create a plot
    dat <- p$data

    # subtitle used
    set.seed(123)
    p_subtitle <-
      ggstatsplot::subtitle_onesample_proptest(
        data = ggplot2::msleep,
        main = "vore",
        nboot = 25
      )

    # checking dimensions of data
    data_dims <- dim(dat)

    # testing everything is okay with data
    testthat::expect_equal(data_dims, c(4L, 4L))
    testthat::expect_equal(dat$perc,
      c(26.30, 6.58, 42.10, 25.00),
      tolerance = 1e-3
    )
    testthat::expect_equal(dat$counts, c(20L, 5L, 32L, 19))
    testthat::expect_equal(
      as.character(dat$main),
      c("omni", "insecti", "herbi", "carni")
    )
    testthat::expect_identical(
      pb$data[[2]]$label,
      c(
        "n = 19\n(25%)",
        "n = 32\n(42.11%)",
        "n = 5\n(6.58%)",
        "n = 20\n(26.32%)"
      )
    )
    testthat::expect_identical(
      dplyr::arrange(pb$data[[2]], group)$label,
      dat$slice.label
    )

    # checking plot labels
    testthat::expect_identical(p$labels$subtitle, p_subtitle)
    testthat::expect_identical(p$labels$title, "mammalian sleep")
    testthat::expect_identical(p$labels$caption, "From ggplot2 package")
    testthat::expect_null(p$labels$x, NULL)
    testthat::expect_null(p$labels$y, NULL)

    # checking layer data
    testthat::expect_equal(length(pb$data), 2L)
    testthat::expect_equal(dim(pb$data[[1]]), c(4L, 13L))
    testthat::expect_equal(dim(pb$data[[2]]), c(4L, 19L))
    testthat::expect_identical(
      pb$data[[1]]$fill,
      c("#E7298A", "#7570B3", "#D95F02", "#1B9E77")
    )
  }
)

# contingency tab ---------------------------------------------------------

testthat::test_that(
  desc = "checking labels with contingency tab",
  code = {

    # creating the plot
    set.seed(123)
    p <- suppressWarnings(
      ggstatsplot::ggpiestats(
        data = mtcars,
        main = "am",
        condition = "cyl",
        bf.message = TRUE,
        perc.k = 2,
        nboot = 25,
        package = "wesanderson",
        palette = "Royal2",
        ggtheme = ggplot2::theme_bw(),
        slice.label = "counts",
        legend.title = "transmission",
        factor.levels = c("0 = automatic", "1 = manual"),
        facet.wrap.name = "cylinders",
        simulate.p.value = TRUE,
        B = 3000,
        messages = FALSE
      )
    )

    # dropped level dataset
    mtcars_small <- dplyr::filter(.data = mtcars, am == "0")

    # plot
    p1 <-
      ggstatsplot::ggpiestats(
        data = mtcars_small,
        main = cyl,
        condition = am,
        nboot = 25,
        facet.wrap.name = "transmission",
        messages = FALSE
      )

    # build plot
    pb <- ggplot2::ggplot_build(p)
    pb1 <- ggplot2::ggplot_build(p1)

    # subtitle used
    set.seed(123)
    p_subtitle <-
      ggstatsplot::subtitle_contingency_tab(
        data = mtcars,
        main = "am",
        condition = "cyl",
        simulate.p.value = TRUE,
        nboot = 25,
        B = 3000,
        messages = FALSE
      )

    # checking data used to create a plot
    dat <- p$data

    # checking dimensions of data
    data_dims <- dim(dat)

    # testing everything is okay with data
    testthat::expect_equal(data_dims, c(6L, 5L))
    testthat::expect_equal(
      dat$perc,
      c(72.73, 42.86, 14.29, 27.27, 57.14, 85.71),
      tolerance = 1e-3
    )
    testthat::expect_equal(p1$data$perc,
      c(63.15789, 21.05263, 15.78947),
      tolerance = 0.001
    )
    testthat::expect_equal(p1$data$counts, c(12L, 4L, 3L))
    testthat::expect_identical(levels(p1$data$main), c("8", "6", "4"))
    testthat::expect_identical(levels(p1$data$condition), c("0"))
    testthat::expect_identical(
      colnames(p1$data),
      c("condition", "main", "counts", "perc", "slice.label")
    )

    # checking plot labels
    testthat::expect_identical(p$labels$subtitle, p_subtitle)
    testthat::expect_identical(pb$plot$plot_env$facet.wrap.name, "cylinders")
    testthat::expect_identical(
      pb$plot$plot_env$legend.labels, c("0 = automatic", "1 = manual")
    )
    testthat::expect_identical(pb$plot$labels$caption, ggplot2::expr(atop(
      displaystyle(NULL),
      expr = paste(
        "In favor of null: ",
        "log"["e"],
        "(BF"["01"],
        ") = ",
        "-2.82",
        ", sampling = ",
        "independent multinomial",
        ", ",
        italic("a"),
        " = ",
        "1.00"
      )
    )))
    testthat::expect_null(p$labels$x, NULL)
    testthat::expect_null(p$labels$y, NULL)
    testthat::expect_null(pb$plot$plot_env$stat.title, NULL)
    testthat::expect_identical(pb$plot$guides$fill$title[1], "transmission")
    testthat::expect_null(pb1$plot$labels$subtitle, NULL)
    testthat::expect_null(pb1$plot$labels$caption, NULL)
    testthat::expect_identical(
      pb1$layout$facet_params$plot_env$facet.wrap.name,
      "transmission"
    )

    # checking labels
    testthat::expect_identical(
      pb$data[[2]]$label,
      c("n = 3", "n = 8", "n = 4", "n = 3", "n = 12", "n = 2")
    )
    testthat::expect_identical(pb$data[[3]]$label, c("ns", "ns", "**"))
    testthat::expect_identical(
      pb$data[[4]]$label,
      c("(n = 11)", "(n = 7)", "(n = 14)")
    )
    testthat::expect_identical(
      dplyr::arrange(pb$data[[2]], group, PANEL)$label,
      dat$slice.label
    )

    # check if palette changed
    testthat::expect_identical(
      pb$data[[1]]$fill,
      c(
        "#F5CDB4",
        "#9A8822",
        "#F5CDB4",
        "#9A8822",
        "#F5CDB4",
        "#9A8822"
      )
    )

    # test layout
    df_layout <- tibble::as_tibble(pb$layout$layout)
    testthat::expect_equal(dim(df_layout), c(3L, 6L))
    testthat::expect_identical(class(df_layout$condition), "factor")
    testthat::expect_identical(levels(df_layout$condition), c("4", "6", "8"))
  }
)

# contingency tab (with counts) ----------------------------------------------

testthat::test_that(
  desc = "checking labels with counts",
  code = {
    testthat::skip_on_cran()

    # plot
    set.seed(123)
    p <- ggstatsplot::ggpiestats(
      data = as.data.frame(Titanic),
      main = Sex,
      condition = Survived,
      nboot = 25,
      bf.message = FALSE,
      counts = "Freq",
      perc.k = 2,
      ggtheme = ggplot2::theme_minimal(),
      conf.level = 0.95,
      messages = TRUE
    )

    # subtitle
    set.seed(123)
    p_subtitle <- ggstatsplot::subtitle_contingency_tab(
      data = as.data.frame(Titanic),
      main = Sex,
      condition = Survived,
      counts = Freq,
      nboot = 25,
      conf.level = 0.95,
      messages = FALSE
    )

    # checking data used to create a plot
    dat <- p$data %>%
      dplyr::mutate_if(
        .tbl = .,
        .predicate = is.factor,
        .funs = ~ as.character(.)
      )

    # checking dimensions of data
    data_dims <- dim(dat)

    # testing everything is okay with data
    testthat::expect_equal(data_dims, c(4L, 5L))
    testthat::expect_equal(dat$perc, c(8.46, 48.38, 91.54, 51.62), tolerance = 1e-3)
    testthat::expect_equal(dat$condition[1], "No")
    testthat::expect_equal(dat$condition[4], "Yes")
    testthat::expect_equal(dat$main[2], "Female")
    testthat::expect_equal(dat$main[3], "Male")
    testthat::expect_identical(dat$counts, c(126L, 344L, 1364L, 367L))

    # checking plot labels
    testthat::expect_identical(p$labels$subtitle, p_subtitle)
    testthat::expect_null(p$labels$caption, NULL)
  }
)

# mcnemar test ---------------------------------------------------------

testthat::test_that(
  desc = "checking labels with contingency tab (paired)",
  code = {
    testthat::skip_on_cran()

    # data
    set.seed(123)
    survey.data <- data.frame(
      `1st survey` = c("Approve", "Approve", "Disapprove", "Disapprove"),
      `2nd survey` = c("Approve", "Disapprove", "Approve", "Disapprove"),
      `Counts` = c(794, 150, 86, 570),
      check.names = FALSE
    )

    # plot
    set.seed(123)
    p <- ggstatsplot::ggpiestats(
      data = survey.data,
      main = `1st survey`,
      condition = `2nd survey`,
      counts = Counts,
      nboot = 25,
      paired = TRUE,
      conf.level = 0.90,
      messages = FALSE
    )

    # subtitle
    set.seed(123)
    p_subtitle <- ggstatsplot::subtitle_contingency_tab(
      data = survey.data,
      main = `1st survey`,
      condition = `2nd survey`,
      counts = Counts,
      nboot = 25,
      paired = TRUE,
      conf.level = 0.90,
      messages = FALSE
    )

    # checking plot labels
    testthat::expect_identical(p$labels$subtitle, p_subtitle)
  }
)

# without enough data ---------------------------------------------------------

testthat::test_that(
  desc = "checking if functions work without enough data",
  code = {
    testthat::skip_on_cran()
    set.seed(123)

    # creating a dataframe
    df <- tibble::tribble(
      ~x, ~y,
      "one", "one"
    )

    # subtitle
    p1 <- ggstatsplot::ggpiestats(data = df, main = x)

    # expected output
    p_subtitle1 <- ggplot2::expr(paste(italic("n"), " = ", 1L))

    # testing overall call
    testthat::expect_identical(p1$labels$subtitle, p_subtitle1)
    testthat::expect_error(ggstatsplot::ggpiestats(
      data = df,
      main = x,
      condition = y
    ))
  }
)

# subtitle return --------------------------------------------------

testthat::test_that(
  desc = "subtitle return",
  code = {
    testthat::skip_on_cran()

    # subtitle return
    set.seed(123)
    p_sub <- ggstatsplot::ggpiestats(
      data = dplyr::sample_frac(tbl = forcats::gss_cat, size = 0.1),
      main = race,
      condition = marital,
      return = "subtitle",
      k = 4,
      messages = FALSE
    )

    # caption return
    set.seed(123)
    p_cap <- ggstatsplot::ggpiestats(
      data = dplyr::sample_frac(tbl = forcats::gss_cat, size = 0.1),
      main = race,
      condition = marital,
      return = "caption",
      k = 4,
      messages = FALSE
    )

    # tests
    testthat::expect_identical(p_sub, ggplot2::expr(
      paste(
        NULL,
        italic(chi)^2,
        "(",
        "8",
        ") = ",
        "109.2007",
        ", ",
        italic("p"),
        " = ",
        "< 0.001",
        ", ",
        italic("V")["Cramer"],
        " = ",
        "0.1594",
        ", CI"["95%"],
        " [",
        "0.0916",
        ", ",
        "0.1278",
        "]",
        ", ",
        italic("n"),
        " = ",
        2148L
      )
    ))

    testthat::expect_identical(
      p_cap,
      ggplot2::expr(atop(
        displaystyle(NULL),
        expr = paste(
          "In favor of null: ",
          "log"["e"],
          "(BF"["01"],
          ") = ",
          "-36.8983",
          ", sampling = ",
          "independent multinomial",
          ", ",
          italic("a"),
          " = ",
          "1.0000"
        )
      ))
    )
  }
)
