# one sample proportion test -----------------------------------------

testthat::test_that(
  desc = "checking one sample proportion test",
  code = {
    testthat::skip_on_cran()

    # creating the plot
    set.seed(123)
    p <-
      ggstatsplot::ggpiestats(
        data = ggplot2::msleep,
        main = vore,
        bf.message = TRUE,
        title = "mammalian sleep",
        legend.title = "vore",
        caption = "From ggplot2 package",
        perc.k = 2,
        nboot = 25,
        ggstatsplot.layer = FALSE,
        label = "both",
        messages = FALSE
      )

    # built plot
    pb <- ggplot2::ggplot_build(p)

    # subtitle used
    set.seed(123)
    p_subtitle <-
      statsExpressions::expr_onesample_proptest(
        data = ggplot2::msleep,
        x = "vore",
        nboot = 25
      )

    # checking geom data
    testthat::expect_equal(
      pb$data[[1]],
      structure(
        list(
          fill = c(
            "#1B9E77FF", "#D95F02FF", "#7570B3FF",
            "#E7298AFF"
          ),
          y = c(
            1, 0.736842105263158, 0.671052631578947,
            0.25
          ),
          x = c(1L, 1L, 1L, 1L),
          PANEL = structure(c(
            1L, 1L, 1L,
            1L
          ), .Label = "1", class = "factor"),
          group = 1:4,
          flipped_aes = c(
            FALSE,
            FALSE, FALSE, FALSE
          ),
          ymin = c(
            0.736842105263158, 0.671052631578947,
            0.25, 0
          ),
          ymax = c(1, 0.736842105263158, 0.671052631578947, 0.25),
          xmin = c(0.5, 0.5, 0.5, 0.5),
          xmax = c(1.5, 1.5, 1.5, 1.5),
          colour = c("black", "black", "black", "black"),
          size = c(
            0.5,
            0.5, 0.5, 0.5
          ),
          linetype = c(1, 1, 1, 1),
          alpha = c(
            NA, NA,
            NA, NA
          )
        ),
        row.names = c(NA, -4L),
        class = "data.frame"
      )
    )

    testthat::expect_equal(
      pb$data[[2]],
      structure(
        list(
          y = c(
            0.868421052631579,
            0.703947368421053,
            0.460526315789474,
            0.125
          ),
          x = c(1L, 1L, 1L, 1L),
          label = c(
            "n = 20\n(26.32%)",
            "n = 5\n(6.58%)",
            "n = 32\n(42.11%)",
            "n = 19\n(25%)"
          ),
          group = 1:4,
          PANEL = structure(c(1L, 1L, 1L, 1L), .Label = "1", class = "factor"),
          ymax = c(1, 0.736842105263158, 0.671052631578947, 0.25),
          xmin = c(1L, 1L, 1L, 1L),
          xmax = c(1L, 1L, 1L, 1L),
          ymin = c(
            0.736842105263158,
            0.671052631578947, 0.25, 0
          ),
          colour = c(
            "black", "black",
            "black", "black"
          ),
          fill = c("white", "white", "white", "white"),
          size = c(3.88, 3.88, 3.88, 3.88),
          angle = c(0, 0, 0, 0),
          hjust = c(0.5, 0.5, 0.5, 0.5),
          vjust = c(
            0.5, 0.5, 0.5,
            0.5
          ),
          alpha = c(1, 1, 1, 1),
          family = c("", "", "", ""),
          fontface = c(1, 1, 1, 1),
          lineheight = c(1.2, 1.2, 1.2, 1.2)
        ),
        row.names = c(NA, -4L),
        class = "data.frame"
      )
    )

    # checking plot labels
    testthat::expect_identical(p$labels$subtitle, p_subtitle)
    testthat::expect_identical(p$labels$title, "mammalian sleep")
    testthat::expect_identical(
      p$labels$caption,
      ggplot2::expr(atop(
        displaystyle("From ggplot2 package"),
        expr = paste(
          "In favor of null: ",
          "log"["e"],
          "(BF"["01"],
          ") = ",
          "-3.65",
          ", ",
          italic("a"),
          " = ",
          "1.00"
        )
      ))
    )
    testthat::expect_null(p$labels$x, NULL)
    testthat::expect_null(p$labels$y, NULL)
    testthat::expect_identical(pb$plot$plot_env$legend.title, "vore")
  }
)

# contingency tab ---------------------------------------------------------

testthat::test_that(
  desc = "checking labels with contingency tab",
  code = {
    testthat::skip_on_cran()

    # creating the plot
    set.seed(123)
    p <-
      suppressWarnings(
        ggstatsplot::ggpiestats(
          data = mtcars,
          main = "am",
          condition = "cyl",
          perc.k = 2,
          nboot = 25,
          package = "wesanderson",
          palette = "Royal2",
          ggtheme = ggplot2::theme_bw(),
          label = "counts",
          legend.title = "transmission",
          factor.levels = c("0 = automatic", "1 = manual"),
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
        messages = FALSE
      )

    # build plot
    pb <- ggplot2::ggplot_build(p)
    pb1 <- ggplot2::ggplot_build(p1)

    # subtitle used
    set.seed(123)
    p_subtitle <-
      suppressWarnings(statsExpressions::expr_contingency_tab(
        data = mtcars,
        x = "am",
        y = "cyl",
        nboot = 25,
        messages = FALSE
      ))

    # with facets
    testthat::expect_equal(length(pb$data), 3L)
    testthat::expect_equal(dim(pb$data[[1]]), c(6L, 14L))
    testthat::expect_equal(dim(pb$data[[2]]), c(6L, 19L))
    testthat::expect_equal(dim(pb$data[[3]]), c(3L, 18L))

    # without facets
    testthat::expect_equal(length(pb1$data), 3L)
    testthat::expect_equal(dim(pb1$data[[1]]), c(3L, 14L))
    testthat::expect_equal(dim(pb1$data[[2]]), c(3L, 19L))
    testthat::expect_equal(dim(pb1$data[[3]]), c(1L, 18L))

    # check geoms
    testthat::expect_equal(
      pb$data[[2]]$y,
      c(
        0.636363636363636,
        0.136363636363636,
        0.785714285714286,
        0.285714285714286,
        0.928571428571429,
        0.428571428571429
      ),
      tolerance = 0.001
    )
    testthat::expect_equal(
      pb1$data[[2]]$y,
      c(0.684210526315789, 0.263157894736842, 0.0789473684210526),
      tolerance = 0.001
    )
    testthat::expect_equal(
      unique(pb$data[[3]]$x),
      unique(pb1$data[[3]]$x),
      tolerance = 0.001
    )
    testthat::expect_equal(
      unique(pb$data[[3]]$y),
      unique(pb1$data[[3]]$y),
      tolerance = 0.001
    )

    # checking plot labels
    testthat::expect_identical(p$labels$subtitle, p_subtitle)
    testthat::expect_identical(
      pb$plot$plot_env$legend.labels, c("0 = automatic", "1 = manual")
    )
    testthat::expect_identical(
      pb$plot$labels$caption,
      ggplot2::expr(atop(
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
      ))
    )
    testthat::expect_null(p$labels$x, NULL)
    testthat::expect_null(p$labels$y, NULL)
    testthat::expect_null(pb$plot$plot_env$stat.title, NULL)
    testthat::expect_identical(pb$plot$guides$fill$title[1], "transmission")
    testthat::expect_null(pb1$plot$labels$subtitle, NULL)
    testthat::expect_null(pb1$plot$labels$caption, NULL)

    # checking labels
    testthat::expect_identical(
      pb$data[[2]]$label,
      c("n = 8", "n = 3", "n = 3", "n = 4", "n = 2", "n = 12")
    )
    testthat::expect_identical(
      pb$data[[3]]$label,
      c(
        "list(~chi['gof']^2~ ( 1 )== 2.27 , ~italic(p) == 0.132 , ~italic(n) == 11 )",
        "list(~chi['gof']^2~ ( 1 )== 0.14 , ~italic(p) == 0.705 , ~italic(n) == 7 )",
        "list(~chi['gof']^2~ ( 1 )== 7.14 , ~italic(p) == 0.008 , ~italic(n) == 14 )"
      )
    )

    # check if palette changed
    testthat::expect_identical(
      pb$data[[1]]$fill,
      c(
        "#9A8822FF",
        "#F5CDB4FF",
        "#9A8822FF",
        "#F5CDB4FF",
        "#9A8822FF",
        "#F5CDB4FF"
      )
    )
    testthat::expect_identical(
      pb1$data[[1]]$fill,
      c("#1B9E77FF", "#D95F02FF", "#7570B3FF")
    )
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
      legend.title = NULL,
      ggtheme = ggplot2::theme_minimal(),
      conf.level = 0.95,
      messages = TRUE
    )

    # subtitle
    set.seed(123)
    p_subtitle <- statsExpressions::expr_contingency_tab(
      data = as.data.frame(Titanic),
      x = Sex,
      y = Survived,
      counts = Freq,
      nboot = 25,
      conf.level = 0.95,
      messages = FALSE
    )

    # build the plot
    pb <- ggplot2::ggplot_build(p)

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
    testthat::expect_equal(dat$Survived[1], "No")
    testthat::expect_equal(dat$Survived[4], "Yes")
    testthat::expect_equal(dat$Sex[2], "Female")
    testthat::expect_equal(dat$Sex[3], "Male")
    testthat::expect_identical(dat$counts, c(126L, 344L, 1364L, 367L))

    # checking plot labels
    testthat::expect_identical(p$labels$subtitle, p_subtitle)
    testthat::expect_null(p$labels$caption, NULL)
    testthat::expect_identical(pb$plot$plot_env$legend.title, "Sex")
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

    # build the plot
    pb <- ggplot2::ggplot_build(p)

    # subtitle
    set.seed(123)
    p_subtitle <-
      statsExpressions::expr_contingency_tab(
        data = survey.data,
        x = `1st survey`,
        y = `2nd survey`,
        counts = Counts,
        nboot = 25,
        paired = TRUE,
        conf.level = 0.90,
        messages = FALSE
      )

    # checking plot labels
    testthat::expect_identical(p$labels$subtitle, p_subtitle)
    testthat::expect_identical(pb$plot$labels$group, "1st survey")
    testthat::expect_identical(pb$plot$labels$fill, "1st survey")
    testthat::expect_identical(pb$plot$labels$label, "label")
    testthat::expect_null(pb$plot$labels$x, NULL)
    testthat::expect_null(pb$plot$labels$y, NULL)
    testthat::expect_null(pb$plot$labels$title, NULL)

    # labels
    testthat::expect_identical(
      pb$data[[3]]$label,
      c(
        "list(~chi['gof']^2~ ( 1 )== 569.62 , ~italic(p) <= 0.001 , ~italic(n) == 880 )",
        "list(~chi['gof']^2~ ( 1 )== 245.00 , ~italic(p) <= 0.001 , ~italic(n) == 720 )"
      )
    )
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
    testthat::expect_null(ggstatsplot::ggpiestats(
      data = df,
      main = x,
      output = "subtitle"
    ))
  }
)

# subtitle output --------------------------------------------------

testthat::test_that(
  desc = "subtitle output",
  code = {
    testthat::skip_on_cran()

    # subtitle output
    set.seed(123)
    p_sub <-
      ggstatsplot::ggpiestats(
        data = dplyr::sample_frac(tbl = forcats::gss_cat, size = 0.1),
        main = race,
        condition = marital,
        output = "subtitle",
        k = 4,
        messages = FALSE
      )

    set.seed(123)
    stats_output <-
      statsExpressions::expr_contingency_tab(
        data = dplyr::sample_frac(tbl = forcats::gss_cat, size = 0.1),
        x = race,
        y = marital,
        k = 4,
        messages = FALSE
      )

    # caption output
    set.seed(123)
    p_cap <-
      ggstatsplot::ggpiestats(
        data = dplyr::sample_frac(tbl = forcats::gss_cat, size = 0.1),
        main = race,
        condition = marital,
        output = "caption",
        k = 4,
        messages = FALSE
      )

    # tests
    testthat::expect_identical(p_sub, stats_output)
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
