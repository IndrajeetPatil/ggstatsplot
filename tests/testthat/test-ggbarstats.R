# contingency tab (with counts) ----------------------------------------------

testthat::test_that(
  desc = "checking labels with counts",
  code = {
    testthat::skip_on_cran()

    # condition variable is not optional for `ggbarstats`
    testthat::expect_error(
      ggstatsplot::ggbarstats(
        data = as.data.frame(Titanic),
        main = Sex
      )
    )

    # plot
    set.seed(123)
    p <-
      ggstatsplot::ggbarstats(
        data = as.data.frame(Titanic),
        main = Sex,
        condition = Survived,
        counts = "Freq",
        perc.k = 2,
        conf.level = 0.95,
        xlab = "Passenger sex",
        ylab = "proportion",
        label.separator = "\n",
        bf.message = FALSE,
        messages = TRUE
      )

    # build plot
    pb <- ggplot2::ggplot_build(p)

    # subtitle
    set.seed(123)
    p_subtitle <-
      statsExpressions::expr_contingency_tab(
        data = as.data.frame(Titanic),
        x = "Sex",
        y = "Survived",
        counts = Freq,
        conf.level = 0.95,
        messages = FALSE
      )

    # checking geom data
    testthat::expect_equal(
      pb$data[[1]],
      structure(
        list(
          fill = c(
            "#1B9E77FF", "#1B9E77FF", "#D95F02FF",
            "#D95F02FF"
          ),
          y = c(1, 1, 0.915436241610738, 0.516174402250352),
          x = c(1L, 2L, 1L, 2L),
          PANEL = structure(c(1L, 1L, 1L, 1L), .Label = "1", class = "factor"),
          group = c(1L, 3L, 2L, 4L),
          flipped_aes = c(
            FALSE, FALSE,
            FALSE, FALSE
          ),
          ymin = c(
            0.915436241610738, 0.516174402250352,
            0, 0
          ),
          ymax = c(1, 1, 0.915436241610738, 0.516174402250352),
          xmin = c(0.55, 1.55, 0.55, 1.55),
          xmax = c(
            1.45, 2.45,
            1.45, 2.45
          ),
          colour = c("black", "black", "black", "black"),
          size = c(0.5, 0.5, 0.5, 0.5),
          linetype = c(1, 1, 1, 1),
          alpha = c(NA, NA, NA, NA)
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
            0.957718120805369,
            0.758087201125176,
            0.457718120805369,
            0.258087201125176
          ),
          x = c(1L, 2L, 1L, 2L),
          label = c(
            "8.46%",
            "48.38%", "91.54%", "51.62%"
          ),
          group = c(1L, 1L, 2L, 2L),
          PANEL = structure(c(
            1L,
            1L, 1L, 1L
          ), .Label = "1", class = "factor"),
          ymax = c(
            1, 1,
            0.915436241610738, 0.516174402250352
          ),
          xmin = c(1L, 2L, 1L, 2L),
          xmax = c(1L, 2L, 1L, 2L),
          ymin = c(
            0.915436241610738, 0.516174402250352,
            0, 0
          ),
          colour = c("black", "black", "black", "black"),
          fill = c(
            "white",
            "white", "white", "white"
          ),
          size = c(3.88, 3.88, 3.88, 3.88),
          angle = c(0, 0, 0, 0),
          hjust = c(0.5, 0.5, 0.5, 0.5),
          vjust = c(
            0.5,
            0.5, 0.5, 0.5
          ),
          alpha = c(1, 1, 1, 1),
          family = c(
            "", "",
            "", ""
          ),
          fontface = c(1, 1, 1, 1),
          lineheight = c(
            1.2, 1.2,
            1.2, 1.2
          )
        ),
        row.names = c(NA, -4L),
        class = "data.frame"
      )
    )

    testthat::expect_equal(
      pb$data[[3]],
      structure(
        list(
          y = c(1.05, 1.05),
          x = 2:1,
          label = c("ns", "***"),
          PANEL = structure(c(1L, 1L), class = "factor", .Label = "1"),
          group = structure(2:1, n = 2L),
          colour = c("black", "black"),
          size = c(5, 5),
          angle = c(0, 0),
          hjust = c(0.5, 0.5),
          vjust = c(0.5, 0.5),
          alpha = c(NA, NA),
          family = c("", ""),
          fontface = c(1, 1),
          lineheight = c(1.2, 1.2)
        ),
        row.names = c(NA, -2L),
        class = "data.frame"
      )
    )

    testthat::expect_equal(
      pb$data[[4]],
      structure(
        list(
          y = c(-0.05, -0.05),
          x = 2:1,
          label = c("(n = 711)", "(n = 1490)"),
          PANEL = structure(c(1L, 1L), class = "factor", .Label = "1"),
          group = structure(2:1, n = 2L),
          colour = c("black", "black"),
          size = c(4, 4),
          angle = c(0, 0),
          hjust = c(0.5, 0.5),
          vjust = c(0.5, 0.5),
          alpha = c(NA, NA),
          family = c("", ""),
          fontface = c(1, 1),
          lineheight = c(1.2, 1.2)
        ),
        row.names = c(NA, -2L),
        class = "data.frame"
      )
    )

    # checking plot labels
    testthat::expect_identical(pb$plot$labels$subtitle, p_subtitle)
    testthat::expect_identical(pb$plot$labels$caption, NULL)
    testthat::expect_identical(pb$plot$labels$x, "Passenger sex")
    testthat::expect_identical(pb$plot$labels$y, "proportion")

    # checking data layers
    testthat::expect_equal(length(pb$data), 4L)
    testthat::expect_equal(unique(pb$data[[3]]$y), 1.05, tolerance = 0.01)
    testthat::expect_equal(unique(pb$data[[4]]$y), -0.05, tolerance = 0.01)
    testthat::expect_identical(
      pb$data[[2]]$label,
      c("8.46%", "48.38%", "91.54%", "51.62%")
    )
    testthat::expect_identical(pb$data[[3]]$label, c("ns", "***"))
    testthat::expect_identical(pb$data[[4]]$label, c("(n = 711)", "(n = 1490)"))

    # checking geoms data
    testthat::expect_equal(
      pb$data[[3]]$y + pb$data[[4]]$y,
      c(1, 1),
      tolerance = 0.001
    )
  }
)

# aesthetic modifications --------------------------------------------------

testthat::test_that(
  desc = "aesthetic modifications",
  code = {
    testthat::skip_on_cran()

    # plot
    set.seed(123)
    p <- suppressWarnings(ggstatsplot::ggbarstats(
      data = mtcars,
      main = vs,
      condition = "cyl",
      bf.message = TRUE,
      nboot = 10,
      label = "both",
      package = "wesanderson",
      palette = "Royal2",
      labels.legend = c("0 = V-shaped", "1 = straight"),
      legend.title = "Engine",
      x.axis.orientation = "slant",
      label.separator = "\n",
      messages = FALSE
    ))

    p1 <- suppressWarnings(ggstatsplot::ggbarstats(
      data = mtcars,
      main = vs,
      condition = cyl,
      label = "counts",
      bf.message = FALSE,
      nboot = 10,
      x.axis.orientation = "vertical",
      messages = FALSE
    ))

    # build plot
    pb <- ggplot2::ggplot_build(p)
    pb1 <- ggplot2::ggplot_build(p1)

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
    testthat::expect_equal(data_dims, c(5L, 5L))
    testthat::expect_identical(
      pb$data[[2]]$label,
      c(
        "n = 10\n(91%)",
        "n = 4\n(57%)",
        "n = 1\n(9%)",
        "n = 3\n(43%)",
        "n = 14\n(100%)"
      )
    )
    testthat::expect_identical(
      pb1$data[[2]]$label,
      c("n = 10", "n = 4", "n = 1", "n = 3", "n = 14")
    )

    # checking layered data
    testthat::expect_identical(pb$plot$guides$fill$title, "Engine")
    testthat::expect_equal(pb$plot$theme$axis.text.x$angle, 45L)
    testthat::expect_equal(pb1$plot$theme$axis.text.x$angle, 90L)
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
      ggstatsplot::ggbarstats(
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
      ggstatsplot::ggbarstats(
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

# proptest output ---------------------------------------------------------

testthat::test_that(
  desc = "proptest output",
  code = {
    testthat::skip_on_cran()

    df <-
      suppressWarnings(ggbarstats(
        mtcars,
        am,
        cyl,
        results.subtitle = FALSE,
        output = "proptest",
        messages = FALSE
      ))

    # tests
    testthat::expect_equal(dim(df), c(3L, 12L))
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

    # should not work
    testthat::expect_output(
      suppressWarnings(ggstatsplot::ggbarstats(
        data = df,
        main = x,
        condition = y
      ))
    )
  }
)
