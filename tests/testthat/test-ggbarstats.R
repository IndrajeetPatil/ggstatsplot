# contingency tab (with counts) ----------------------------------------------

testthat::test_that(
  desc = "checking labels with counts",
  code = {
    testthat::skip_on_cran()

    # y variable is not optional for `ggbarstats`
    testthat::expect_error(
      ggstatsplot::ggbarstats(
        data = as.data.frame(Titanic),
        x = Sex
      )
    )

    # plot
    set.seed(123)
    p <-
      ggstatsplot::ggbarstats(
        data = as.data.frame(Titanic),
        x = Sex,
        y = Survived,
        counts = "Freq",
        perc.k = 2,
        conf.level = 0.95,
        xlab = "Passenger sex",
        ylab = "proportion",
        label.separator = "\n",
        bf.message = FALSE
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
        counts = Freq
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
          x = structure(c(1L, 2L, 1L, 2L), class = c(
            "mapped_discrete",
            "numeric"
          )),
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
          xmin = structure(c(0.55, 1.55, 0.55, 1.55), class = c(
            "mapped_discrete",
            "numeric"
          )), xmax = structure(c(1.45, 2.45, 1.45, 2.45), class = c(
            "mapped_discrete",
            "numeric"
          )),
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
          x = structure(c(1L, 2L, 1L, 2L), class = c(
            "mapped_discrete",
            "numeric"
          )),
          label = c(
            "8.46%", "48.38%", "91.54%", "51.62%"
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
          xmin = structure(c(1L, 2L, 1L, 2L), class = c(
            "mapped_discrete",
            "numeric"
          )),
          xmax = structure(c(1L, 2L, 1L, 2L), class = c(
            "mapped_discrete",
            "numeric"
          )),
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
          x = structure(2:1, class = c(
            "mapped_discrete",
            "numeric"
          )),
          label = c("list(~italic(p)=='0.388')", "list(~italic(p)=='1.08e-225')"),
          PANEL = structure(c(1L, 1L), class = "factor", .Label = "1"),
          group = structure(2:1, n = 2L),
          colour = c("black", "black"),
          size = c(2.8, 2.8),
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
          x = structure(2:1, class = c(
            "mapped_discrete",
            "numeric"
          )),
          label = c("(n = 711)", "(n = 1,490)"),
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
    p <-
      suppressWarnings(ggstatsplot::ggbarstats(
        data = mtcars,
        x = vs,
        y = "cyl",
        bf.message = TRUE,
        label = "both",
        package = "wesanderson",
        palette = "Royal2",
        legend.title = "Engine"
      ))

    p1 <-
      suppressWarnings(ggstatsplot::ggbarstats(
        data = mtcars,
        x = vs,
        y = cyl,
        label = "counts",
        bf.message = FALSE
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

    # testing everything is okay with data
    testthat::expect_equal(dim(dat), c(5L, 5L))
    testthat::expect_identical(
      pb$data[[2]]$label,
      c(
        "10\n(91%)",
        "4\n(57%)",
        "1\n(9%)",
        "3\n(43%)",
        "14\n(100%)"
      )
    )
    testthat::expect_identical(
      pb1$data[[2]]$label,
      c("10", "4", "1", "3", "14")
    )

    # checking layered data
    testthat::expect_identical(pb$plot$guides$fill$title, "Engine")
  }
)

# dropped factor levels --------------------------------------------------

testthat::test_that(
  desc = "dropped factor levels",
  code = {
    testthat::skip_on_cran()

    # dropped level dataset
    mtcars_small <- dplyr::filter(.data = mtcars, am == "0")

    set.seed(123)
    p <-
      ggstatsplot::ggbarstats(
        data = mtcars_small,
        x = cyl,
        y = am,
        results.subtitle = NULL
      )

    pb <- ggplot2::ggplot_build(p)

    # data
    testthat::expect_equal(length(pb$data), 3L)

    testthat::expect_equal(
      pb$data[[1]],
      structure(
        list(
          fill = c("#1B9E77FF", "#D95F02FF", "#7570B3FF"),
          y = c(1, 0.368421052631579, 0.157894736842105),
          x = structure(c(
            1L,
            1L, 1L
          ), class = c("mapped_discrete", "numeric")),
          PANEL = structure(c(
            1L,
            1L, 1L
          ), .Label = "1", class = "factor"),
          group = 1:3,
          flipped_aes = c(
            FALSE,
            FALSE, FALSE
          ),
          ymin = c(
            0.368421052631579, 0.157894736842105,
            0
          ),
          ymax = c(1, 0.368421052631579, 0.157894736842105),
          xmin = structure(c(
            0.55,
            0.55, 0.55
          ), class = c("mapped_discrete", "numeric")),
          xmax = structure(c(
            1.45,
            1.45, 1.45
          ), class = c("mapped_discrete", "numeric")),
          colour = c(
            "black",
            "black", "black"
          ),
          size = c(0.5, 0.5, 0.5),
          linetype = c(
            1, 1,
            1
          ),
          alpha = c(NA, NA, NA)
        ),
        row.names = c(NA, -3L),
        class = "data.frame"
      )
    )

    testthat::expect_equal(
      pb$data[[2]],
      structure(
        list(
          y = c(0.684210526315789, 0.263157894736842, 0.0789473684210526),
          x = structure(c(1L, 1L, 1L), class = c(
            "mapped_discrete",
            "numeric"
          )),
          label = c("63%", "21%", "16%"),
          group = 1:3,
          PANEL = structure(c(
            1L,
            1L, 1L
          ), .Label = "1", class = "factor"),
          ymax = c(
            1, 0.368421052631579,
            0.157894736842105
          ),
          xmin = structure(c(1L, 1L, 1L), class = c(
            "mapped_discrete",
            "numeric"
          )),
          xmax = structure(c(1L, 1L, 1L), class = c(
            "mapped_discrete",
            "numeric"
          )),
          ymin = c(0.368421052631579, 0.157894736842105, 0),
          colour = c("black", "black", "black"),
          fill = c(
            "white", "white",
            "white"
          ),
          size = c(3.88, 3.88, 3.88),
          angle = c(0, 0, 0),
          hjust = c(
            0.5,
            0.5, 0.5
          ),
          vjust = c(0.5, 0.5, 0.5),
          alpha = c(1, 1, 1),
          family = c(
            "",
            "", ""
          ),
          fontface = c(1, 1, 1),
          lineheight = c(1.2, 1.2, 1.2)
        ),
        row.names = c(
          NA,
          -3L
        ),
        class = "data.frame"
      )
    )

    testthat::expect_equal(
      pb$data[[3]],
      structure(
        list(
          y = -0.05,
          x = structure(1L, class = c(
            "mapped_discrete",
            "numeric"
          )),
          label = "(n = 19)",
          PANEL = structure(1L, .Label = "1", class = "factor"),
          group = structure(1L, n = 1L),
          colour = "black",
          size = 4,
          angle = 0,
          hjust = 0.5,
          vjust = 0.5,
          alpha = NA,
          family = "",
          fontface = 1,
          lineheight = 1.2
        ),
        row.names = c(NA, -1L),
        class = "data.frame"
      )
    )

    testthat::expect_equal(
      pb$plot$labels,
      list(
        x = "am",
        y = NULL,
        title = NULL,
        subtitle = NULL,
        caption = NULL,
        fill = "cyl",
        label = ".label",
        group = "cyl"
      )
    )
  }
)

# other outputs --------------------------------------------------

testthat::test_that(
  desc = "other outputs",
  code = {
    testthat::skip_on_cran()

    set.seed(123)
    df <- dplyr::sample_frac(tbl = forcats::gss_cat, size = 0.1) %>%
      dplyr::mutate_if(., is.factor, droplevels)


    # subtitle output
    set.seed(123)
    p_sub <-
      ggstatsplot::ggbarstats(
        data = df,
        x = race,
        y = marital,
        output = "subtitle",
        k = 4
      )

    set.seed(123)
    stats_output <-
      statsExpressions::expr_contingency_tab(
        data = df,
        x = race,
        y = marital,
        k = 4
      )

    # tests
    testthat::expect_identical(p_sub, stats_output)

    testthat::expect_null(ggstatsplot::ggbarstats(
      data = mtcars,
      x = cyl,
      y = am,
      paired = TRUE,
      output = "caption"
    ))
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
    testthat::expect_s3_class(
      suppressWarnings(ggstatsplot::ggbarstats(
        data = df,
        x = x,
        y = y
      )),
      "ggplot"
    )
  }
)
