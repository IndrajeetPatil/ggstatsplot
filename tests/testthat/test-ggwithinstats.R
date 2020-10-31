# for t-test
data_bugs_2 <- ggstatsplot::bugs_long %>%
  dplyr::filter(.data = ., condition %in% c("HDLF", "HDHF"))

# basic plotting works - two groups ---------------------------------

testthat::test_that(
  desc = "basic plotting works - two groups",
  code = {
    testthat::skip_on_cran()

    # plot
    set.seed(123)
    p1 <-
      ggstatsplot::ggwithinstats(
        data = data_bugs_2,
        x = condition,
        y = desire,
        type = "bayes",
        k = 4,
        # mean.ci = TRUE,
        conf.level = 0.99,
        outlier.tagging = TRUE,
        outlier.label = "region",
        outlier.coef = 1.5,
        bf.message = TRUE,
        pairwise.comparisons = TRUE,
        ggsignif.args = list(textsize = 6, tip_length = 0.01),
        pairwise.annotation = "asterisk",
        point.path.args = list(color = "red"),
        mean.path.args = list(color = "blue", size = 2, alpha = 0.8),
        mean.point.args = list(size = 3, color = "darkgreen", alpha = 0.5),
        title = "bugs dataset",
        caption = "From `jmv` package",
        messages = FALSE
      )

    # build the plot
    pb1 <- ggplot2::ggplot_build(p1)

    # subtitle
    set.seed(123)
    p1_subtitle <-
      statsExpressions::expr_t_bayes(
        data = data_bugs_2,
        x = condition,
        y = desire,
        k = 4,
        paired = TRUE,
        conf.level = 0.99,
        messages = FALSE
      )

    # dataframe used for visualization
    testthat::expect_equal(length(pb1$data), 8L)
    testthat::expect_equal(dim(p1$data), c(180L, 6L))
    testthat::expect_equal(dim(pb1$data[[1]]), c(180L, 10L))
    testthat::expect_equal(dim(pb1$data[[2]]), c(2L, 26L))
    testthat::expect_equal(dim(pb1$data[[3]]), c(1024L, 21L))
    testthat::expect_equal(dim(pb1$data[[4]]), c(180L, 8L))
    testthat::expect_equal(dim(pb1$data[[5]]), c(0L, 0L))
    testthat::expect_equal(dim(pb1$data[[6]]), c(2L, 8L))
    testthat::expect_equal(dim(pb1$data[[7]]), c(2L, 13L))
    testthat::expect_equal(dim(pb1$data[[8]]), c(2L, 15L))

    # checking geom data
    testthat::expect_identical(pb1$data[[4]]$colour[[1]], "red")
    testthat::expect_equal(pb1$data[[4]]$linetype[[1]], 1)
    testthat::expect_equal(pb1$data[[4]]$size[[1]], 0.5)
    testthat::expect_equal(
      pb1$data[[2]],
      structure(
        list(
          ymin = c(0, 0),
          lower = c(6, 4.5),
          middle = c(
            8.75,
            8
          ),
          upper = c(10, 9.5),
          ymax = c(10, 10),
          outliers = list(
            numeric(0),
            numeric(0)
          ),
          notchupper = c(9.41618649374214, 8.83273311717767),
          notchlower = c(8.08381350625786, 7.16726688282233),
          x = structure(c(
            1,
            2
          ), class = c("mapped_discrete", "numeric")),
          flipped_aes = c(FALSE, FALSE),
          PANEL = structure(c(1L, 1L), .Label = "1", class = "factor"),
          group = 1:2,
          ymin_final = c(
            0,
            0
          ),
          ymax_final = c(10, 10),
          xmin = structure(c(0.9, 1.9), class = c(
            "mapped_discrete",
            "numeric"
          )),
          xmax = structure(c(1.1, 2.1), class = c(
            "mapped_discrete",
            "numeric"
          )),
          xid = c(1, 2),
          newx = structure(c(1, 2), .Dim = 2L),
          new_width = c(0.2, 0.2),
          weight = c(1, 1),
          colour = c("grey20", "grey20"),
          fill = c(
            "white",
            "white"
          ),
          size = c(0.5, 0.5),
          alpha = c(0.5, 0.5),
          shape = c(
            19,
            19
          ),
          linetype = c("solid", "solid")
        ),
        row.names = c(NA, -2L),
        class = "data.frame"
      )
    )

    testthat::expect_equal(
      pb1$data[[7]],
      structure(
        list(
          x = structure(1:2, class = c(
            "mapped_discrete",
            "numeric"
          )),
          group = 1:2,
          y = c(
            7.86666666666667,
            6.73888888888889
          ),
          ymin = c(NA_real_, NA_real_),
          ymax = c(
            NA_real_,
            NA_real_
          ),
          PANEL = structure(c(1L, 1L), .Label = "1", class = "factor"),
          flipped_aes = c(FALSE, FALSE),
          shape = c(19, 19),
          colour = c(
            "darkgreen",
            "darkgreen"
          ),
          size = c(3, 3),
          fill = c(NA, NA),
          alpha = c(
            0.5,
            0.5
          ),
          stroke = c(0.5, 0.5)
        ),
        row.names = c(NA, -2L),
        class = "data.frame"
      )
    )

    testthat::expect_equal(
      pb1$data[[8]],
      structure(
        list(
          x = structure(1:2, class = c(
            "mapped_discrete",
            "numeric"
          )),
          y = c(7.86666666666667, 6.73888888888889),
          label = c(
            "list(~italic(widehat(mu))==7.8667)",
            "list(~italic(widehat(mu))==6.7389)"
          ),
          PANEL = structure(c(1L, 1L), class = "factor", .Label = "1"),
          group = structure(1:2, n = 2L),
          colour = c("black", "black"),
          fill = c("white", "white"),
          size = c(3, 3),
          angle = c(
            0,
            0
          ),
          alpha = c(NA, NA),
          family = c("", ""),
          fontface = c(
            1,
            1
          ),
          lineheight = c(1.2, 1.2),
          hjust = c(0.5, 0.5),
          vjust = c(
            0.5,
            0.5
          )
        ),
        row.names = c(NA, -2L),
        class = "data.frame"
      )
    )

    testthat::expect_equal(
      pb1$data[[6]],
      structure(
        list(
          x = structure(1:2, class = c(
            "mapped_discrete",
            "numeric"
          )),
          y = c(7.86666666666667, 6.73888888888889),
          group = structure(c(1L, 1L), n = 1L),
          PANEL = structure(c(
            1L,
            1L
          ), class = "factor", .Label = "1"),
          colour = c("blue", "blue"),
          size = c(2, 2),
          linetype = c(1, 1),
          alpha = c(0.8, 0.8)
        ),
        row.names = c(
          NA,
          -2L
        ),
        class = "data.frame"
      )
    )

    # data from difference layers
    testthat::expect_equal(max(pb1$data[[4]]$group), 90L)

    # range of y variable
    testthat::expect_equal(ggplot2::layer_scales(p1)$y$range$range, c(0L, 10L))

    # checking x-axis sample size labels
    testthat::expect_identical(
      ggplot2::layer_scales(p1)$x$labels,
      c("HDHF\n(n = 90)", "HDLF\n(n = 90)")
    )

    # checking plot labels
    testthat::expect_identical(p1$labels$title, "bugs dataset")
    testthat::expect_identical(p1$labels$subtitle, p1_subtitle)
    testthat::expect_identical(p1$labels$x, "condition")
    testthat::expect_identical(p1$labels$y, "desire")
  }
)

# basic plotting works - more than two groups ---------------------------------

testthat::test_that(
  desc = "basic plotting works - more than two groups",
  code = {
    testthat::skip_on_cran()
    if (utils::packageVersion("BayesFactor") >= package_version("0.9.12-4.3")) {
      library(WRS2)

      # plot
      set.seed(123)
      p1 <-
        ggstatsplot::ggwithinstats(
          data = WineTasting,
          x = Wine,
          y = "Taste",
          type = "p",
          sort = "ascending",
          sort.fun = median,
          bf.message = TRUE,
          k = 4,
          conf.level = 0.99,
          outlier.tagging = TRUE,
          outlier.coef = 2.5,
          pairwise.comparisons = TRUE,
          pairwise.annotation = "asterisk",
          title = "wine tasting data",
          caption = "From `WRS2` package",
          messages = TRUE
        )

      # build the plot
      pb1 <- ggplot2::ggplot_build(p1)

      # subtitle
      set.seed(123)
      p1_subtitle <-
        statsExpressions::expr_anova_parametric(
          data = WineTasting,
          x = "Wine",
          y = Taste,
          type = "p",
          k = 4,
          paired = TRUE,
          conf.level = 0.99,
          messages = FALSE
        )

      # dataframe used for visualization
      testthat::expect_equal(length(pb1$data), 8L)
      testthat::expect_equal(dim(pb1$data[[1]]), c(66L, 10L))
      testthat::expect_equal(dim(pb1$data[[2]]), c(3L, 26L))
      testthat::expect_equal(dim(pb1$data[[3]]), c(1536L, 21L))
      testthat::expect_equal(dim(pb1$data[[4]]), c(4L, 15L))
      testthat::expect_equal(dim(pb1$data[[5]]), c(3L, 8L))
      testthat::expect_equal(dim(pb1$data[[6]]), c(3L, 13L))
      testthat::expect_equal(dim(pb1$data[[7]]), c(3L, 15L))
      testthat::expect_equal(dim(pb1$data[[8]]), c(6L, 19L))

      # data from difference layers
      testthat::expect_equal(
        pb1$data[[5]]$x,
        structure(c(1L, 2L, 3L), class = c(
          "mapped_discrete",
          "numeric"
        ))
      )
      testthat::expect_equal(
        pb1$data[[5]]$y,
        c(5.54318181818182, 5.53409090909091, 5.45909090909091),
        tolerance = 0.001
      )

      # checking displayed outlier labels
      testthat::expect_equal(
        ggplot2::layer_grob(p1, i = 4L)$`1`$lab,
        c(5.00, 6.30, 6.30, 6.25),
        tolerance = 0.01
      )

      # range of y variable
      testthat::expect_equal(
        ggplot2::layer_scales(p1)$y$range$range,
        c(4.95000, 6.55875),
        tolerance = 1e-5
      )

      # checking x-axis sample size labels
      testthat::expect_identical(
        ggplot2::layer_scales(p1)$x$labels,
        c("Wine A\n(n = 22)", "Wine B\n(n = 22)", "Wine C\n(n = 22)")
      )

      # checking plot labels
      testthat::expect_identical(p1$labels$title, "wine tasting data")
      testthat::expect_identical(p1$labels$subtitle, p1_subtitle)
      # testthat::expect_identical(
      #   pb1$plot$labels$caption,
      #   ggplot2::expr(atop(
      #     displaystyle(atop(
      #       displaystyle("From `WRS2` package"),
      #       expr = paste(
      #         "log"["e"],
      #         "(BF"["01"],
      #         ") = ",
      #         "-2.1154",
      #         ", ",
      #         widehat(italic(R^"2"))["median"]^"posterior",
      #         " = ",
      #         "0.8930",
      #         ", CI"["95%"]^"HDI",
      #         " [",
      #         "0.8499",
      #         ", ",
      #         "0.9228",
      #         "]",
      #         ", ",
      #         italic("r")["Cauchy"]^"JZS",
      #         " = ",
      #         "0.7070"
      #       )
      #     )),
      #     expr = paste(
      #       "Pairwise test: ",
      #       bold("Student's t-test"),
      #       "; Comparisons shown: ",
      #       bold("only significant")
      #     )
      #   ))
      # )
      testthat::expect_identical(p1$labels$x, "Wine")
      testthat::expect_identical(p1$labels$y, "Taste")
    }
  }
)

# checking subtitle outputs - without NAs ------------------------------------

testthat::test_that(
  desc = "checking subtitle outputs - without NAs",
  code = {
    testthat::skip_on_cran()

    if (utils::packageVersion("BayesFactor") >= package_version("0.9.12-4.3")) {
      set.seed(123)
      p1 <-
        ggstatsplot::ggwithinstats(
          data = iris_long,
          x = condition,
          y = value,
          type = "np",
          ggsignif.args = list(textsize = 6, tip_length = 0.01),
          pairwise.display = "s",
          pairwise.annotation = "p",
          outlier.tagging = FALSE,
          pairwise.comparisons = TRUE,
          conf.level = 0.90
        )

      set.seed(123)
      p1_subtitle <-
        statsExpressions::expr_anova_nonparametric(
          data = iris_long,
          x = condition,
          y = value,
          type = "np",
          paired = TRUE,
          conf.level = 0.90
        )

      set.seed(123)
      p2 <-
        suppressWarnings(ggstatsplot::ggwithinstats(
          data = iris_long,
          x = condition,
          y = value,
          type = "r",
          pairwise.display = "ns",
          outlier.tagging = FALSE,
          pairwise.annotation = "p-value",
          conf.level = 0.90
        ))

      set.seed(123)
      p2_subtitle <-
        statsExpressions::expr_anova_robust(
          data = iris_long,
          x = condition,
          y = value,
          paired = TRUE,
          conf.level = 0.90
        )

      set.seed(123)
      p3 <-
        suppressWarnings(ggstatsplot::ggwithinstats(
          data = ggstatsplot::VR_dilemma,
          x = modality,
          y = score,
          type = "r",
          k = 3,
          nboot = 25,
          pairwise.comparisons = TRUE,
          pairwise.display = "all",
          pairwise.annotation = "p",
          messages = FALSE,
          bf.message = TRUE
        ))

      set.seed(123)
      p3_subtitle <-
        suppressWarnings(statsExpressions::expr_t_robust(
          data = ggstatsplot::VR_dilemma,
          x = modality,
          y = score,
          paired = TRUE,
          k = 3,
          nboot = 25,
          messages = FALSE
        ))

      set.seed(123)
      p4 <-
        ggstatsplot::ggwithinstats(
          data = ggstatsplot::VR_dilemma,
          x = modality,
          y = score,
          type = "np",
          k = 4,
          nboot = 15,
          conf.level = 0.50,
          pairwise.comparisons = TRUE,
          pairwise.display = "all",
          pairwise.annotation = "p",
          messages = FALSE,
          bf.message = TRUE
        )

      set.seed(123)
      p4_subtitle <-
        statsExpressions::expr_t_nonparametric(
          data = ggstatsplot::VR_dilemma,
          x = modality,
          y = score,
          conf.level = 0.50,
          paired = TRUE,
          k = 4,
          nboot = 15,
          messages = FALSE
        )

      # built plots
      pb1 <- ggplot2::ggplot_build(p1)
      pb2 <- ggplot2::ggplot_build(p2)
      pb3 <- ggplot2::ggplot_build(p3)
      pb4 <- ggplot2::ggplot_build(p4)

      # checking subtitle outputs
      testthat::expect_identical(p1$labels$subtitle, p1_subtitle)
      testthat::expect_identical(p2$labels$subtitle, p2_subtitle)
      testthat::expect_identical(p3$labels$subtitle, p3_subtitle)
      testthat::expect_identical(p4$labels$subtitle, p4_subtitle)

      # testing captions
      testthat::expect_identical(
        pb1$plot$labels$caption,
        ggplot2::expr(atop(
          displaystyle(NULL),
          expr = paste(
            "Pairwise test: ",
            bold("Durbin-Conover test"),
            "; Comparisons shown: ",
            bold("only significant")
          )
        ))
      )
      testthat::expect_identical(
        pb2$plot$labels$caption,
        ggplot2::expr(atop(
          displaystyle(NULL),
          expr = paste(
            "Pairwise test: ",
            bold("Yuen's trimmed means test"),
            "; Comparisons shown: ",
            bold("only non-significant")
          )
        ))
      )
      testthat::expect_null(p3$labels$caption, NULL)
      testthat::expect_null(p4$labels$caption, NULL)


      p5 <-
        ggstatsplot::ggwithinstats(
          data = iris_long,
          x = condition,
          y = value,
          type = "bf",
          pairwise.comparisons = TRUE
        )

      testthat::expect_is(p5, "ggplot")

      # checking changes made to ggsignif geom work
      testthat::expect_equal(pb1$data[[7]]$textsize[[1]], 6L)
      testthat::expect_equal(pb1$data[[7]]$shape[[1]], 19L)
      testthat::expect_identical(pb1$data[[7]]$colour[[1]], "black")
      testthat::expect_equal(pb1$data[[7]]$size[[1]], 0.5, tolerance = 0.001)
    }
  }
)

# ggplot component addition works ------------------------------------------

testthat::test_that(
  desc = "ggplot component addition works",
  code = {
    testthat::skip_on_cran()

    # setup
    set.seed(123)
    library(WRS2)

    # plot
    p <-
      ggstatsplot::ggwithinstats(
        data = WineTasting,
        x = Wine,
        y = Taste,
        results.subtitle = FALSE,
        pairwise.comparisons = FALSE,
        messages = FALSE,
        ggplot.component = ggplot2::labs(y = "Taste rating")
      )

    # build plot
    pb <- ggplot2::ggplot_build(p)

    # test
    testthat::expect_identical(p$labels$y, "Taste rating")
  }
)
