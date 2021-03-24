if (require("afex")) {

  # for t-test
  data_bugs_2 <- ggstatsplot::bugs_long %>%
    dplyr::filter(subject <= 30, condition %in% c("HDLF", "HDHF"))

  # basic plotting works - two groups ---------------------------------

  test_that(
    desc = "basic plotting works - two groups",
    code = {
      skip_on_cran()
      skip_if(getRversion() < "3.6")

      # plot
      set.seed(123)
      p1 <-
        ggstatsplot::ggwithinstats(
          data = data_bugs_2,
          x = condition,
          y = desire,
          k = 4,
          conf.level = 0.99,
          outlier.tagging = TRUE,
          outlier.label = "region",
          outlier.coef = 1.5,
          bf.message = TRUE,
          pairwise.comparisons = TRUE,
          ggsignif.args = list(textsize = 6, tip_length = 0.01),
          pairwise.annotation = "asterisk",
          point.path.args = list(color = "red"),
          centrality.path.args = list(color = "blue", size = 2, alpha = 0.8),
          centrality.point.args = list(size = 3, color = "darkgreen", alpha = 0.5),
          title = "bugs dataset",
          caption = "From `jmv` package"
        )

      # build the plot
      pb1 <- ggplot2::ggplot_build(p1)

      # subtitle
      set.seed(123)
      p1_subtitle <-
        statsExpressions::two_sample_test(
          data = data_bugs_2,
          x = condition,
          y = desire,
          k = 4,
          paired = TRUE,
          conf.level = 0.99
        )$expression[[1]]

      # check data
      set.seed(123)
      expect_snapshot(list(
        pb1$data[[1]], pb1$data[[2]], pb1$data[[4]],
        pb1$data[[5]], pb1$data[[6]], pb1$data[[7]]
      ))

      # range of y variable
      expect_equal(ggplot2::layer_scales(p1)$y$range$range, c(0L, 10L))

      # checking x-axis sample size labels
      expect_identical(
        ggplot2::layer_scales(p1)$x$labels,
        c("HDHF\n(n = 27)", "HDLF\n(n = 27)")
      )

      # checking plot labels
      expect_identical(p1$labels$title, "bugs dataset")
      expect_identical(p1$labels$subtitle, p1_subtitle)
      expect_identical(p1$labels$x, "condition")
      expect_identical(p1$labels$y, "desire")
    }
  )

  # basic plotting works - more than two groups ---------------------------------

  test_that(
    desc = "basic plotting works - more than two groups",
    code = {
      skip_on_cran()
      skip_if(getRversion() < "3.6")

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
            bf.message = TRUE,
            k = 4,
            conf.level = 0.99,
            outlier.tagging = TRUE,
            outlier.coef = 2.5,
            pairwise.comparisons = TRUE,
            pairwise.annotation = "asterisk",
            title = "wine tasting data",
            caption = "From `WRS2` package"
          )

        # build the plot
        pb1 <- ggplot2::ggplot_build(p1)

        # subtitle
        set.seed(123)
        p1_subtitle <-
          statsExpressions::oneway_anova(
            data = WineTasting,
            x = "Wine",
            y = Taste,
            type = "p",
            k = 4,
            paired = TRUE,
            conf.level = 0.99
          )$expression[[1]]

        # check data
        set.seed(123)
        expect_snapshot(list(
          pb1$data[[1]], pb1$data[[2]], pb1$data[[4]],
          pb1$data[[5]], pb1$data[[6]], pb1$data[[7]]
        ))

        # checking displayed outlier labels
        expect_equal(
          ggplot2::layer_grob(p1, i = 4L)$`1`$lab,
          c(5.00, 6.30, 6.30, 6.25),
          tolerance = 0.01
        )

        # checking x-axis sample size labels
        expect_identical(
          ggplot2::layer_scales(p1)$x$labels,
          c("Wine A\n(n = 22)", "Wine B\n(n = 22)", "Wine C\n(n = 22)")
        )

        # checking plot labels
        expect_identical(p1$labels$title, "wine tasting data")
        expect_identical(p1$labels$subtitle, p1_subtitle)
        # expect_identical(
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
        expect_identical(p1$labels$x, "Wine")
        expect_identical(p1$labels$y, "Taste")
      }
    }
  )

  # checking subtitle outputs - without NAs ------------------------------------

  test_that(
    desc = "checking subtitle outputs - without NAs",
    code = {
      skip_on_cran()
      skip_if(getRversion() < "3.6")

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
          statsExpressions::oneway_anova(
            data = iris_long,
            x = condition,
            y = value,
            type = "np",
            paired = TRUE,
            conf.level = 0.90
          )$expression[[1]]

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
          statsExpressions::oneway_anova(
            data = iris_long,
            x = condition,
            y = value,
            paired = TRUE,
            type = "r",
            conf.level = 0.90
          )$expression[[1]]

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
            bf.message = TRUE
          ))

        set.seed(123)
        p3_subtitle <-
          suppressWarnings(statsExpressions::two_sample_test(
            data = ggstatsplot::VR_dilemma,
            x = modality,
            y = score,
            paired = TRUE,
            type = "r",
            k = 3,
            nboot = 25
          )$expression[[1]])

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
            bf.message = TRUE
          )

        set.seed(123)
        p4_subtitle <-
          suppressWarnings(statsExpressions::two_sample_test(
            data = ggstatsplot::VR_dilemma,
            x = modality,
            y = score,
            type = "np",
            conf.level = 0.50,
            paired = TRUE,
            k = 4,
            nboot = 15
          )$expression[[1]])

        # built plots
        pb1 <- ggplot2::ggplot_build(p1)
        pb2 <- ggplot2::ggplot_build(p2)
        pb3 <- ggplot2::ggplot_build(p3)
        pb4 <- ggplot2::ggplot_build(p4)

        # checking subtitle outputs
        expect_identical(p1$labels$subtitle, p1_subtitle)
        expect_identical(p2$labels$subtitle, p2_subtitle)
        expect_identical(p3$labels$subtitle, p3_subtitle)
        expect_identical(p4$labels$subtitle, p4_subtitle)

        # testing captions
        expect_identical(
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
        expect_identical(
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
        expect_null(p3$labels$caption, NULL)
        expect_null(p4$labels$caption, NULL)


        p5 <-
          ggstatsplot::ggwithinstats(
            data = iris_long,
            x = condition,
            y = value,
            type = "bayes",
            pairwise.comparisons = TRUE
          )

        expect_s3_class(p5, "ggplot")

        # checking changes made to ggsignif geom work
        expect_equal(pb1$data[[7]]$textsize[[1]], 6L)
        expect_equal(pb1$data[[7]]$shape[[1]], 19L)
        expect_identical(pb1$data[[7]]$colour[[1]], "black")
        expect_equal(pb1$data[[7]]$size[[1]], 0.5, tolerance = 0.001)
      }
    }
  )

  # ggplot component addition works ------------------------------------------

  test_that(
    desc = "ggplot component addition works",
    code = {
      skip_on_cran()

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
          pairwise.comparisons = TRUE,
          ggplot.component = ggplot2::labs(y = "Taste rating")
        )

      # build plot
      pb <- ggplot2::ggplot_build(p)

      # test
      expect_identical(p$labels$y, "Taste rating")
    }
  )

  # turning off mean path works ------------------------------------------

  test_that(
    desc = "turning off mean path works",
    code = {
      set.seed(123)
      library(ggstatsplot)

      p1 <-
        ggwithinstats(
          iris_long,
          condition,
          value,
          centrality.point.args = list(size = 5, alpha = 0.5, color = "darkred"),
          centrality.path = TRUE,
          results.subtitle = FALSE,
          pairwise.comparisons = FALSE
        )

      p2 <-
        ggwithinstats(
          iris_long,
          condition,
          value,
          centrality.point.args = list(size = 5, alpha = 0.5, color = "darkred"),
          centrality.path = FALSE,
          results.subtitle = FALSE,
          pairwise.comparisons = FALSE
        )

      pb1 <- ggplot2::ggplot_build(p1)
      pb2 <- ggplot2::ggplot_build(p2)

      expect_equal(pb1$data[[1]], pb2$data[[1]])
      expect_equal(pb1$data[[2]], pb2$data[[2]])
      expect_equal(pb1$data[[3]], pb2$data[[3]])
      expect_equal(pb1$data[[5]], pb2$data[[4]])
      expect_equal(pb1$data[[6]], pb2$data[[5]])
    }
  )
}
