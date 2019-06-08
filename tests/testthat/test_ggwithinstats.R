context(desc = "ggwithinstats")

# common setup
set.seed(123)
library(WRS2)
library(jmv)
data("bugs", package = "jmv")

# converting to long format
data_bugs <- bugs %>%
  tibble::as_tibble(.) %>%
  tidyr::gather(., key, value, LDLF:HDHF)

# for t-test
data_bugs_2 <- dplyr::filter(.data = data_bugs, key %in% c("HDLF", "HDHF"))

# errors ------------------------------------------------------------------

testthat::test_that(
  desc = "error when x and outlier.label are same",
  code = {
    testthat::expect_error(
      ggstatsplot::ggwithinstats(
        data = ggstatsplot::iris_long,
        x = condition,
        y = value,
        outlier.label = condition
      )
    )
  }
)

# basic plotting works - two groups ---------------------------------

testthat::test_that(
  desc = "basic plotting works - two groups",
  code = {

    # plot
    set.seed(123)
    p1 <- ggstatsplot::ggwithinstats(
      data = data_bugs_2,
      x = key,
      y = value,
      type = "p",
      sort = "descending",
      sort.fun = mean,
      k = 4,
      conf.level = 0.99,
      outlier.tagging = TRUE,
      outlier.label = "Region",
      outlier.coef = 1.5,
      bf.message = TRUE,
      pairwise.comparisons = TRUE,
      title = "bugs dataset",
      caption = "From `jmv` package",
      messages = FALSE
    )

    # build the plot
    pb1 <- ggplot2::ggplot_build(p1)

    # subtitle
    set.seed(123)
    p1_subtitle <- ggstatsplot::subtitle_t_parametric(
      data = data_bugs_2,
      x = key,
      y = value,
      type = "p",
      k = 4,
      paired = TRUE,
      conf.level = 0.99,
      messages = FALSE
    )

    # dataframe used for visualization
    testthat::expect_equal(length(pb1$data), 8L)
    testthat::expect_equal(dim(p1$data), c(180L, 6L))
    testthat::expect_equal(dim(pb1$data[[1]]), c(180L, 10L))
    testthat::expect_equal(dim(pb1$data[[2]]), c(2L, 25L))
    testthat::expect_equal(dim(pb1$data[[3]]), c(1024L, 20L))
    testthat::expect_equal(dim(pb1$data[[4]]), c(180L, 8L))
    testthat::expect_equal(dim(pb1$data[[5]]), c(0L, 0L))
    testthat::expect_equal(dim(pb1$data[[6]]), c(2L, 12L))
    testthat::expect_equal(dim(pb1$data[[7]]), c(2L, 15L))
    testthat::expect_equal(dim(pb1$data[[8]]), c(2L, 8L))

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
    testthat::expect_identical(
      p1$labels$caption,
      ggplot2::expr(atop(
        displaystyle("From `jmv` package"),
        expr = paste(
          "In favor of null: ",
          "log"["e"],
          "(BF"["01"],
          ") = ",
          "-3.7724",
          ", ",
          italic("r")["Cauchy"],
          " = ",
          "0.7070"
        )
      ))
    )
    testthat::expect_identical(p1$labels$x, "key")
    testthat::expect_identical(p1$labels$y, "value")
  }
)

# basic plotting works - more than two groups ---------------------------------

testthat::test_that(
  desc = "basic plotting works - more than two groups",
  code = {
    testthat::skip_on_cran()
    set.seed(123)

    # plot
    p1 <- ggstatsplot::ggwithinstats(
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
      title = "wine tasting data",
      caption = "From `WRS2` package",
      messages = TRUE
    )

    # build the plot
    pb1 <- ggplot2::ggplot_build(p1)

    # subtitle
    set.seed(123)
    p1_subtitle <- ggstatsplot::subtitle_anova_parametric(
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
    testthat::expect_equal(dim(pb1$data[[2]]), c(3L, 25L))
    testthat::expect_equal(dim(pb1$data[[3]]), c(1536L, 20L))
    testthat::expect_equal(dim(pb1$data[[4]]), c(4L, 15L))
    testthat::expect_equal(dim(pb1$data[[5]]), c(3L, 12L))
    testthat::expect_equal(dim(pb1$data[[6]]), c(3L, 15L))
    testthat::expect_equal(dim(pb1$data[[7]]), c(3L, 8L))
    testthat::expect_equal(dim(pb1$data[[8]]), c(6L, 19L))

    # data from difference layers
    testthat::expect_equal(pb1$data[[5]]$x, c(1L, 2L, 3L))
    testthat::expect_equal(pb1$data[[5]]$y,
      c(5.459091, 5.543182, 5.53409),
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
      c("Wine C\n(n = 22)", "Wine A\n(n = 22)", "Wine B\n(n = 22)")
    )

    # checking plot labels
    testthat::expect_identical(p1$labels$title, "wine tasting data")
    testthat::expect_identical(p1$labels$subtitle, p1_subtitle)
    testthat::expect_identical(
      p1$labels$caption,
      ggplot2::expr(atop(
        displaystyle(atop(
          displaystyle("From `WRS2` package"),
          expr = paste(
            "In favor of null: ",
            "log"["e"],
            "(BF"["01"],
            ") = ",
            "2.0635",
            ", ",
            italic("r")["Cauchy"],
            " = ",
            "0.7070"
          )
        )),
        expr = paste(
          "Pairwise comparisons: ",
          bold("Student's t-test"),
          "; Adjustment (p-value): ",
          bold("Holm")
        )
      ))
    )
    testthat::expect_identical(p1$labels$x, "Wine")
    testthat::expect_identical(p1$labels$y, "Taste")

    # checking pairwise comparisons
    testthat::expect_equal(levels(pb1$data[[8]]$annotation), c("*", "**"))
  }
)

# checking sorting -------------------------------------------------------

testthat::test_that(
  desc = "checking sorting",
  code = {
    testthat::skip_on_cran()

    set.seed(123)
    p1 <- ggstatsplot::ggwithinstats(
      data = WineTasting,
      x = Wine,
      y = Taste,
      sort = "none",
      package = "wesanderson",
      palette = "Royal1",
      results.subtitle = FALSE,
      messages = FALSE
    )

    set.seed(123)
    p2 <- ggstatsplot::ggwithinstats(
      data = WineTasting,
      x = Wine,
      y = Taste,
      sort = "ascending",
      results.subtitle = FALSE,
      messages = FALSE
    )

    set.seed(123)
    p3 <- ggstatsplot::ggwithinstats(
      data = WineTasting,
      x = Wine,
      y = Taste,
      sort = "xxx",
      results.subtitle = FALSE,
      messages = FALSE
    )

    # built plots
    pb1 <- ggplot2::ggplot_build(p1)
    pb2 <- ggplot2::ggplot_build(p2)
    pb3 <- ggplot2::ggplot_build(p3)

    # tests
    testthat::expect_equal(pb1$data[[6]]$label, rev(pb3$data[[6]]$label))
    testthat::expect_equal(pb1$data[[6]]$label, pb2$data[[6]]$label)
  }
)

# checking subtitle outputs - without NAs ------------------------------------

testthat::test_that(
  desc = "checking subtitle outputs - without NAs",
  code = {
    testthat::skip_on_cran()

    set.seed(123)
    p1 <- ggstatsplot::ggwithinstats(
      data = ggstatsplot::iris_long,
      x = condition,
      y = value,
      type = "np",
      pairwise.display = "s",
      pairwise.annotation = "p",
      outlier.tagging = FALSE,
      pairwise.comparisons = TRUE,
      axes.range.restrict = TRUE,
      conf.level = 0.90,
      messages = FALSE
    )

    set.seed(123)
    p1_subtitle <- ggstatsplot::subtitle_anova_nonparametric(
      data = ggstatsplot::iris_long,
      x = condition,
      y = value,
      type = "np",
      paired = TRUE,
      conf.level = 0.90,
      messages = FALSE
    )

    set.seed(123)
    p2 <- ggstatsplot::ggwithinstats(
      data = ggstatsplot::iris_long,
      x = condition,
      y = value,
      type = "r",
      pairwise.display = "ns",
      outlier.tagging = FALSE,
      pairwise.comparisons = TRUE,
      pairwise.annotation = "p",
      conf.level = 0.90,
      messages = FALSE
    )

    set.seed(123)
    p2_subtitle <- ggstatsplot::subtitle_anova_robust(
      data = ggstatsplot::iris_long,
      x = condition,
      y = value,
      paired = TRUE,
      conf.level = 0.90,
      messages = FALSE
    )

    set.seed(123)
    p3 <- suppressWarnings(ggstatsplot::ggwithinstats(
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
    p3_subtitle <- suppressWarnings(ggstatsplot::subtitle_t_robust(
      data = ggstatsplot::VR_dilemma,
      x = modality,
      y = score,
      paired = TRUE,
      k = 3,
      nboot = 25,
      messages = FALSE
    ))

    set.seed(123)
    p4 <- ggstatsplot::ggwithinstats(
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
    p4_subtitle <- ggstatsplot::subtitle_t_nonparametric(
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
    testthat::expect_identical(p1$labels$caption, ggplot2::expr(atop(
      displaystyle(NULL),
      expr = paste(
        "Pairwise comparisons: ",
        bold("Durbin-Conover test"),
        "; Adjustment (p-value): ",
        bold("Holm")
      )
    )))
    testthat::expect_identical(p2$labels$caption, ggplot2::expr(atop(
      displaystyle(NULL),
      expr = paste(
        "Pairwise comparisons: ",
        bold("Yuen's trimmed means test"),
        "; Adjustment (p-value): ",
        bold("Holm")
      )
    )))
    testthat::expect_null(p3$labels$caption, NULL)
    testthat::expect_null(p4$labels$caption, NULL)


    p5 <- ggstatsplot::ggwithinstats(
      data = ggstatsplot::iris_long,
      x = condition,
      y = value,
      type = "bf",
      pairwise.comparisons = TRUE
    )

    testthat::expect_is(p5, "ggplot")
  }
)

# checking subtitle outputs - with NAs ----------------------------------------

testthat::test_that(
  desc = "checking subtitle outputs - with NAs",
  code = {
    testthat::skip_on_cran()
    set.seed(123)

    set.seed(123)
    p1 <- ggstatsplot::ggwithinstats(
      data = data_bugs,
      x = key,
      y = value,
      type = "np",
      outlier.tagging = TRUE,
      pairwise.comparisons = FALSE,
      axes.range.restrict = TRUE,
      pairwise.annotation = "p",
      bf.message = TRUE,
      messages = FALSE
    )

    set.seed(123)
    p1_subtitle <- ggstatsplot::subtitle_anova_nonparametric(
      data = data_bugs,
      x = key,
      y = value,
      paired = TRUE,
      messages = FALSE
    )

    set.seed(123)
    p2 <- ggstatsplot::ggwithinstats(
      data = data_bugs,
      x = key,
      y = value,
      type = "r",
      outlier.tagging = TRUE,
      pairwise.comparisons = TRUE,
      pairwise.annotation = "p",
      conf.level = 0.99,
      bf.message = TRUE,
      messages = FALSE
    )

    set.seed(123)
    p2_subtitle <- ggstatsplot::subtitle_anova_robust(
      data = data_bugs,
      x = key,
      y = value,
      type = "r",
      paired = TRUE,
      conf.level = 0.99,
      messages = FALSE
    )

    set.seed(123)
    p3 <- ggstatsplot::ggwithinstats(
      data = data_bugs_2,
      x = key,
      y = value,
      type = "np",
      messages = FALSE
    )

    set.seed(123)
    p3_subtitle <- ggstatsplot::subtitle_t_nonparametric(
      data = data_bugs_2,
      x = key,
      y = value,
      type = "np",
      paired = TRUE,
      messages = FALSE
    )

    set.seed(123)
    p4 <- ggstatsplot::ggwithinstats(
      data = data_bugs_2,
      x = key,
      y = value,
      type = "r",
      nboot = 20,
      conf.level = 0.90,
      messages = FALSE
    )

    set.seed(123)
    p4_subtitle <- ggstatsplot::subtitle_t_robust(
      data = data_bugs_2,
      x = key,
      y = value,
      paired = TRUE,
      nboot = 20,
      conf.level = 0.90,
      messages = FALSE
    )

    # testing subtitle and caption
    testthat::expect_identical(p1$labels$subtitle, p1_subtitle)
    testthat::expect_identical(p2$labels$subtitle, p2_subtitle)
    testthat::expect_identical(p3$labels$subtitle, p3_subtitle)
    testthat::expect_identical(p4$labels$subtitle, p4_subtitle)
    testthat::expect_null(p1$labels$caption, NULL)
    testthat::expect_identical(p2$labels$caption, ggplot2::expr(atop(
      displaystyle(NULL),
      expr = paste(
        "Pairwise comparisons: ",
        bold("Yuen's trimmed means test"),
        "; Adjustment (p-value): ",
        bold("Holm")
      )
    )))
    testthat::expect_null(p3$labels$caption, NULL)
    testthat::expect_null(p4$labels$caption, NULL)
  }
)

# ggplot component addition works ------------------------------------------

testthat::test_that(
  desc = "ggplot component addition works",
  code = {
    testthat::skip_on_cran()

    # plot
    p <- ggstatsplot::ggwithinstats(
      data = WineTasting,
      x = Wine,
      y = Taste,
      results.subtitle = FALSE,
      messages = FALSE,
      ggplot.component = ggplot2::labs(y = "Taste rating")
    )

    # build plot
    pb <- ggplot2::ggplot_build(p)

    # test
    testthat::expect_identical(p$labels$y, "Taste rating")
  }
)

# checking warning message when too few obs --------------------------------

testthat::test_that(
  desc = "checking warning message when too few obs",
  code = {
    testthat::skip_on_cran()
    set.seed(123)

    # dataframe
    df <- structure(list(
      x = c(
        30, 40, 50, 60, 70, 80, 90, 30, 40, 50,
        60, 70, 80, 90, 30, 40, 50, 60, 70, 80, 90, 30, 40, 50, 60, 70,
        80, 90, 30, 40, 50, 60, 70, 80, 90
      ),
      Participant = c(
        "FH2", "FH2",
        "FH2", "FH2", "FH2", "FH2", "FH2", "ZW", "ZW", "ZW", "ZW", "ZW",
        "ZW", "ZW", "KS", "KS", "KS", "KS", "KS", "KS", "KS", "CL", "CL",
        "CL", "CL", "CL", "CL", "CL", "AG", "AG", "AG", "AG", "AG", "AG",
        "AG"
      ),
      Method = c(
        2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2,
        2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2
      ),
      y = c(
        2571.25, 2688.003333, 2779.363333, 2832.046667,
        3050.72, 3255.553333, 3327.173667, 1766.296667, 2107.890333,
        2391.7, 2569.24, 2680.22, 2807.59, 2807.953333, 2078.734,
        2414.366667, 2583.27, 2923.253333, 3085.96, 3094.003333,
        3121.49, 2824.990667, 2716.429667, 2844.323333, 3124.713333,
        3252.863333, 3424.24, 3674.463333, 2401.996667, 2719.046667,
        2712.99, 2951.965667, 3046.526667, 3100.902667, 3195.331333
      )
    ),
    class = c("spec_tbl_df", "tbl_df", "tbl", "data.frame"),
    row.names = c(NA, -35L),
    spec = structure(list(
      cols = list(
        x = structure(list(), class = c("collector_double", "collector")),
        Participant = structure(list(), class = c(
          "collector_character",
          "collector"
        )),
        Method = structure(list(), class = c(
          "collector_double",
          "collector"
        )),
        y = structure(list(), class = c(
          "collector_double",
          "collector"
        ))
      ),
      default = structure(list(), class = c(
        "collector_guess",
        "collector"
      )), skip = 1
    ),
    class = "col_spec"
    )
    )

    # capture the message
    set.seed(123)
    p <- suppressWarnings(ggstatsplot::ggwithinstats(
      data = df,
      x = x,
      y = y,
      pairwise.display = "significant",
      pairwise.annotation = "p.value",
      pairwise.comparisons = TRUE,
      sphericity.correction = TRUE,
      messages = FALSE
    ))

    # build the plot
    pb <- ggplot2::ggplot_build(p)

    # check that
    testthat::expect_identical(
      pb$plot$labels$subtitle,
      ggplot2::expr(paste(
        NULL,
        italic("F"),
        "(",
        "6",
        ",",
        "24",
        ") = ",
        "43.14",
        ", ",
        italic("p"),
        " = ",
        "< 0.001",
        ", ",
        omega^2,
        " = ",
        "0.60",
        ", CI"["95%"],
        " [",
        "0.77",
        ", ",
        "0.93",
        "]",
        ", ",
        italic("n"),
        " = ",
        5L
      ))
    )
  }
)
