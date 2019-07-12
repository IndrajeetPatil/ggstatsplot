context(desc = "ggbetweenstats")

# outlier labeling works ----------------------------------------------------

testthat::test_that(
  desc = "error when x and outlier.label are same",
  code = {
    testthat::expect_error(
      suppressWarnings(ggstatsplot::ggbetweenstats(
        data = iris,
        x = Species,
        y = Sepal.Length,
        outlier.label = Species
      ))
    )
  }
)

testthat::test_that(
  desc = "outlier.labeling works across vector types",
  code = {
    testthat::skip_on_cran()

    # `outlier.label` is numeric
    set.seed(123)
    testthat::expect_true(inherits(
      x = ggstatsplot::ggbetweenstats(
        data = dplyr::sample_frac(tbl = ggstatsplot::movies_long, size = 0.25),
        x = genre,
        y = rating,
        messages = TRUE,
        palette = "Set3",
        outlier.tagging = TRUE,
        outlier.label = length,
        pairwise.comparisons = TRUE
      ),
      what = "gg"
    ))

    # `outlier.label` is factor
    set.seed(123)
    testthat::expect_true(inherits(
      ggstatsplot::ggbetweenstats(
        data = dplyr::sample_frac(tbl = ggstatsplot::movies_long, size = 0.25),
        x = genre,
        y = rating,
        messages = FALSE,
        palette = "Set3",
        outlier.tagging = TRUE,
        outlier.label = "title"
      ),
      what = "gg"
    ))


    # `outlier.label` is character
    # also x, y, and outlier.label arguments as characters
    set.seed(123)
    movies_long1 <-
      dplyr::sample_frac(tbl = ggstatsplot::movies_long, size = 0.25)
    movies_long1$title <- as.character(movies_long1$title)

    testthat::expect_true(inherits(
      x =
        ggstatsplot::ggbetweenstats(
          data = movies_long1,
          x = "genre",
          y = "rating",
          messages = FALSE,
          palette = "Set3",
          outlier.tagging = TRUE,
          outlier.label = "title",
          outlier.coef = 5
        ),
      what = "gg"
    ))
  }
)

# checking sorting -------------------------------------------------------

testthat::test_that(
  desc = "checking sorting",
  code = {
    testthat::skip_on_cran()

    set.seed(123)
    p1 <- ggstatsplot::ggbetweenstats(
      data = iris,
      x = Species,
      y = Sepal.Length,
      sort = "none",
      results.subtitle = FALSE,
      messages = FALSE
    )

    set.seed(123)
    p2 <- ggstatsplot::ggbetweenstats(
      data = iris,
      x = Species,
      y = Sepal.Length,
      sort = "ascending",
      results.subtitle = FALSE,
      messages = FALSE
    )

    set.seed(123)
    p3 <- ggstatsplot::ggbetweenstats(
      data = iris,
      x = Species,
      y = Sepal.Length,
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

# checking labels and data from plot -------------------------------------

testthat::test_that(
  desc = "checking labels and data from plot",
  code = {

    # creating the plot
    set.seed(123)
    p <- ggstatsplot::ggbetweenstats(
      data = ggplot2::msleep,
      x = vore,
      y = brainwt,
      title = "mammalian sleep",
      caption = "From ggplot2 package",
      xlab = "vore",
      ylab = "brain weight",
      axes.range.restrict = TRUE,
      outlier.tagging = TRUE,
      outlier.label = name,
      conf.level = 0.99,
      k = 5,
      bf.message = TRUE,
      messages = FALSE
    )

    # subtitle
    set.seed(123)
    p_subtitle <- ggstatsplot::subtitle_anova_parametric(
      data = ggplot2::msleep,
      x = vore,
      y = brainwt,
      k = 5,
      messages = FALSE,
      conf.level = 0.99
    )

    # plot build
    pb <- ggplot2::ggplot_build(p)

    # dataframe used for visualization
    testthat::expect_equal(length(pb$data), 6L)
    testthat::expect_equal(dim(pb$data[[1]]), c(44L, 13L))
    testthat::expect_equal(dim(pb$data[[2]]), c(4L, 25L))
    testthat::expect_equal(dim(pb$data[[3]]), c(2048L, 20L))
    testthat::expect_equal(dim(pb$data[[4]]), c(7L, 15L))
    testthat::expect_equal(dim(pb$data[[5]]), c(4L, 12L))
    testthat::expect_equal(dim(pb$data[[6]]), c(4L, 15L))

    # data from difference layers
    testthat::expect_equal(length(pb$data), 6L)
    testthat::expect_equal(pb$data[[5]]$x, c(1L, 2L, 3L, 4L))
    testthat::expect_equal(pb$data[[5]]$y,
      c(0.07925556, 0.62159750, 0.02155000, 0.14573118),
      tolerance = 0.001
    )

    # checking displayed outlier labels
    outlier.labels <- ggplot2::layer_grob(p, i = 4L)$`1`$lab

    testthat::expect_equal(length(outlier.labels), 7L)
    testthat::expect_identical(
      outlier.labels,
      c(
        "Asian elephant",
        "Horse",
        "Gray seal",
        "Human",
        "African elephant",
        "Jaguar",
        "Giant armadillo"
      )
    )

    # range of y variable
    testthat::expect_equal(ggplot2::layer_scales(p)$y$range$range,
      c(0.00014, 5.71200000),
      tolerance = 1e-5
    )

    # limits of data
    testthat::expect_equal(ggplot2::layer_scales(p)$y$limits,
      c(0.00014, 5.71200),
      tolerance = 1e-3
    )

    # checking x-axis sample size labels
    testthat::expect_identical(
      ggplot2::layer_scales(p)$x$labels,
      c(
        "carni\n(n = 9)",
        "herbi\n(n = 20)",
        "insecti\n(n = 5)",
        "omni\n(n = 17)"
      )
    )

    # checking plot labels
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
          "1.54274",
          ", ",
          italic("r")["Cauchy"]^"JZS",
          " = ",
          "0.70700"
        )
      ))
    )
    testthat::expect_identical(p$labels$x, "vore")
    testthat::expect_identical(p$labels$y, "brain weight")
  }
)

# mean labelling tests work ------------------------------------------

testthat::test_that(
  desc = "checking mean labels are working",
  code = {
    testthat::skip_on_cran()

    # creating the plot
    set.seed(123)
    p <- ggstatsplot::ggbetweenstats(
      data = tibble::as_tibble(mtcars, rownames = "name"),
      x = "cyl",
      y = "wt",
      type = "np",
      mean.ci = TRUE,
      k = 3,
      conf.level = 0.90,
      outlier.tagging = TRUE,
      outlier.label = "name",
      outlier.coef = 2.5,
      nboot = 5,
      messages = FALSE
    ) +
      ggplot2::coord_cartesian(ylim = c(1, 6)) +
      ggplot2::scale_y_continuous(limits = c(1, 6), breaks = seq(1, 6, 1))

    # plot build
    pb <- ggplot2::ggplot_build(p)

    # checking dimensions of data for `geom_point`
    # since there are outliers, there should be 3 less no. of points than sample
    # size (which is 32L here)
    testthat::expect_equal(dim(pb$data[[1]]), c(29L, 13L))

    # checking displayed mean labels
    testthat::expect_identical(
      pb$data[[6]]$label,
      c(
        "2.290, 95% CI [1.907, 2.673]",
        "3.120, 95% CI [2.787, 3.453]",
        "4.000, 95% CI [3.561, 4.439]"
      )
    )

    testthat::expect_identical(
      pb$data[[4]]$label,
      c(
        "Cadillac Fleetwood",
        "Lincoln Continental",
        "Chrysler Imperial"
      )
    )

    # check if the y-axis labels have changed
    testthat::expect_identical(
      pb$layout$panel_params[[1]]$x.labels,
      c("4\n(n = 11)", "6\n(n = 7)", "8\n(n = 14)")
    )

    testthat::expect_identical(
      pb$layout$panel_params[[1]]$y.labels,
      c("1", "2", "3", "4", "5", "6")
    )
  }
)

# plot caption is correct --------------------------------------------------

testthat::test_that(
  desc = "checking mean labels are working",
  code = {
    testthat::skip_on_cran()

    library(ggplot2)

    # caption for the plot
    set.seed(254)
    plot_caption <-
      ggstatsplot::ggbetweenstats(
        data = msleep,
        x = vore,
        y = brainwt,
        messages = FALSE,
        bf.prior = 0.85,
        k = 3,
        return = "caption"
      )

    # function output
    set.seed(254)
    fun_output <-
      bf_oneway_anova(
        data = msleep,
        x = vore,
        y = brainwt,
        bf.prior = 0.85,
        k = 3
      )

    # these should be equal
    testthat::expect_identical(plot_caption, fun_output)
  }
)

# subtitles with bayesian tests work -----------------------------------------

testthat::test_that(
  desc = "subtitles with bayesian tests work",
  code = {
    testthat::skip_on_cran()

    # plot
    set.seed(123)
    p1 <- ggstatsplot::ggbetweenstats(
      data = ggplot2::mpg,
      x = drv,
      y = cty,
      bf.prior = 0.8,
      messages = TRUE,
      k = 4,
      type = "bf",
      pairwise.comparisons = TRUE
    )

    # subtitle
    set.seed(123)
    p1_subtitle <- ggstatsplot::subtitle_anova_bayes(
      data = ggplot2::mpg,
      x = drv,
      y = cty,
      bf.prior = 0.8,
      messages = FALSE,
      k = 4
    )

    # plot
    set.seed(123)
    p2 <- ggstatsplot::ggbetweenstats(
      data = ToothGrowth,
      x = supp,
      y = len,
      messages = FALSE,
      k = 3,
      type = "bayes"
    )

    # subtitle
    set.seed(123)
    p2_subtitle <- ggstatsplot::subtitle_t_bayes(
      data = ToothGrowth,
      x = supp,
      y = len,
      messages = FALSE,
      k = 3
    )

    # checking if these two are equal
    testthat::expect_identical(p1$labels$subtitle, p1_subtitle)
    testthat::expect_identical(p2$labels$subtitle, p2_subtitle)
  }
)

# subtitle works with equal variance -----------------------------------------

testthat::test_that(
  desc = "subtitle works with equal variance assumption",
  code = {
    testthat::skip_on_cran()

    # plot
    set.seed(123)
    p <- ggstatsplot::ggbetweenstats(
      data = mtcars,
      x = cyl,
      y = wt,
      nboot = 10,
      effsize.type = "partial.eta",
      var.equal = TRUE,
      messages = FALSE,
      k = 3
    )

    # subtitle
    set.seed(123)
    p_subtitle <- ggstatsplot::subtitle_anova_parametric(
      data = mtcars,
      x = cyl,
      y = wt,
      nboot = 10,
      effsize.type = "partial.eta",
      var.equal = TRUE,
      messages = FALSE,
      k = 3
    )

    # checking if these two are equal
    testthat::expect_identical(p$labels$subtitle, p_subtitle)
  }
)

# checking if plot.type argument works --------------------------------------

testthat::test_that(
  desc = "checking if plot.type argument works",
  code = {
    testthat::skip_on_cran()
    set.seed(123)

    # boxplot
    p1 <-
      ggstatsplot::ggbetweenstats(
        data = ToothGrowth,
        x = supp,
        y = len,
        plot.type = "box",
        results.subtitle = FALSE,
        outlier.tagging = TRUE,
        bf.message = TRUE,
        outlier.coef = 0.75,
        outlier.color = "blue",
        mean.color = "darkgreen",
        mean.label.color = "blue",
        messages = FALSE
      )

    # violin
    p2 <-
      ggstatsplot::ggbetweenstats(
        data = ToothGrowth,
        x = supp,
        y = len,
        effsize.noncentral = FALSE,
        plot.type = "violin",
        outlier.tagging = TRUE,
        outlier.coef = 0.75,
        outlier.color = "blue",
        bf.message = FALSE,
        mean.plotting = FALSE,
        sample.size.label = FALSE,
        package = "wesanderson",
        palette = "Royal1",
        messages = FALSE
      ) +
      ggplot2::scale_y_continuous(breaks = seq(0, 30, 5))

    # build the plots
    pb1 <- ggplot2::ggplot_build(p1)
    pb2 <- ggplot2::ggplot_build(p2)

    # tests for labels
    testthat::expect_null(pb1$plot$labels$subtitle, NULL)
    testthat::expect_null(pb1$plot$labels$caption, NULL)
    testthat::expect_identical(pb2$plot$labels$subtitle, ggplot2::expr(
      paste(
        NULL,
        italic("t"),
        "(",
        "55.31",
        ") = ",
        "1.92",
        ", ",
        italic("p"),
        " = ",
        "0.061",
        ", ",
        italic("g"),
        " = ",
        "0.49",
        ", CI"["95%"],
        " [",
        "-0.04",
        ", ",
        "1.01",
        "]",
        ", ",
        italic("n"),
        " = ",
        60L
      )
    ))
    testthat::expect_null(pb2$plot$labels$caption, NULL)
    testthat::expect_identical(length(pb1$data), 5L)
    testthat::expect_identical(length(pb1$data), 5L)
    testthat::expect_identical(length(pb2$data), 4L)
    testthat::expect_identical(
      unique(pb1$data[[1]]$colour),
      c("#1B9E77", "#D95F02")
    )
    testthat::expect_identical(
      unique(pb2$data[[1]]$colour),
      c("#899DA4", "#C93312")
    )
    testthat::expect_identical(
      pb2$layout$panel_params[[1]]$x.labels,
      c("OJ", "VC")
    )
    testthat::expect_identical(
      pb2$layout$panel_params[[1]]$y.labels,
      c("5", "10", "15", "20", "25", "30")
    )

    # tests for data
    testthat::expect_equal(dim(pb1$data[[1]]), c(58L, 13L))
    testthat::expect_equal(dim(pb1$data[[2]]), c(2L, 25L))
    testthat::expect_equal(dim(pb1$data[[3]]), c(2L, 15L))
    testthat::expect_equal(dim(pb1$data[[4]]), c(2L, 12L))
    testthat::expect_equal(dim(pb1$data[[5]]), c(2L, 15L))
    testthat::expect_equal(pb1$data[[4]]$x, c(1L, 2L))
    testthat::expect_identical(
      as.character(round(pb1$data[[4]]$y, 2)),
      pb1$data[[5]]$label
    )
    testthat::expect_equal(dim(pb1$data[[2]]), c(2L, 25L))
    testthat::expect_equal(dim(pb1$data[[3]]), c(2L, 15L))
    testthat::expect_equal(dim(pb1$data[[4]]), c(2L, 12L))
    testthat::expect_equal(dim(pb1$data[[5]]), c(2L, 15L))
    testthat::expect_equal(pb1$data[[4]]$x, c(1L, 2L))
    testthat::expect_identical(pb1$data[[3]]$colour[1], "black")
    testthat::expect_identical(pb1$data[[4]]$colour[1], "darkgreen")
    testthat::expect_identical(pb1$data[[5]]$colour[1], "blue")
    testthat::expect_equal(dim(pb2$data[[1]]), c(58L, 13L))
    testthat::expect_equal(dim(pb2$data[[2]]), c(2L, 10L))
    testthat::expect_equal(dim(pb2$data[[3]]), c(1024L, 20L))
  }
)

# ggplot component addition works ------------------------------------------

testthat::test_that(
  desc = "ggplot component addition works",
  code = {
    testthat::skip_on_cran()

    # plot
    p <- ggstatsplot::ggbetweenstats(
      data = iris,
      x = Species,
      y = Sepal.Length,
      results.subtitle = FALSE,
      messages = FALSE,
      ggplot.component = ggplot2::labs(y = "SL")
    )

    # build plot
    pb <- ggplot2::ggplot_build(p)

    # test
    testthat::expect_identical(p$labels$y, "SL")
  }
)

# subtitle return works ------------------------------------------------

testthat::test_that(
  desc = "subtitle return works",
  code = {
    testthat::skip_on_cran()

    # plot
    set.seed(123)
    subtitle_exp <- ggstatsplot::ggbetweenstats(
      data = iris,
      x = Species,
      y = Sepal.Length,
      return = "subtitle",
      messages = FALSE
    )

    # test
    testthat::expect_identical(
      subtitle_exp,
      ggplot2::expr(paste(
        NULL,
        italic("F"),
        "(",
        "2",
        ",",
        "92.21",
        ") = ",
        "138.91",
        ", ",
        italic("p"),
        " = ",
        "< 0.001",
        ", ",
        omega["p"]^2,
        " = ",
        "0.61",
        ", CI"["95%"],
        " [",
        "0.54",
        ", ",
        "0.69",
        "]",
        ", ",
        italic("n"),
        " = ",
        150L
      ))
    )
  }
)
