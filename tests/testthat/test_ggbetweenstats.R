# context -------------------------------------------------------------------
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

    # `outlier.label` is numeric
    set.seed(123)
    testthat::expect_true(object = inherits(
      x = ggstatsplot::ggbetweenstats(
        data = dplyr::sample_frac(tbl = ggstatsplot::movies_long, size = 0.25),
        x = genre,
        y = rating,
        messages = FALSE,
        outlier.tagging = TRUE,
        outlier.label = length
      ),
      what = "gg"
    ))

    # `outlier.label` is factor
    set.seed(123)
    testthat::expect_true(object = inherits(
      x = ggstatsplot::ggbetweenstats(
        data = dplyr::sample_frac(tbl = ggstatsplot::movies_long, size = 0.25),
        x = genre,
        y = rating,
        messages = FALSE,
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

    testthat::expect_true(object = inherits(
      x =
        ggstatsplot::ggbetweenstats(
          data = movies_long1,
          x = "genre",
          y = "rating",
          messages = FALSE,
          outlier.tagging = TRUE,
          outlier.label = "title",
          outlier.coef = 5
        ),
      what = "gg"
    ))
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
      xlab = "vorarephilia",
      ylab = "brain weight",
      outlier.tagging = TRUE,
      outlier.label = name,
      conf.level = 0.99,
      bf.message = TRUE,
      messages = FALSE
    )

    # subtitle
    set.seed(123)
    p_subtitle <- ggstatsplot::subtitle_anova_parametric(
      data = ggplot2::msleep,
      x = vore,
      y = brainwt,
      messages = FALSE,
      conf.level = 0.99
    )

    # plot build
    pb <- ggplot2::ggplot_build(p)

    # dataframe used for visualization
    testthat::expect_equal(dim(pb$data[[1]]), c(51L, 13L))
    testthat::expect_equal(dim(pb$data[[2]]), c(4L, 22L))
    testthat::expect_equal(dim(pb$data[[3]]), c(4L, 25L))
    testthat::expect_equal(dim(pb$data[[4]]), c(2048L, 20L))
    testthat::expect_equal(dim(pb$data[[5]]), c(7L, 15L))
    testthat::expect_equal(dim(pb$data[[6]]), c(4L, 12L))
    testthat::expect_equal(dim(pb$data[[7]]), c(4L, 15L))

    # data from difference layers
    testthat::expect_equal(length(pb$data), 7L)
    testthat::expect_equal(pb$data[[5]]$x, c(2L, 2L, 1L, 4L, 2L, 1L, 3L))
    testthat::expect_equal(pb$data[[5]]$y,
      c(4.603, 0.655, 0.325, 1.320, 5.712, 0.157, 0.081),
      tolerance = 0.001
    )

    # checking displayed outlier labels
    outlier.labels <- ggplot2::layer_grob(p, i = 5L)$`1`$lab

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

    # range of data
    y_range <- ggplot2::layer_scales(p)$y$range$range

    testthat::expect_equal(y_range[1], -0.09710299, tolerance = 1e-5)
    testthat::expect_equal(y_range[2], 5.71200000, tolerance = 1e-5)

    # limits of data
    y_limits <- ggplot2::layer_scales(p)$y$limits

    testthat::expect_equal(y_limits[1], 0.00014, tolerance = 1e-3)
    testthat::expect_equal(y_limits[2], 5.71200, tolerance = 1e-3)

    # checking x-axis sample size labels
    x_labels <- ggplot2::layer_scales(p)$x$labels

    testthat::expect_identical(
      x_labels,
      c(
        "carni\n(n = 9)",
        "herbi\n(n = 20)",
        "insecti\n(n = 5)",
        "omni\n(n = 17)"
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
          "1.54",
          ", Prior width = ",
          "0.71"
        )
      ))
    )
    testthat::expect_identical(p$labels$x, "vorarephilia")
    testthat::expect_identical(p$labels$y, "brain weight")
  }
)

# mean labelling tests work ------------------------------------------

testthat::test_that(
  desc = "checking mean labels are working",
  code = {

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
    )

    # plot build
    pb <- ggplot2::ggplot_build(p)

    # checking dimensions of data
    data_dims <- ggplot2::layer_data(p) %>%
      tibble::as_tibble(x = .) %>%
      dim(.)

    # dataframe used for visualization
    testthat::expect_equal(data_dims, c(32L, 13L))

    # checking displayed mean labels
    testthat::expect_identical(
      pb$data[[7]]$label,
      c(
        "2.290, 95% CI [1.907, 2.673]",
        "3.120, 95% CI [2.787, 3.453]",
        "4.000, 95% CI [3.561, 4.439]"
      )
    )

    testthat::expect_identical(
      pb$data[[5]]$label,
      c(
        "Cadillac Fleetwood",
        "Lincoln Continental",
        "Chrysler Imperial"
      )
    )
  }
)

# subtitles with bayesian tests work -----------------------------------------

testthat::test_that(
  desc = "subtitles with bayesian tests work",
  code = {

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

    # plot
    set.seed(123)
    p <- ggstatsplot::ggbetweenstats(
      data = mtcars,
      x = cyl,
      y = wt,
      nboot = 50,
      var.equal = TRUE,
      messages = FALSE,
      k = 2
    )

    # subtitle
    set.seed(123)
    p_subtitle <- ggstatsplot::subtitle_anova_parametric(
      data = mtcars,
      x = cyl,
      y = wt,
      nboot = 50,
      var.equal = TRUE,
      messages = FALSE,
      k = 2
    )

    # checking if these two are equal
    testthat::expect_identical(p$labels$subtitle, p_subtitle)
  }
)
