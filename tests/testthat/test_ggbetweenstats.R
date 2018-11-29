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

    # checking dimensions of data
    data_dims <- ggplot2::layer_data(p) %>%
      tibble::as_tibble(x = .) %>%
      dim(.)

    testthat::expect_equal(data_dims[1], 51L)
    testthat::expect_equal(data_dims[2], 13L)

    # checking displayed outlier labels
    outlier.labels <- ggplot2::layer_grob(p, i = 5L)$`1`$lab

    testthat::expect_equal(length(outlier.labels), 7L)
    testthat::expect_identical(outlier.labels[1], "Asian elephant")
    testthat::expect_identical(outlier.labels[7], "Giant armadillo")

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

    testthat::expect_identical(x_labels[1], "carni\n(n = 9)")
    testthat::expect_identical(x_labels[2], "herbi\n(n = 20)")
    testthat::expect_identical(x_labels[3], "insecti\n(n = 5)")
    testthat::expect_identical(x_labels[4], "omni\n(n = 17)")

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
      data = mtcars,
      x = "cyl",
      y = "wt",
      type = "np",
      mean.ci = TRUE,
      k = 3,
      conf.level = 0.90,
      nboot = 5,
      messages = FALSE
    )

    # checking displayed mean labels
    mean.labels <- ggplot2::layer_grob(p, i = 5L)$`1`$lab

    testthat::expect_identical(mean.labels[1], "2.290, 95% CI [1.907, 2.673]")
    testthat::expect_identical(mean.labels[2], "3.120, 95% CI [2.787, 3.453]")
    testthat::expect_identical(mean.labels[3], "4.000, 95% CI [3.561, 4.439]")
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
