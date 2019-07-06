context(desc = "gghistostats")

# checking plot and parametric stats --------------------------------------

testthat::test_that(
  desc = "checking gghistostats plot and parametric stats - data with NAs",
  code = {
    testthat::skip_on_cran()

    # creating the plot
    set.seed(123)
    p <-
      ggstatsplot::gghistostats(
        data = dplyr::starwars,
        x = height,
        xlab = "character height",
        title = "starwars: character heights",
        binwidth = 20,
        test.value = 150,
        bf.message = TRUE,
        bf.prior = 0.9,
        test.k = 0,
        centrality.k = 0,
        test.value.line = TRUE,
        messages = TRUE
      )

    # build the plot
    pb <- ggplot2::ggplot_build(p)

    # checking data used to create a plot
    dat <- tibble::as_tibble(p$data) %>%
      dplyr::mutate_if(
        .tbl = .,
        .predicate = is.factor,
        .funs = ~ as.character(.)
      )

    # checking dimensions of data
    data_dims <- dim(dat)

    # testing everything is okay with data
    testthat::expect_equal(data_dims, c(81L, 1L))

    # checking different data layers
    testthat::expect_equal(length(pb$data), 5L)
    testthat::expect_equal(dim(pb$data[[1]]), c(11L, 17L))
    testthat::expect_equal(dim(pb$data[[2]]), c(1L, 7L))
    testthat::expect_equal(dim(pb$data[[3]]), c(81L, 15L))
    testthat::expect_equal(dim(pb$data[[4]]), c(1L, 7L))
    testthat::expect_equal(dim(pb$data[[5]]), c(81L, 15L))

    testthat::expect_equal(pb$data[[4]]$xintercept,
      mean(dplyr::starwars$height, na.rm = TRUE),
      tolerance = 0.001
    )
    testthat::expect_equal(pb$data[[2]]$xintercept,
      150.000,
      tolerance = 0.001
    )
    testthat::expect_equal(
      class(pb$data[[3]]$label[[1]]),
      "call"
    )
    testthat::expect_equal(
      class(pb$data[[5]]$label[[1]]),
      "call"
    )
    testthat::expect_equal(
      pb$data[[3]]$label[[1]],
      ggplot2::expr("test" == "150")
    )
    testthat::expect_equal(
      pb$data[[5]]$label[[1]],
      ggplot2::expr("mean" == "174")
    )
    testthat::expect_equal(
      pb$data[[1]]$y,
      c(1L, 2L, 4L, 2L, 3L, 15L, 32L, 15L, 5L, 1L, 1L)
    )
    testthat::expect_equal(
      pb$data[[1]]$x,
      c(60L, 80L, 100L, 120L, 140L, 160L, 180L, 200L, 220L, 240L, 260L)
    )
    testthat::expect_equal(pb$data[[1]]$xmin[1], 50L)
    testthat::expect_equal(pb$data[[1]]$xmax[1], 70L)
    testthat::expect_equal(pb$data[[1]]$xmin[7], 170L)
    testthat::expect_equal(pb$data[[1]]$xmax[7], 190L)
    testthat::expect_null(pb$layout$panel_params[[1]]$y.sec.labels, NULL)

    # checking subtitle
    set.seed(123)
    p_subtitle <-
      ggstatsplot::subtitle_t_onesample(
        data = dplyr::starwars,
        x = height,
        type = "p",
        test.value = 150,
        messages = FALSE
      )

    # testing overall call
    testthat::expect_identical(p$labels$subtitle, p_subtitle)
    testthat::expect_identical(p$labels$title, "starwars: character heights")
    testthat::expect_identical(p$labels$x, "character height")
    testthat::expect_identical(p$labels$caption, ggplot2::expr(atop(
      displaystyle(NULL),
      expr = paste(
        "In favor of null: ",
        "log"["e"],
        "(BF"["01"],
        ") = ",
        "-13.55",
        ", ",
        italic("r")["Cauchy"]^"JZS",
        " = ",
        "0.90"
      )
    )))
  }
)

# checking plot and non-parametric stats -----------------------------------

testthat::test_that(
  desc = "checking gghistostats and non-parametric stats - data without NAs",
  code = {

    # creating the plot
    set.seed(123)
    p <-
      ggstatsplot::gghistostats(
        data = ggplot2::mpg,
        x = cty,
        xlab = "city miles per gallon",
        title = "fuel economy",
        caption = substitute(paste(italic("source"), ": government website")),
        centrality.para = "median",
        bar.measure = "mix",
        binwidth = 5,
        test.value = 20,
        k = 3,
        type = "np",
        test.k = 2,
        centrality.k = 2,
        test.value.line = TRUE,
        messages = FALSE
      )

    # checking subtitle
    set.seed(123)
    p_subtitle <-
      ggstatsplot::subtitle_t_onesample(
        data = ggplot2::mpg,
        x = cty,
        type = "np",
        test.value = 20,
        k = 3,
        messages = FALSE
      )

    # build the plot
    pb <- ggplot2::ggplot_build(p)

    # checking data used to create a plot
    dat <- tibble::as_tibble(p$data) %>%
      dplyr::mutate_if(
        .tbl = .,
        .predicate = is.factor,
        .funs = ~ as.character(.)
      )

    # checking dimensions of data
    data_dims <- dim(dat)

    # testing everything is okay with data
    testthat::expect_equal(data_dims, c(234L, 1L))

    # checking different data layers
    testthat::expect_equal(length(pb$data), 5L)
    testthat::expect_equal(nrow(pb$data[[1]]), 6L)
    testthat::expect_equal(pb$data[[4]]$xintercept,
      median(ggplot2::mpg$cty, na.rm = TRUE),
      tolerance = 0.001
    )
    testthat::expect_equal(pb$data[[2]]$xintercept,
      20.000,
      tolerance = 0.001
    )
    testthat::expect_equal(
      pb$data[[3]]$label[[1]],
      ggplot2::expr("test" == "20.00")
    )
    testthat::expect_equal(
      pb$data[[5]]$label[[1]],
      ggplot2::expr("median" == "17.00")
    )
    testthat::expect_equal(pb$data[[1]]$y[1], 33L)
    testthat::expect_equal(pb$data[[1]]$y[6], 2L)
    testthat::expect_equal(pb$data[[1]]$x[1], 10L)
    testthat::expect_equal(pb$data[[1]]$x[6], 35L)
    testthat::expect_equal(pb$data[[1]]$xmin[1], 7.5, tolerance = 0.001)
    testthat::expect_equal(pb$data[[1]]$xmax[1], 12.5, tolerance = 0.001)
    testthat::expect_equal(pb$data[[1]]$xmin[6], 32.5, tolerance = 0.001)
    testthat::expect_equal(pb$data[[1]]$xmax[6], 37.5, tolerance = 0.001)

    # checking panel parameters
    testthat::expect_equal(pb$layout$panel_params[[1]]$x.range, c(6L, 39L))
    testthat::expect_identical(
      pb$layout$panel_params[[1]]$x.labels,
      c("10", "20", "30")
    )
    testthat::expect_equal(pb$layout$panel_params[[1]]$y.range,
      c(-4.95, 103.95),
      tolerance = 0.001
    )
    testthat::expect_identical(
      pb$layout$panel_params[[1]]$y.labels,
      c("0", "25", "50", "75", "100")
    )
    testthat::expect_equal(pb$layout$panel_params[[1]]$y.sec.range,
      c(-0.02115385, 0.44423077),
      tolerance = 0.001
    )
    testthat::expect_identical(
      pb$layout$panel_params[[1]]$y.sec.labels,
      c("0%", "10%", "20%", "30%", "40%")
    )
    testthat::expect_identical(
      pb$layout$panel_params[[1]]$y.arrange,
      c("primary", "secondary")
    )

    # testing labels
    testthat::expect_identical(p$labels$subtitle, p_subtitle)
    testthat::expect_identical(p$labels$title, "fuel economy")
    testthat::expect_identical(p$labels$x, "city miles per gallon")
    testthat::expect_identical(p$labels$y, "count")
    testthat::expect_identical(
      p$labels$caption,
      ggplot2::expr(paste(
        italic("source"),
        ": government website"
      ))
    )
  }
)

# checking robust stats and proportions -----------------------------------

testthat::test_that(
  desc = "checking robust stats and proportions",
  code = {
    testthat::skip_on_cran()

    # creating the plot
    set.seed(123)
    p <-
      ggstatsplot::gghistostats(
        data = mtcars,
        x = wt,
        bar.measure = "proportion",
        binwidth = 0.5,
        test.value = 2.5,
        type = "r",
        test.value.line = FALSE,
        centrality.para = FALSE,
        messages = FALSE
      ) +
      scale_x_continuous(limits = c(1, 6))

    # build the plot
    pb <- ggplot2::ggplot_build(p)

    # checking subtitle
    set.seed(123)
    p_subtitle <-
      ggstatsplot::subtitle_t_onesample(
        data = mtcars,
        x = wt,
        test.value = 2.5,
        type = "r",
        messages = FALSE
      )

    # testing labels
    testthat::expect_identical(pb$plot$labels$subtitle, p_subtitle)
    testthat::expect_null(pb$plot$labels$caption, NULL)
    testthat::expect_identical(pb$plot$labels$y, "proportion")

    # checking different data layers
    testthat::expect_equal(length(pb$data), 1L)
    testthat::expect_equal(nrow(pb$data[[1]]), 11L)
    testthat::expect_equal(
      pb$data[[1]]$y,
      c(
        0.00000,
        0.06250,
        0.12500,
        0.09375,
        0.21875,
        0.28125,
        0.12500,
        0.00000,
        0.03125,
        0.06250,
        0.00000
      ),
      tolerance = 0.001
    )
    testthat::expect_equal(pb$data[[1]]$x,
      c(1.0, 1.5, 2.0, 2.5, 3.0, 3.5, 4.0, 4.5, 5.0, 5.5, 6.0),
      tolerance = 0.01
    )

    # checking panel parameters
    testthat::expect_equal(pb$layout$panel_params[[1]]$x.range,
      c(0.75, 6.25),
      tolerance = 0.001
    )
    testthat::expect_identical(
      pb$layout$panel_params[[1]]$x.labels,
      c("2", "4", "6")
    )
    testthat::expect_equal(pb$layout$panel_params[[1]]$y.range,
      c(-0.0140625, 0.2953125),
      tolerance = 0.001
    )
    testthat::expect_identical(
      pb$layout$panel_params[[1]]$y.labels,
      c("0%", "10%", "20%")
    )
  }
)

# checking bayes stats and density -----------------------------------

testthat::test_that(
  desc = "checking bayes stats and density",
  code = {
    testthat::skip_on_cran()

    # creating the plot
    set.seed(123)
    p <-
      suppressMessages(ggstatsplot::gghistostats(
        data = morley,
        x = Speed,
        bar.measure = "density",
        binwidth = 50,
        test.value = 2.5,
        type = "bf",
        test.value.line = FALSE,
        centrality.para = FALSE,
        messages = FALSE
      ))

    # build the plot
    pb <- ggplot2::ggplot_build(p)

    # checking subtitle
    set.seed(123)
    p_subtitle <-
      ggstatsplot::subtitle_t_onesample(
        data = morley,
        x = Speed,
        test.value = 2.5,
        type = "bf",
        messages = FALSE
      )

    # testing labels
    testthat::expect_identical(p$labels$subtitle, p_subtitle)
    testthat::expect_identical(p$labels$y, "density")

    # checking different data layers
    testthat::expect_equal(length(pb$data), 1L)
    testthat::expect_equal(nrow(pb$data[[1]]), 10L)
    testthat::expect_equal(pb$data[[1]]$y[1], 0.0002, tolerance = 0.001)
    testthat::expect_equal(pb$data[[1]]$y[5], 0.0044, tolerance = 0.001)
    testthat::expect_equal(pb$data[[1]]$x[1], 600, tolerance = 0.01)
    testthat::expect_equal(pb$data[[1]]$x[5], 800, tolerance = 0.01)

    # checking panel parameters
    testthat::expect_equal(pb$layout$panel_params[[1]]$x.range,
      c(550, 1100),
      tolerance = 0.001
    )
    testthat::expect_identical(
      pb$layout$panel_params[[1]]$x.labels,
      c("600", "700", "800", "900", "1000", "1100")
    )
    testthat::expect_equal(pb$layout$panel_params[[1]]$y.range,
      c(-0.00025, 0.00525),
      tolerance = 0.001
    )
    testthat::expect_identical(
      pb$layout$panel_params[[1]]$y.labels,
      c("0.000", "0.001", "0.002", "0.003", "0.004", "0.005")
    )
  }
)

# checking if normal curve works -------------------------------------

testthat::test_that(
  desc = "checking with default binwidth",
  code = {
    testthat::skip_on_cran()

    # creating a subset of the dataset
    set.seed(123)
    dat1 <- dplyr::sample_frac(tbl = ggplot2::txhousing, size = 0.05)
    dat2 <- ggplot2::msleep

    # plot-1
    p1 <- ggstatsplot::gghistostats(
      data = dat1,
      x = sales,
      results.subtitle = FALSE,
      normal.curve = TRUE,
      bar.measure = "count",
      messages = FALSE
    )

    # plot-2
    p2 <- ggstatsplot::gghistostats(
      data = dat1,
      x = sales,
      results.subtitle = FALSE,
      normal.curve = TRUE,
      binwidth = 100,
      bar.measure = "proportion",
      messages = FALSE
    )

    # plot-3
    p3 <- ggstatsplot::gghistostats(
      data = dat2,
      x = brainwt,
      results.subtitle = FALSE,
      normal.curve = TRUE,
      normal.curve.color = "red",
      normal.curve.size = 0.8,
      bar.measure = "mix",
      messages = FALSE
    )

    # plot-4
    p4 <- ggstatsplot::gghistostats(
      data = dat2,
      x = brainwt,
      results.subtitle = FALSE,
      normal.curve = TRUE,
      bar.measure = "density",
      binwidth = 0.05,
      normal.curve.linetype = "dashed",
      messages = FALSE
    )

    # plot-5
    p5 <- ggstatsplot::gghistostats(
      data = dat2,
      x = brainwt,
      results.subtitle = FALSE,
      normal.curve = FALSE,
      messages = FALSE
    )

    # build plots
    pb1 <- ggplot2::ggplot_build(p1)
    pb2 <- ggplot2::ggplot_build(p2)
    pb3 <- ggplot2::ggplot_build(p3)
    pb4 <- ggplot2::ggplot_build(p4)
    pb5 <- ggplot2::ggplot_build(p5)

    # check data layers
    testthat::expect_equal(length(pb1$data), 4L)
    testthat::expect_equal(length(pb2$data), 4L)
    testthat::expect_equal(length(pb3$data), 4L)
    testthat::expect_equal(length(pb4$data), 4L)
    testthat::expect_equal(length(pb5$data), 3L)

    # check aesthetic of the respective layer
    testthat::expect_equal(dim(pb1$data[[2]]), c(101L, 8L))
    testthat::expect_identical(unique(pb1$data[[2]]$colour), "black")
    testthat::expect_identical(unique(pb1$data[[2]]$linetype), "solid")
    testthat::expect_equal(unique(pb1$data[[2]]$size), 1L)
    testthat::expect_identical(unique(pb3$data[[2]]$colour), "red")
    testthat::expect_equal(unique(pb3$data[[2]]$size), 0.8)
    testthat::expect_identical(unique(pb4$data[[2]]$linetype), "dashed")

    # even if binwidth changes mean of the distribution shouldn't change
    testthat::expect_identical(mean(pb1$data[[2]]$x), mean(pb2$data[[2]]$x))
    testthat::expect_identical(mean(pb3$data[[2]]$x), mean(pb4$data[[2]]$x))
    testthat::expect_equal(mean(pb1$data[[2]]$y), 14.02795, tolerance = 0.001)
    testthat::expect_equal(mean(pb2$data[[2]]$y), 0.008271081, tolerance = 0.001)
    testthat::expect_equal(mean(pb3$data[[2]]$y), 4.627659, tolerance = 0.001)
    testthat::expect_equal(mean(pb4$data[[2]]$y), 0.1082654, tolerance = 0.001)

    # annotation
    testthat::expect_null(pb1$plot$labels$caption, NULL)
    testthat::expect_null(pb2$plot$labels$caption, NULL)
    testthat::expect_null(pb3$plot$labels$caption, NULL)
    testthat::expect_null(pb4$plot$labels$caption, NULL)
  }
)

# checking with default binwidth -------------------------------------

testthat::test_that(
  desc = "checking with default binwidth",
  code = {
    testthat::skip_on_cran()

    # creating the plot
    set.seed(123)
    p <-
      suppressWarnings(ggstatsplot::gghistostats(
        data = morley,
        x = Speed,
        effsize.type = "d",
        effsize.noncentral = FALSE,
        bf.message = FALSE,
        ggplot.component = ggplot2::scale_x_continuous(
          sec.axis = ggplot2::dup_axis(name = ggplot2::element_blank())
        ),
        messages = FALSE
      ))

    # build the plot
    pb <- ggplot2::ggplot_build(p)

    # tests
    testthat::expect_null(pb$plot$labels$caption, NULL)
    testthat::expect_equal(pb$layout$panel_params[[1]]$x.range,
      c(582.75, 1127.25),
      tolerance = 0.001
    )
    testthat::expect_equal(pb$layout$panel_params[[1]]$x.range,
      pb$layout$panel_params[[1]]$x.sec.range,
      tolerance = 0.001
    )
    testthat::expect_identical(
      pb$layout$panel_params[[1]]$x.labels,
      c("600", "700", "800", "900", "1000", "1100")
    )
    testthat::expect_identical(
      pb$layout$panel_params[[1]]$x.labels,
      pb$layout$panel_params[[1]]$x.sec.labels
    )
    testthat::expect_equal(pb$layout$panel_params[[1]]$y.range,
      c(-1.15, 24.15),
      tolerance = 0.001
    )
    testthat::expect_identical(
      pb$layout$panel_params[[1]]$y.labels,
      c("0", "5", "10", "15", "20")
    )
  }
)

# subtitle return --------------------------------------------------

testthat::test_that(
  desc = "subtitle return",
  code = {
    testthat::skip_on_cran()

    # should return a list of length 3
    set.seed(123)
    p_sub <- ggstatsplot::gghistostats(
      data = ggplot2::msleep,
      x = brainwt,
      return = "subtitle",
      type = "np",
      test.value = 0.25,
      messages = FALSE
    )

    # tests
    testthat::expect_identical(
      p_sub,
      ggplot2::expr(
        paste(
          NULL,
          "log"["e"](italic("V")),
          " = ",
          "5.57",
          ", ",
          italic("p"),
          " = ",
          "< 0.001",
          ", ",
          italic("r"),
          " = ",
          "-0.58",
          ", CI"["95%"],
          " [",
          "-0.81",
          ", ",
          "-0.38",
          "]",
          ", ",
          italic("n"),
          " = ",
          56L
        )
      )
    )
  }
)
