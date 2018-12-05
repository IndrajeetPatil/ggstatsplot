# context ------------------------------------------------------------
context(desc = "gghistostats")

# checking plot and parametric stats ---------------------------------------------

testthat::test_that(
  desc = "checking gghistostats plot and parametric stats - data with NAs",
  code = {
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
    testthat::expect_equal(data_dims, c(81L, 1L))

    # checking different data layers
    testthat::expect_equal(length(pb$data), 5L)
    testthat::expect_equal(nrow(pb$data[[1]]), 11L)
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
    testthat::expect_equal(pb$data[[1]]$y[1], 1L)
    testthat::expect_equal(pb$data[[1]]$y[7], 32L)
    testthat::expect_equal(pb$data[[1]]$x[1], 60L)
    testthat::expect_equal(pb$data[[1]]$x[7], 180L)
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
      NULL,
      expr = paste(
        "In favor of null: ",
        "log"["e"],
        "(BF"["01"],
        ") = ",
        "-13.55",
        ", Prior width = ",
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
        x = ggplot2::mpg$cty,
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
        x = ggplot2::mpg$cty,
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
      c("0.0%", "10.0%", "20.0%", "30.0%", "40.0%")
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
    # creating the plot
    set.seed(123)
    p <-
      ggstatsplot::gghistostats(
        x = mtcars$wt,
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
        x = mtcars$wt,
        test.value = 2.5,
        type = "r",
        messages = FALSE
      )

    # testing labels
    testthat::expect_identical(p$labels$subtitle, p_subtitle)
    testthat::expect_identical(p$labels$y, "proportion")

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
      c("0.0%", "10.0%", "20.0%")
    )
  }
)

# checking bayes stats and density -----------------------------------

testthat::test_that(
  desc = "checking bayes stats and density",
  code = {
    # creating the plot
    set.seed(123)
    p <-
      suppressMessages(ggstatsplot::gghistostats(
        x = morley$Speed,
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
        x = morley$Speed,
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
