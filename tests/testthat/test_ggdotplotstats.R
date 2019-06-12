context(desc = "ggdotplotstats")

# ggdotplotstats works ----------------------------------------------

testthat::test_that(
  desc = "ggdotplotstats works as expected",
  code = {

    # creating a new dataset
    morley_new <- morley %>%
      tibble::as_tibble(x = .) %>%
      dplyr::mutate(
        .data = .,
        Expt = dplyr::case_when(
          Expt == 1 ~ "1st",
          Expt == 2 ~ "2nd",
          Expt == 3 ~ "3rd",
          Expt == 4 ~ "4th",
          Expt == 5 ~ "5th"
        )
      )

    # creating the plot
    set.seed(123)
    p <-
      ggstatsplot::ggdotplotstats(
        data = morley_new,
        x = Speed,
        y = Expt,
        test.value = 800,
        type = "p",
        k = 4,
        effsize.type = "d",
        effsize.noncentral = FALSE,
        title = "Michelson-Morley experiment",
        caption = "Studies carried out in 1887",
        xlab = substitute(paste("Speed of light (", italic("c"), ")")),
        ylab = "Experimental run",
        bf.message = TRUE,
        ggplot.component = ggplot2::scale_x_continuous(
          breaks = seq(800, 900, 10),
          sec.axis = ggplot2::dup_axis()
        ),
        bf.prior = 0.88,
        test.value.line = TRUE,
        centrality.para = TRUE,
        messages = TRUE
      )

    # build the plot
    pb <- ggplot2::ggplot_build(p)

    # data used for statistics
    dat <- tibble::as_tibble(pb$data[[1]])

    # checking subtitle
    set.seed(123)
    p_subtitle <-
      ggstatsplot::subtitle_t_onesample(
        data = dat,
        x = x,
        effsize.type = "d",
        effsize.noncentral = FALSE,
        test.value = 800,
        type = "p",
        k = 4,
        messages = FALSE
      )

    # testing labels
    testthat::expect_identical(
      p$labels$x,
      ggplot2::expr(
        paste("Speed of light (", italic("c"), ")")
      )
    )
    testthat::expect_identical(pb$plot$labels$y, "Experimental run")
    testthat::expect_identical(pb$plot$labels$title, "Michelson-Morley experiment")
    testthat::expect_identical(pb$plot$labels$subtitle, p_subtitle)
    testthat::expect_identical(pb$plot$labels$caption, ggplot2::expr(atop(
      displaystyle("Studies carried out in 1887"),
      expr = paste(
        "In favor of null: ",
        "log"["e"],
        "(BF"["01"],
        ") = ",
        "-1.2779",
        ", ",
        italic("r")["Cauchy"],
        " = ",
        "0.8800"
      )
    )))

    # checking different data layers
    testthat::expect_equal(length(pb$data), 5L)
    testthat::expect_equal(nrow(pb$data[[1]]), 5L)
    testthat::expect_equal(pb$data[[1]]$x,
      c(820.5, 831.5, 845.0, 856.0, 909.0),
      tolerance = 0.001
    )
    testthat::expect_equal(pb$data[[4]]$xintercept,
      mean(dat$x, na.rm = TRUE),
      tolerance = 0.001
    )
    testthat::expect_equal(pb$data[[2]]$xintercept,
      800.000,
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
      ggplot2::expr("test" == "800")
    )
    testthat::expect_equal(
      pb$data[[5]]$label[[1]],
      ggplot2::expr("mean" == "852.40")
    )

    # checking panel parameters
    testthat::expect_equal(pb$layout$panel_params[[1]]$x.range,
      c(794.55, 914.45),
      tolerance = 0.001
    )
    testthat::expect_identical(
      pb$layout$panel_params[[1]]$x.labels,
      c("800", "810", "820", "830", "840", "850", "860", "870", "880", "890", "900")
    )
    testthat::expect_equal(pb$layout$panel_params[[1]]$y.range,
      c(0.8, 5.2),
      tolerance = 0.001
    )
    testthat::expect_identical(
      as.character(pb$layout$panel_params[[1]]$y.labels),
      c("4th", "5th", "3rd", "2nd", "1st")
    )
    testthat::expect_identical(
      as.character(pb$layout$panel_params[[1]]$y.sec.labels),
      c("0", "25", "50", "75", "100")
    )
    testthat::expect_equal(pb$layout$panel_params[[1]]$y.range,
      pb$layout$panel_params[[1]]$y.sec.range,
      tolerance = 0.001
    )
    testthat::expect_identical(
      pb$layout$panel_params[[1]]$x.labels,
      pb$layout$panel_params[[1]]$x.sec.labels
    )
  }
)

# ggdotplotstats works with summarized data -----------------------------------

testthat::test_that(
  desc = "ggdotplotstats works with summarized data",
  code = {
    testthat::skip_on_cran()

    # creating a summary data
    set.seed(123)
    df <-
      groupedstats::grouped_summary(
        data = iris,
        grouping.vars = Species,
        measures = Sepal.Length
      )

    # plot
    p <- ggstatsplot::ggdotplotstats(
      data = df,
      x = mean,
      y = Species,
      results.subtitle = FALSE,
      messages = FALSE
    )

    # build plot
    pb <- ggplot2::ggplot_build(p)

    # checking panel parameters
    testthat::expect_equal(pb$layout$panel_params[[1]]$x.range,
      c(4.931, 6.669),
      tolerance = 0.001
    )
    testthat::expect_identical(
      pb$layout$panel_params[[1]]$x.labels,
      c("5.0", "5.5", "6.0", "6.5")
    )
    testthat::expect_equal(pb$layout$panel_params[[1]]$y.range,
      c(0.9, 3.1),
      tolerance = 0.001
    )
    testthat::expect_identical(
      as.character(pb$layout$panel_params[[1]]$y.labels),
      c("setosa", "versicolor", "virginica")
    )
    testthat::expect_identical(
      as.character(pb$layout$panel_params[[1]]$y.sec.labels),
      c("0", "25", "50", "75", "100")
    )
    testthat::expect_equal(pb$layout$panel_params[[1]]$y.range,
      pb$layout$panel_params[[1]]$y.sec.range,
      tolerance = 0.001
    )
    testthat::expect_identical(
      pb$layout$panel_params[[1]]$x.labels,
      pb$layout$panel_params[[1]]$x.sec.labels
    )
    testthat::expect_null(pb$plot$labels$subtitle, NULL)
    testthat::expect_null(pb$plot$labels$caption, NULL)
  }
)

# subtitle return --------------------------------------------------

testthat::test_that(
  desc = "subtitle return",
  code = {
    testthat::skip_on_cran()

    # should return a list of length 3
    set.seed(123)
    p_sub <- suppressWarnings(ggstatsplot::ggdotplotstats(
      data = morley,
      x = Speed,
      y = Expt,
      test.value = 800,
      return = "subtitle",
      type = "np",
      messages = FALSE
    ))

    # tests
    testthat::expect_identical(
      p_sub,
      ggplot2::expr(
        paste(
          NULL,
          "log"["e"](italic("V")),
          " = ",
          "2.71",
          ", ",
          italic("p"),
          " = ",
          "0.059",
          ", ",
          italic(r),
          " = ",
          "0.90",
          ", CI"["95%"],
          " [",
          "0.88",
          ", ",
          "0.91",
          "]",
          ", ",
          italic("n"),
          " = ",
          5L
        )
      )
    )
  }
)
