# ggdotplotstats works ----------------------------------------------

testthat::test_that(
  desc = "ggdotplotstats works as expected",
  code = {
    testthat::skip_on_cran()

    # creating a new dataset
    morley_new <-
      morley %>%
      dplyr::mutate(
        .data = .,
        Expt = dplyr::case_when(
          Expt == 1 ~ "1st",
          Expt == 2 ~ "2nd",
          Expt == 3 ~ "3rd",
          Expt == 4 ~ "4th",
          Expt == 5 ~ "5th"
        )
      ) %>%
      as_tibble(x = .)

    # creating the plot
    set.seed(123)
    p <-
      ggstatsplot::ggdotplotstats(
        data = morley_new,
        x = Speed,
        y = "Expt",
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
        centrality.parameter = "mean",
        messages = TRUE
      )

    # build the plot
    pb <- ggplot2::ggplot_build(p)

    # checking subtitle
    set.seed(123)
    p_subtitle <-
      statsExpressions::expr_t_onesample(
        data = morley_new,
        x = Speed,
        y = "Expt",
        test.value = 800,
        type = "p",
        k = 4,
        effsize.type = "d",
        effsize.noncentral = FALSE,
        messages = FALSE
      )

    # testing labels
    testthat::expect_identical(
      p$labels$x,
      ggplot2::expr(paste("Speed of light (", italic("c"), ")"))
    )
    testthat::expect_identical(pb$plot$labels$y, "Experimental run")
    testthat::expect_identical(pb$plot$labels$title, "Michelson-Morley experiment")
    testthat::expect_identical(
      pb$plot$labels$caption,
      ggplot2::expr(atop(
        displaystyle("Studies carried out in 1887"),
        expr = paste(
          "In favor of null: ",
          "log"["e"],
          "(BF"["01"],
          ") = ",
          "-1.2779",
          ", ",
          italic("r")["Cauchy"]^"JZS",
          " = ",
          "0.8800"
        )
      ))
    )

    # checking panel parameters
    testthat::expect_equal(
      pb$layout$panel_params[[1]]$x$scale$range$range,
      c(800, 909)
    )
    testthat::expect_equal(
      pb$layout$panel_params[[1]]$x$breaks,
      c(800, 810, 820, 830, 840, 850, 860, 870, 880, 890, 900)
    )
    testthat::expect_equal(
      pb$layout$panel_params[[1]]$y.range,
      c(0.8, 5.2),
      tolerance = 0.001
    )
    testthat::expect_identical(
      pb$layout$panel_params[[1]]$y$scale$labels,
      structure(
        c(4L, 5L, 3L, 2L, 1L),
        .Label = c(
          "1st", "2nd", "3rd",
          "4th", "5th"
        ),
        class = "factor"
      )
    )
    testthat::expect_equal(
      pb$layout$panel_params[[1]]$y.sec$break_info,
      list(
        range = c(0.8, 5.2),
        labels = c(0, 25, 50, 75, 100),
        major = c(
          0.045,
          0.272, 0.499, 0.728, 0.955
        ),
        minor = c(
          0.045, 0.159, 0.272, 0.386,
          0.499, 0.614, 0.728, 0.841, 0.955
        ),
        major_source = c(
          0.998198198198198,
          1.997997997998,
          2.9977977977978,
          4.002002002002,
          5.0018018018018
        ),
        minor_source = c(
          0.998198198198198,
          1.5003003003003,
          1.997997997998,
          2.5001001001001,
          2.9977977977978,
          3.4998998998999,
          4.002002002002,
          4.4996996996997,
          5.0018018018018
        ),
        major_source_user = c(
          1, 2,
          3, 4, 5
        ),
        minor_source_user = c(
          1, 1.5, 2, 2.5, 3, 3.5, 4, 4.5,
          5
        )
      )
    )

    # geom data
    testthat::expect_equal(
      pb$data[[1]],
      structure(
        list(
          y = c(1, 2, 3, 4, 5),
          x = c(820.5, 831.5, 845, 856, 909),
          PANEL = structure(c(1L, 1L, 1L, 1L, 1L), class = "factor", .Label = "1"),
          group = structure(c(-1L, -1L, -1L, -1L, -1L), n = 1L),
          shape = c(16, 16, 16, 16, 16),
          colour = c("black", "black", "black", "black", "black"),
          size = c(3, 3, 3, 3, 3),
          fill = c(NA, NA, NA, NA, NA),
          alpha = c(NA, NA, NA, NA, NA),
          stroke = c(0.5, 0.5, 0.5, 0.5, 0.5)
        ),
        row.names = c(NA, -5L),
        class = "data.frame"
      )
    )

    testthat::expect_equal(
      pb$data[[2]],
      structure(
        list(
          xintercept = 800,
          PANEL = structure(1L, .Label = "1", class = "factor"),
          group = structure(c(-1L), n = 1L),
          colour = "black",
          size = 1,
          linetype = "dashed",
          alpha = NA
        ),
        row.names = c(NA, -1L),
        class = "data.frame"
      )
    )

    testthat::expect_equal(
      pb$data[[3]],
      structure(
        list(
          y = c(2.25, 2.25, 2.25, 2.25, 2.25),
          x = c(800, 800, 800, 800, 800),
          label = list(
            ggplot2::expr("test" == "800"),
            ggplot2::expr("test" == "800"),
            ggplot2::expr("test" == "800"),
            ggplot2::expr("test" == "800"),
            ggplot2::expr("test" == "800")
          ),
          PANEL = structure(c(1L, 1L, 1L, 1L, 1L), class = "factor", .Label = "1"),
          group = structure(c(-1L, -1L, -1L, -1L, -1L), n = 1L),
          colour = c("black", "black", "black", "black", "black"),
          fill = c("white", "white", "white", "white", "white"),
          size = c(3, 3, 3, 3, 3),
          angle = c(0, 0, 0, 0, 0),
          hjust = c(0.5, 0.5, 0.5, 0.5, 0.5),
          vjust = c(0.5, 0.5, 0.5, 0.5, 0.5),
          alpha = c(0.5, 0.5, 0.5, 0.5, 0.5),
          family = c("", "", "", "", ""),
          fontface = c(1, 1, 1, 1, 1),
          lineheight = c(1.2, 1.2, 1.2, 1.2, 1.2)
        ),
        row.names = c(NA, -5L),
        class = "data.frame"
      )
    )

    testthat::expect_equal(
      pb$data[[4]],
      structure(
        list(
          xintercept = 852.4,
          PANEL = structure(1L, .Label = "1", class = "factor"),
          group = structure(c(-1L), n = 1L),
          colour = "blue",
          size = 1,
          linetype = "dashed",
          alpha = NA
        ),
        row.names = c(NA, -1L),
        class = "data.frame"
      )
    )

    testthat::expect_equal(
      pb$data[[5]],
      structure(
        list(
          y = c(3.75, 3.75, 3.75, 3.75, 3.75),
          x = c(852.4, 852.4, 852.4, 852.4, 852.4),
          label = list(
            ggplot2::expr("mean" == "852.40"),
            ggplot2::expr("mean" == "852.40"),
            ggplot2::expr("mean" == "852.40"),
            ggplot2::expr("mean" == "852.40"),
            ggplot2::expr("mean" == "852.40")
          ),
          PANEL = structure(c(1L, 1L, 1L, 1L, 1L), class = "factor", .Label = "1"),
          group = structure(c(-1L, -1L, -1L, -1L, -1L), n = 1L),
          colour = c("blue", "blue", "blue", "blue", "blue"),
          fill = c("white", "white", "white", "white", "white"),
          size = c(3, 3, 3, 3, 3),
          angle = c(0, 0, 0, 0, 0),
          hjust = c(0.5, 0.5, 0.5, 0.5, 0.5),
          vjust = c(0.5, 0.5, 0.5, 0.5, 0.5),
          alpha = c(0.5, 0.5, 0.5, 0.5, 0.5),
          family = c("", "", "", "", ""),
          fontface = c(1, 1, 1, 1, 1),
          lineheight = c(1.2, 1.2, 1.2, 1.2, 1.2)
        ),
        row.names = c(NA, -5L),
        class = "data.frame"
      )
    )
  }
)

# messing with factors --------------------------------------------------

testthat::test_that(
  desc = "messing with factors",
  code = {
    testthat::skip_on_cran()

    # creating a new label for the dataset
    df_msleep <- ggplot2::msleep

    # leaving out a level
    df_msleep %<>% dplyr::filter(., vore != "omni")

    # reordering factor levels
    df_msleep %<>%
      dplyr::mutate(
        .data = .,
        vore = forcats::fct_relevel(vore, "herbi", "insecti", "carni")
      )

    # plot with original data
    p1 <-
      ggdotplotstats(
        data = df_msleep,
        y = vore,
        x = brainwt,
        results.subtitle = FALSE,
        messages = FALSE
      )

    # plot with modified data
    p2 <-
      ggdotplotstats(
        data = dplyr::filter(ggplot2::msleep, vore != "omni"),
        y = vore,
        x = brainwt,
        results.subtitle = FALSE,
        messages = FALSE
      )

    # build those plots
    pb1 <- ggplot2::ggplot_build(p1)
    pb2 <- ggplot2::ggplot_build(p2)

    # tests
    testthat::expect_identical(
      levels(pb1$plot$data$vore),
      c("herbi", "insecti", "carni")
    )
    testthat::expect_identical(
      levels(pb2$plot$data$vore),
      c("carni", "herbi", "insecti")
    )
    testthat::expect_identical(
      dplyr::select(pb1$plot$data, -vore),
      dplyr::select(pb2$plot$data, -vore)
    )
    testthat::expect_identical(pb1$data[[1]], pb2$data[[1]])
    testthat::expect_identical(pb1$data[[2]], pb2$data[[2]])
    testthat::expect_identical(pb1$data[[3]], pb2$data[[3]])
  }
)

# subtitle output -------------------------------------------------------

testthat::test_that(
  desc = "subtitle output",
  code = {
    testthat::skip_on_cran()

    # should output a list of length 3
    set.seed(123)
    p_sub <-
      suppressWarnings(ggstatsplot::ggdotplotstats(
        data = morley,
        x = Speed,
        y = Expt,
        test.value = 800,
        output = "subtitle",
        type = "np",
        messages = FALSE
      ))

    # tests
    set.seed(123)
    testthat::expect_identical(
      p_sub,
      suppressWarnings(ggstatsplot::gghistostats(
        data = dplyr::group_by(.data = morley, Expt) %>%
          dplyr::summarise(mean = mean(Speed)),
        x = mean,
        test.value = 800,
        output = "subtitle",
        type = "np",
        messages = FALSE
      ))
    )
  }
)
