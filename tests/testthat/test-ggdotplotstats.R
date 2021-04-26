# ggdotplotstats works ----------------------------------------------

test_that(
  desc = "ggdotplotstats works as expected",
  code = {
    skip_on_cran()

    # creating a new dataset
    morley_new <-
      dplyr::mutate(
        .data = morley,
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
      suppressMessages(ggdotplotstats(
        data = morley_new,
        x = Speed,
        y = "Expt",
        test.value = 800,
        type = "p",
        k = 4,
        effsize.type = "d",
        title = "Michelson-Morley experiment",
        caption = "Studies carried out in 1887",
        xlab = substitute(paste("Speed of light (", italic("c"), ")")),
        ylab = "Experimental run",
        bf.message = TRUE,
        ggplot.component = ggplot2::scale_x_continuous(
          breaks = seq(800, 900, 10),
          sec.axis = ggplot2::dup_axis()
        ),
        bf.prior = 0.88
      ))

    # build the plot
    pb <- ggplot2::ggplot_build(p)

    # checking subtitle
    set.seed(123)
    p_subtitle <-
      statsExpressions::one_sample_test(
        data = morley_new,
        x = Speed,
        y = "Expt",
        test.value = 800,
        type = "p",
        k = 4,
        effsize.type = "d"
      )$expression[[1]]

    # testing labels
    expect_identical(
      p$labels$x,
      ggplot2::expr(paste("Speed of light (", italic("c"), ")"))
    )
    expect_identical(pb$plot$labels$y, "Experimental run")
    expect_identical(pb$plot$labels$title, "Michelson-Morley experiment")

    # checking panel parameters
    expect_equal(
      pb$layout$panel_params[[1]]$x$scale$range$range,
      c(820.5, 909.0),
      tolerance = 0.001
    )
    expect_equal(
      pb$layout$panel_params[[1]]$x$breaks,
      c(NA, NA, 820, 830, 840, 850, 860, 870, 880, 890, 900)
    )
    expect_equal(
      pb$layout$panel_params[[1]]$y.range,
      c(0.8, 5.2),
      tolerance = 0.001
    )
    expect_identical(
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
    expect_equal(
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
      ),
      tolerance = 0.01
    )

    # check data
    set.seed(123)
    expect_snapshot(pb$data)
  }
)

# messing with factors --------------------------------------------------

test_that(
  desc = "messing with factors",
  code = {
    skip_on_cran()

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
        results.subtitle = FALSE
      )

    # plot with modified data
    p2 <-
      ggdotplotstats(
        data = dplyr::filter(ggplot2::msleep, vore != "omni"),
        y = vore,
        x = brainwt,
        results.subtitle = FALSE
      )

    # build those plots
    pb1 <- ggplot2::ggplot_build(p1)
    pb2 <- ggplot2::ggplot_build(p2)

    # tests
    expect_identical(
      levels(pb1$plot$data$vore),
      c("herbi", "insecti", "carni")
    )
    expect_identical(
      levels(pb2$plot$data$vore),
      c("carni", "herbi", "insecti")
    )
    expect_identical(
      dplyr::select(pb1$plot$data, -vore),
      dplyr::select(pb2$plot$data, -vore)
    )
    expect_identical(pb1$data[[1]], pb2$data[[1]])
    expect_identical(pb1$data[[2]], pb2$data[[2]])
  }
)

# subtitle output -------------------------------------------------------

test_that(
  desc = "subtitle output",
  code = {
    skip_on_cran()

    # should output a list of length 3
    set.seed(123)
    p_sub <-
      suppressWarnings(ggdotplotstats(
        data = morley,
        x = Speed,
        y = Expt,
        test.value = 800,
        type = "np",
        output = "subtitle"
      ))

    # tests
    set.seed(123)
    expect_identical(
      p_sub,
      suppressWarnings(gghistostats(
        data = dplyr::group_by(.data = morley, Expt) %>%
          dplyr::summarise(mean = mean(Speed)),
        x = mean,
        test.value = 800,
        type = "np",
        output = "subtitle"
      ))
    )
  }
)
