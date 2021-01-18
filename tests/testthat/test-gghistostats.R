# checking plot and parametric stats --------------------------------------

test_that(
  desc = "checking gghistostats plot and parametric stats - data with NAs",
  code = {
    skip_on_cran()

    # creating the plot
    set.seed(123)
    p <-
      ggstatsplot::gghistostats(
        data = dplyr::starwars,
        x = height,
        xlab = "character height",
        title = "starwars: character heights",
        binwidth = 20,
        bar.fill = "orange",
        test.value = 150,
        bf.prior = 0.9,
        centrality.k = 0
      )

    # build the plot
    pb <- ggplot2::ggplot_build(p)

    # checking geom data
    expect_equal(
      pb$data[[1]],
      structure(
        list(
          fill = c(
            "orange",
            "orange",
            "orange",
            "orange",
            "orange",
            "orange",
            "orange",
            "orange",
            "orange",
            "orange",
            "orange"
          ),
          y = c(1, 2, 4, 2, 3, 15, 32, 15, 5, 1, 1),
          count = c(
            1, 2,
            4, 2, 3, 15, 32, 15, 5, 1, 1
          ),
          x = c(
            60, 80, 100, 120, 140, 160,
            180, 200, 220, 240, 260
          ),
          xmin = c(
            50, 70, 90, 110, 130, 150,
            170, 190, 210, 230, 250
          ),
          xmax = c(
            70, 90, 110, 130, 150, 170,
            190, 210, 230, 250, 270
          ),
          density = c(
            0.000617283950617284,
            0.00123456790123457,
            0.00246913580246914,
            0.00123456790123457,
            0.00185185185185185,
            0.00925925925925926,
            0.0197530864197531,
            0.00925925925925926,
            0.00308641975308642,
            0.000617283950617284,
            0.000617283950617284
          ),
          ncount = c(
            0.03125,
            0.0625,
            0.125,
            0.0625,
            0.09375,
            0.46875,
            1,
            0.46875,
            0.15625,
            0.03125,
            0.03125
          ),
          ndensity = c(
            0.03125,
            0.0625,
            0.125,
            0.0625,
            0.09375,
            0.46875,
            1,
            0.46875,
            0.15625,
            0.03125,
            0.03125
          ),
          flipped_aes = c(
            FALSE,
            FALSE,
            FALSE,
            FALSE,
            FALSE,
            FALSE,
            FALSE,
            FALSE,
            FALSE,
            FALSE,
            FALSE
          ),
          PANEL = structure(
            c(
              1L,
              1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L
            ),
            .Label = "1",
            class = "factor"
          ),
          group = c(
            -1L, -1L, -1L, -1L, -1L, -1L, -1L, -1L, -1L, -1L,
            -1L
          ),
          ymin = c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0),
          ymax = c(
            1,
            2, 4, 2, 3, 15, 32, 15, 5, 1, 1
          ),
          colour = c(
            "black",
            "black",
            "black",
            "black",
            "black",
            "black",
            "black",
            "black",
            "black",
            "black",
            "black"
          ),
          size = c(
            0.5, 0.5, 0.5, 0.5, 0.5, 0.5,
            0.5, 0.5, 0.5, 0.5, 0.5
          ),
          linetype = c(
            1, 1, 1, 1, 1, 1,
            1, 1, 1, 1, 1
          ),
          alpha = c(
            0.7, 0.7, 0.7, 0.7, 0.7, 0.7, 0.7,
            0.7, 0.7, 0.7, 0.7
          )
        ),
        row.names = c(NA, -11L),
        class = "data.frame"
      )
    )

    expect_equal(
      pb$data[[2]],
      structure(
        list(
          xintercept = 174.358024691358,
          PANEL = structure(1L, .Label = "1", class = "factor"),
          group = structure(-1L, n = 1L),
          colour = "blue",
          size = 1,
          linetype = "dashed",
          alpha = NA
        ),
        row.names = c(NA, -1L),
        class = "data.frame"
      )
    )

    # checking different data layers
    expect_equal(length(pb$data), 3L)
    expect_equal(dim(pb$data[[1]]), c(11L, 18L))
    expect_equal(dim(pb$data[[2]]), c(1L, 7L))
    expect_equal(dim(pb$data[[3]]), c(81L, 15L))

    expect_equal(
      class(pb$data[[3]]$label[[1]]),
      "character"
    )
    expect_identical(
      pb$data[[3]]$label[[1]],
      "list(~widehat(mu)[mean]=='174')"
    )
    expect_null(pb$layout$panel_params[[1]]$y.sec.labels, NULL)

    # checking subtitle
    set.seed(123)
    p_subtitle <-
      statsExpressions::expr_t_onesample(
        data = dplyr::starwars,
        x = height,
        type = "p",
        test.value = 150
      )

    # checking caption
    set.seed(123)
    p_cap <-
      statsExpressions::bf_ttest(
        data = dplyr::starwars,
        x = height,
        type = "p",
        test.value = 150,
        bf.prior = 0.9,
        output = "caption"
      )

    # testing overall call
    expect_identical(pb$plot$labels$subtitle, p_subtitle)
    expect_identical(pb$plot$labels$title, "starwars: character heights")
    expect_identical(pb$plot$labels$x, "character height")
    expect_identical(pb$plot$labels$caption, p_cap)
  }
)

# checking plot and non-parametric stats -----------------------------------

test_that(
  desc = "checking gghistostats and non-parametric stats - data without NAs",
  code = {
    skip_on_cran()

    # creating the plot
    set.seed(123)
    p <-
      ggstatsplot::gghistostats(
        data = ggplot2::mpg,
        x = cty,
        xlab = "city miles per gallon",
        title = "fuel economy",
        caption = substitute(paste(italic("source"), ": government website")),
        binwidth = 5,
        test.value = 20,
        k = 3,
        type = "np",
        centrality.k = 2,
        results.subtitle = FALSE
      )

    # build the plot
    pb <- ggplot2::ggplot_build(p)

    # checking different data layers
    expect_equal(length(pb$data), 3L)
    expect_equal(nrow(pb$data[[1]]), 6L)
    expect_equal(
      pb$data[[2]]$xintercept,
      median(ggplot2::mpg$cty, na.rm = TRUE),
      tolerance = 0.001
    )
    expect_equal(pb$data[[2]]$xintercept,
      17.000,
      tolerance = 0.001
    )
    expect_identical(
      pb$data[[3]]$label[[1]],
      "list(~widehat(mu)[median]=='17.00')"
    )
    expect_equal(pb$data[[1]]$y[1], 33L)
    expect_equal(pb$data[[1]]$y[6], 2L)
    expect_equal(pb$data[[1]]$x[1], 10L)
    expect_equal(pb$data[[1]]$x[6], 35L)
    expect_equal(pb$data[[1]]$xmin[1], 7.5, tolerance = 0.001)
    expect_equal(pb$data[[1]]$xmax[1], 12.5, tolerance = 0.001)
    expect_equal(pb$data[[1]]$xmin[6], 32.5, tolerance = 0.001)
    expect_equal(pb$data[[1]]$xmax[6], 37.5, tolerance = 0.001)

    # checking panel parameters
    expect_equal(pb$layout$panel_params[[1]]$x$limits, c(7.5, 37.5))
    expect_identical(
      pb$layout$panel_params[[1]]$x$breaks,
      c(NA, 10, 20, 30, NA)
    )
    expect_identical(
      pb$layout$panel_params[[1]]$y$breaks,
      c(0, 25, 50, 75, 100)
    )
    expect_equal(
      pb$layout$panel_params[[1]]$y.sec$break_info,
      list(
        range = c(-0.0211538461538462, 0.444230769230769),
        labels = c("0%", "10%", "20%", "30%", "40%"),
        major = c(
          0.045, 0.26, 0.475, 0.69,
          0.905
        ),
        minor = c(
          0.045, 0.153, 0.26, 0.367, 0.475, 0.583, 0.69,
          0.798, 0.905
        ),
        major_source = c(
          -0.044594594594594,
          23.3923423423423,
          46.8292792792793,
          70.1572072072072,
          93.5941441441442
        ),
        minor_source = c(
          -0.044594594594594,
          11.7283783783784,
          23.3923423423423,
          35.0563063063063,
          46.8292792792793,
          58.4932432432432,
          70.1572072072072,
          81.9301801801802,
          93.5941441441442
        ),
        major_source_user = c(0, 0.1, 0.2, 0.3, 0.4),
        minor_source_user = c(
          0,
          0.05, 0.1, 0.15, 0.2, 0.25, 0.3, 0.35, 0.4
        )
      )
    )

    # testing labels
    expect_identical(p$labels$subtitle, NULL)
    expect_identical(p$labels$title, "fuel economy")
    expect_identical(p$labels$x, "city miles per gallon")
    expect_identical(p$labels$y, "count")
    expect_identical(
      p$labels$caption,
      ggplot2::expr(paste(
        italic("source"),
        ": government website"
      ))
    )
  }
)

# checking robust stats and proportions -----------------------------------

test_that(
  desc = "checking robust stats and proportions",
  code = {
    skip_on_cran()

    # creating the plot
    set.seed(123)
    p <-
      ggstatsplot::gghistostats(
        data = mtcars,
        x = wt,
        binwidth = 0.5,
        test.value = 2.5,
        type = "r"
      ) +
      scale_x_continuous(limits = c(1, 6))

    # build the plot
    pb <- ggplot2::ggplot_build(p)

    # checking subtitle
    set.seed(123)
    p_subtitle <-
      statsExpressions::expr_t_onesample(
        data = mtcars,
        x = wt,
        test.value = 2.5,
        type = "r"
      )

    # testing labels
    expect_identical(pb$plot$labels$subtitle, p_subtitle)
    expect_null(pb$plot$labels$caption, NULL)
    expect_identical(pb$plot$labels$y, "count")

    expect_identical(
      pb$data[[3]]$label[[1]],
      "list(~widehat(mu)[trimmed]=='3.15')"
    )

    # checking different data layers
    expect_equal(length(pb$data), 3L)
    expect_equal(nrow(pb$data[[1]]), 11L)
    expect_equal(
      pb$data[[1]]$y,
      c(0, 2, 4, 3, 7, 9, 4, 0, 1, 2, 0),
      tolerance = 0.001
    )
    expect_equal(
      pb$data[[1]]$x,
      c(1.0, 1.5, 2.0, 2.5, 3.0, 3.5, 4.0, 4.5, 5.0, 5.5, 6.0),
      tolerance = 0.01
    )
  }
)

# checking if normal curve works -------------------------------------

test_that(
  desc = "checking if normal curve work",
  code = {
    skip_on_cran()

    # plot
    set.seed(123)
    p1 <-
      ggstatsplot::gghistostats(
        data = ggplot2::msleep,
        x = awake,
        binwidth = 1,
        results.subtitle = FALSE,
        normal.curve = TRUE,
        normal.curve.args =
          list(
            color = "red",
            size = 0.8
          )
      )

    # build plots
    pb1 <- ggplot2::ggplot_build(p1)

    # check data layers
    expect_equal(length(pb1$data), 4L)

    # check individual layers
    expect_equal(
      pb1$data[[1]],
      structure(list(fill = c(
        "grey50", "grey50", "grey50", "grey50",
        "grey50", "grey50", "grey50", "grey50", "grey50", "grey50", "grey50",
        "grey50", "grey50", "grey50", "grey50", "grey50", "grey50", "grey50",
        "grey50"
      ), y = c(
        2, 1, 2, 3, 4, 4, 7, 7, 2, 6, 12, 8, 5, 1, 4,
        4, 5, 5, 1
      ), count = c(
        2, 1, 2, 3, 4, 4, 7, 7, 2, 6, 12, 8, 5,
        1, 4, 4, 5, 5, 1
      ), x = c(
        4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14,
        15, 16, 17, 18, 19, 20, 21, 22
      ), xmin = c(
        3.5, 4.5, 5.5, 6.5,
        7.5, 8.5, 9.5, 10.5, 11.5, 12.5, 13.5, 14.5, 15.5, 16.5, 17.5,
        18.5, 19.5, 20.5, 21.5
      ), xmax = c(
        4.5, 5.5, 6.5, 7.5, 8.5, 9.5,
        10.5, 11.5, 12.5, 13.5, 14.5, 15.5, 16.5, 17.5, 18.5, 19.5, 20.5,
        21.5, 22.5
      ), density = c(
        0.0240963855421687, 0.0120481927710843,
        0.0240963855421687, 0.036144578313253, 0.0481927710843374, 0.0481927710843374,
        0.0843373493975904, 0.0843373493975904, 0.0240963855421687, 0.072289156626506,
        0.144578313253012, 0.0963855421686747, 0.0602409638554217, 0.0120481927710843,
        0.0481927710843374, 0.0481927710843374, 0.0602409638554217, 0.0602409638554217,
        0.0120481927710843
      ), ncount = c(
        0.166666666666667, 0.0833333333333333,
        0.166666666666667, 0.25, 0.333333333333333, 0.333333333333333,
        0.583333333333333, 0.583333333333333, 0.166666666666667, 0.5,
        1, 0.666666666666667, 0.416666666666667, 0.0833333333333333,
        0.333333333333333, 0.333333333333333, 0.416666666666667, 0.416666666666667,
        0.0833333333333333
      ), ndensity = c(
        0.166666666666667, 0.0833333333333333,
        0.166666666666667, 0.25, 0.333333333333333, 0.333333333333333,
        0.583333333333333, 0.583333333333333, 0.166666666666667, 0.5,
        1, 0.666666666666667, 0.416666666666667, 0.0833333333333333,
        0.333333333333333, 0.333333333333333, 0.416666666666667, 0.416666666666667,
        0.0833333333333333
      ), flipped_aes = c(
        FALSE, FALSE, FALSE, FALSE,
        FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE,
        FALSE, FALSE, FALSE, FALSE, FALSE, FALSE
      ), PANEL = structure(c(
        1L,
        1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L,
        1L, 1L
      ), .Label = "1", class = "factor"), group = c(
        -1L, -1L,
        -1L, -1L, -1L, -1L, -1L, -1L, -1L, -1L, -1L, -1L, -1L, -1L, -1L,
        -1L, -1L, -1L, -1L
      ), ymin = c(
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
        0, 0, 0, 0, 0, 0, 0, 0
      ), ymax = c(
        2, 1, 2, 3, 4, 4, 7, 7, 2,
        6, 12, 8, 5, 1, 4, 4, 5, 5, 1
      ), colour = c(
        "black", "black",
        "black", "black", "black", "black", "black", "black", "black",
        "black", "black", "black", "black", "black", "black", "black",
        "black", "black", "black"
      ), size = c(
        0.5, 0.5, 0.5, 0.5, 0.5,
        0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5,
        0.5
      ), linetype = c(
        1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
        1, 1, 1, 1, 1
      ), alpha = c(
        0.7, 0.7, 0.7, 0.7, 0.7, 0.7, 0.7,
        0.7, 0.7, 0.7, 0.7, 0.7, 0.7, 0.7, 0.7, 0.7, 0.7, 0.7, 0.7
      )), row.names = c(
        NA,
        -19L
      ), class = "data.frame")
    )

    expect_equal(
      pb1$data[[3]],
      structure(
        list(
          xintercept = 13.5674698795181,
          PANEL = structure(1L, .Label = "1", class = "factor"),
          group = structure(-1L, n = 1L),
          colour = "blue",
          size = 1,
          linetype = "dashed",
          alpha = NA
        ),
        row.names = c(NA, -1L),
        class = "data.frame"
      )
    )

    # annotation
    expect_equal(
      pb1$plot$labels,
      list(
        x = "awake",
        y = "count",
        title = NULL,
        subtitle = NULL,
        caption = NULL,
        fill = "count",
        weight = "weight",
        xintercept = "xintercept",
        label = "centrality_df$label[[1]]"
      )
    )
  }
)

# subtitle output --------------------------------------------------

test_that(
  desc = "subtitle output",
  code = {
    skip_on_cran()

    # should output a list of length 3
    set.seed(123)
    p_sub <-
      ggstatsplot::gghistostats(
        data = ggplot2::msleep,
        x = brainwt,
        output = "subtitle",
        type = "np",
        test.value = 0.25
      )

    set.seed(123)
    sub <-
      statsExpressions::expr_t_onesample(
        data = ggplot2::msleep,
        x = brainwt,
        output = "subtitle",
        type = "np",
        test.value = 0.25
      )

    # tests
    expect_identical(p_sub, sub)
  }
)
