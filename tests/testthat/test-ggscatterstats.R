# pearson's r with NAs ---------------------------------------------

testthat::test_that(
  desc = "checking ggscatterstats - without NAs - pearson's r",
  code = {
    testthat::skip_on_cran()

    # creating the plot
    set.seed(123)
    p <-
      ggstatsplot::ggscatterstats(
        data = ggplot2::msleep,
        x = sleep_total,
        y = "bodywt",
        label.var = "name",
        label.expression = bodywt > 2000,
        xlab = "sleep (total)",
        ylab = "body weight",
        type = "p",
        xfill = "red",
        yfill = "orange",
        marginal = FALSE,
        bf.message = TRUE,
        caption = "ggplot2 dataset",
        title = "Mammalian sleep"
      )

    # plot build
    pb <- ggplot2::ggplot_build(p)

    # checking layer data
    testthat::expect_equal(length(pb$data), 3L)
    testthat::expect_equal(dim(pb$data[[1]]), c(83L, 10L))
    testthat::expect_equal(dim(pb$data[[2]]), c(80L, 14L))

    # checking outliers
    testthat::expect_equal(
      pb$data[[3]],
      structure(
        list(
          x = c(3.9, 3.3),
          y = c(2547, 6654),
          label = c(
            "Asian elephant",
            "African elephant"
          ),
          PANEL = structure(c(1L, 1L), .Label = "1", class = "factor"),
          group = structure(c(-1L, -1L), n = 1L),
          colour = c(
            "black",
            "black"
          ),
          fill = c("white", "white"),
          size = c(3, 3),
          angle = c(
            0,
            0
          ),
          alpha = c(NA, NA),
          family = c("", ""),
          fontface = c(
            1,
            1
          ),
          lineheight = c(1.2, 1.2),
          hjust = c(0.5, 0.5),
          vjust = c(
            0.5,
            0.5
          ),
          point.size = c(1, 1),
          segment.linetype = c(1, 1),
          segment.size = c(
            0.5,
            0.5
          ),
          segment.curvature = c(0, 0),
          segment.angle = c(
            90,
            90
          ),
          segment.ncp = c(1, 1),
          segment.shape = c(0.5, 0.5),
          segment.square = c(TRUE, TRUE),
          segment.squareShape = c(
            1,
            1
          ),
          segment.inflect = c(FALSE, FALSE),
          segment.debug = c(
            FALSE,
            FALSE
          )
        ),
        row.names = c(NA, -2L),
        class = "data.frame"
      )
    )

    # subtitle
    set.seed(123)
    p_subtitle <-
      statsExpressions::expr_corr_test(
        data = ggplot2::msleep,
        x = "sleep_total",
        y = bodywt,
        type = "p"
      )

    # subtitle
    set.seed(123)
    p_cap <-
      statsExpressions::bf_corr_test(
        data = ggplot2::msleep,
        x = "sleep_total",
        y = bodywt,
        top.text = "ggplot2 dataset",
        output = "expression"
      )

    # checking plot labels
    testthat::expect_identical(pb$plot$labels$caption, p_cap)
    testthat::expect_identical(pb$plot$labels$title, "Mammalian sleep")
    testthat::expect_identical(pb$plot$labels$subtitle, p_subtitle)
    testthat::expect_identical(pb$plot$labels$x, "sleep (total)")
    testthat::expect_identical(pb$plot$labels$y, "body weight")
    testthat::expect_identical(
      pb$data[[3]]$label,
      c("Asian elephant", "African elephant")
    )
  }
)

# spearman's rho with NAs ---------------------------------------------

testthat::test_that(
  desc = "checking ggscatterstats - without NAs - spearman's rho",
  code = {
    testthat::skip_on_cran()

    # creating the plot
    set.seed(123)
    p <-
      ggstatsplot::ggscatterstats(
        data = ggplot2::msleep,
        x = "sleep_total",
        y = bodywt,
        centrality.parameter = "none",
        type = "np",
        conf.level = 0.99,
        marginal = FALSE
      )

    # build the plot
    pb <- ggplot2::ggplot_build(p)

    # subtitle
    set.seed(123)
    p_subtitle <-
      statsExpressions::expr_corr_test(
        data = ggplot2::msleep,
        x = sleep_total,
        y = bodywt,
        type = "np",
        conf.level = 0.99,
        messages = FALSE
      )

    # testing data and annotations
    testthat::expect_equal(length(pb$data), 2L)
    testthat::expect_identical(pb$plot$labels$subtitle, p_subtitle)
    testthat::expect_null(pb$plot$labels$caption, NULL)
  }
)


# percentage bend with NAs ---------------------------------------------

testthat::test_that(
  desc = "checking ggscatterstats - without NAs - percentage bend",
  code = {
    testthat::skip_on_cran()

    # creating the plot
    set.seed(123)
    p <-
      ggstatsplot::ggscatterstats(
        data = ggplot2::msleep,
        x = sleep_total,
        y = bodywt,
        type = "r",
        centrality.parameter = "mean",
        conf.level = 0.90,
        point.args = list(color = "red", size = 5),
        marginal = FALSE,
        messages = FALSE
      )

    # subtitle
    set.seed(123)
    p_subtitle <-
      statsExpressions::expr_corr_test(
        data = ggplot2::msleep,
        x = sleep_total,
        y = bodywt,
        type = "r",
        conf.level = 0.90,
        messages = FALSE
      )

    # built plot
    pb <- ggplot2::ggplot_build(p)

    testthat::expect_identical(pb$plot$labels$subtitle, p_subtitle)

    # checking layered data
    testthat::expect_equal(unique(pb$data[[1]]$size), 5L)
    testthat::expect_equal(unique(pb$data[[1]]$shape), 19L)
    testthat::expect_identical(unique(pb$data[[1]]$colour), "red")
  }
)

# bayes factor plus class of object -----------------------------------------

testthat::test_that(
  desc = "bayes factor plus class of object",
  code = {
    testthat::skip_on_cran()

    # creating the plot
    set.seed(123)
    p <-
      ggstatsplot::ggscatterstats(
        data = ggplot2::msleep,
        x = sleep_total,
        y = bodywt,
        xlab = "total sleep",
        ylab = "body weight",
        title = "mammalian sleep dataset",
        caption = "source: ggplot2 package",
        type = "bf",
        ggplot.component = ggplot2::scale_y_continuous(breaks = seq(0, 6000, 1000)),
        messages = FALSE
      )

    # subtitle
    set.seed(123)
    p_subtitle <-
      statsExpressions::expr_corr_test(
        data = ggplot2::msleep,
        x = sleep_total,
        y = bodywt,
        type = "bf"
      )

    testthat::expect_identical(class(p)[[1]], "ggExtraPlot")
    testthat::expect_identical(
      enframe(p$grobs[[23]]$children)$value[[1]][[1]],
      "mammalian sleep dataset"
    )
    testthat::expect_identical(
      enframe(p$grobs[[17]]$children)$value[[1]][[1]],
      "source: ggplot2 package"
    )
    testthat::expect_identical(
      enframe(p$grobs[[12]]$children)$value[[1]][[1]],
      "total sleep"
    )
    testthat::expect_identical(
      enframe(p$grobs[[13]]$children)$value[[1]][[1]],
      "body weight"
    )
    testthat::expect_identical(
      enframe(p$grobs[[22]]$children)$value[[1]][[1]],
      p_subtitle
    )
  }
)

# aesthetic modifications work ---------------------------------------------

testthat::test_that(
  desc = "aesthetic modifications work",
  code = {
    testthat::skip_on_cran()

    # creating the plot
    set.seed(123)
    p <-
      ggstatsplot::ggscatterstats(
        data = ggplot2::msleep,
        x = sleep_total,
        y = sleep_cycle,
        label.expression = "sleep_total > 17",
        label.var = "order",
        point.label.args = list(size = 4, color = "blue", alpha = 0.5),
        results.subtitle = FALSE,
        marginal = FALSE
      ) +
      ggplot2::coord_cartesian(ylim = c(0, 7000)) +
      ggplot2::scale_y_continuous(breaks = seq(0, 7000, 1000))

    # build the plot
    pb <- ggplot2::ggplot_build(p)

    testthat::expect_equal(
      pb$data[[3]],
      structure(
        list(
          y = c(0.383333333, 0.333333333, 0.116666667, 0.2),
          x = c(17.4, 18, 19.7, 19.9),
          label = c(
            "Cingulata", "Didelphimorphia",
            "Chiroptera", "Chiroptera"
          ),
          PANEL = structure(c(
            1L, 1L, 1L,
            1L
          ), .Label = "1", class = "factor"),
          group = structure(c(
            -1L,
            -1L, -1L, -1L
          ), n = 1L),
          colour = c("blue", "blue", "blue", "blue"),
          fill = c("white", "white", "white", "white"),
          size = c(
            4,
            4, 4, 4
          ),
          angle = c(0, 0, 0, 0),
          alpha = c(0.5, 0.5, 0.5, 0.5),
          family = c("", "", "", ""),
          fontface = c(1, 1, 1, 1),
          lineheight = c(
            1.2,
            1.2, 1.2, 1.2
          ),
          hjust = c(0.5, 0.5, 0.5, 0.5),
          vjust = c(
            0.5,
            0.5, 0.5, 0.5
          ),
          point.size = c(1, 1, 1, 1),
          segment.linetype = c(
            1,
            1, 1, 1
          ),
          segment.size = c(0.5, 0.5, 0.5, 0.5),
          segment.curvature = c(
            0,
            0, 0, 0
          ),
          segment.angle = c(90, 90, 90, 90),
          segment.ncp = c(
            1,
            1, 1, 1
          ),
          segment.shape = c(0.5, 0.5, 0.5, 0.5),
          segment.square = c(
            TRUE,
            TRUE, TRUE, TRUE
          ),
          segment.squareShape = c(1, 1, 1, 1),
          segment.inflect = c(
            FALSE,
            FALSE, FALSE, FALSE
          ),
          segment.debug = c(
            FALSE, FALSE, FALSE,
            FALSE
          )
        ),
        row.names = c(NA, -4L),
        class = "data.frame"
      )
    )

    # both quoted
    testthat::expect_s3_class(p, "gg")
  }
)

# labeling input variations ---------------------------------------------

testthat::test_that(
  desc = "checking ggscatterstats with different kinds of inputs to labeling",
  code = {
    testthat::skip_on_cran()

    # both quoted
    testthat::expect_true(inherits(
      ggstatsplot::ggscatterstats(
        data = ggplot2::msleep,
        x = sleep_total,
        y = sleep_cycle,
        label.expression = "sleep_total > 17",
        label.var = "order",
        results.subtitle = FALSE,
        marginal = FALSE
      ),
      what = "gg"
    ))

    # both unquoted
    testthat::expect_true(inherits(
      ggstatsplot::ggscatterstats(
        data = ggplot2::msleep,
        x = sleep_total,
        y = sleep_cycle,
        label.expression = sleep_total > 17,
        label.var = order,
        results.subtitle = FALSE,
        marginal = FALSE
      ),
      what = "gg"
    ))

    # label.expression not specified
    testthat::expect_true(inherits(
      ggstatsplot::ggscatterstats(
        data = dplyr::sample_frac(ggplot2::msleep, 0.1),
        x = sleep_total,
        y = sleep_cycle,
        label.expression = NULL,
        label.var = order,
        results.subtitle = FALSE,
        marginal = FALSE
      ),
      what = "gg"
    ))
  }
)

# with marginals ----------------------------------------------------------

testthat::test_that(
  desc = "with marginals",
  code = {
    testthat::skip_on_cran()

    # creating the plot
    set.seed(123)
    p <-
      ggstatsplot::ggscatterstats(
        data = ggplot2::msleep,
        x = sleep_total,
        y = bodywt,
        margins = "y",
        results.subtitle = FALSE
      )

    testthat::expect_identical(
      class(p),
      c("ggExtraPlot", "gtable", "gTree", "grob", "gDesc")
    )
  }
)

# subtitle output ----------------------------------------------------------

testthat::test_that(
  desc = "subtitle output",
  code = {
    testthat::skip_on_cran()

    # creating the messages
    set.seed(123)
    p_sub <-
      ggstatsplot::ggscatterstats(
        data = dplyr::starwars,
        x = mass,
        y = height,
        conf.level = 0.90,
        type = "r",
        output = "subtitle"
      )

    fun_sub <-
      statsExpressions::expr_corr_test(
        data = dplyr::starwars,
        x = mass,
        y = height,
        conf.level = 0.90,
        type = "r",
        output = "subtitle"
      )

    # checking captured messages
    testthat::expect_identical(p_sub, fun_sub)
  }
)
