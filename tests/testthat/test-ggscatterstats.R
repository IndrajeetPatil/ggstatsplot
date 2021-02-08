# pearson's r with NAs ---------------------------------------------

test_that(
  desc = "checking ggscatterstats - without NAs - pearson's r",
  code = {
    skip_on_cran()

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
        caption = "ggplot2 dataset",
        title = "Mammalian sleep"
      )

    # plot build
    pb <- ggplot2::ggplot_build(p)

    # check data
    set.seed(123)
    expect_snapshot(pb$data)

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
      statsExpressions::expr_corr_test(
        data = ggplot2::msleep,
        x = "sleep_total",
        y = bodywt,
        top.text = "ggplot2 dataset",
        output = "expression",
        type = "bayes"
      )

    # checking plot labels
    expect_identical(pb$plot$labels$caption, p_cap)
    expect_identical(pb$plot$labels$title, "Mammalian sleep")
    expect_identical(pb$plot$labels$subtitle, p_subtitle)
    expect_identical(pb$plot$labels$x, "sleep (total)")
    expect_identical(pb$plot$labels$y, "body weight")
    expect_identical(
      pb$data[[3]]$label,
      c("Asian elephant", "African elephant")
    )
  }
)

# spearman's rho with NAs ---------------------------------------------

test_that(
  desc = "checking ggscatterstats - without NAs - spearman's rho",
  code = {
    skip_on_cran()

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

    # check data
    set.seed(123)
    expect_snapshot(pb$data)

    # subtitle
    set.seed(123)
    p_subtitle <-
      statsExpressions::expr_corr_test(
        data = ggplot2::msleep,
        x = sleep_total,
        y = bodywt,
        type = "np",
        conf.level = 0.99
      )

    # testing data and annotations
    expect_identical(pb$plot$labels$subtitle, p_subtitle)
    expect_null(pb$plot$labels$caption, NULL)
  }
)


# percentage bend with NAs ---------------------------------------------

test_that(
  desc = "checking ggscatterstats - without NAs - percentage bend",
  code = {
    skip_on_cran()

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
        marginal = FALSE
      )

    # subtitle
    set.seed(123)
    p_subtitle <-
      statsExpressions::expr_corr_test(
        data = ggplot2::msleep,
        x = sleep_total,
        y = bodywt,
        type = "r",
        conf.level = 0.90
      )

    pb <- ggplot2::ggplot_build(p)

    # check data
    set.seed(123)
    expect_snapshot(pb$data)

    expect_identical(pb$plot$labels$subtitle, p_subtitle)
  }
)

# bayes factor plus class of object -----------------------------------------

test_that(
  desc = "bayes factor plus class of object",
  code = {
    skip_on_cran()

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
        ggplot.component = ggplot2::scale_y_continuous(breaks = seq(0, 6000, 1000))
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

    expect_identical(class(p)[[1]], "ggExtraPlot")
    expect_identical(
      enframe(p$grobs[[23]]$children)$value[[1]][[1]],
      "mammalian sleep dataset"
    )
    expect_identical(
      enframe(p$grobs[[17]]$children)$value[[1]][[1]],
      "source: ggplot2 package"
    )
    expect_identical(
      enframe(p$grobs[[12]]$children)$value[[1]][[1]],
      "total sleep"
    )
    expect_identical(
      enframe(p$grobs[[13]]$children)$value[[1]][[1]],
      "body weight"
    )
    expect_identical(
      enframe(p$grobs[[22]]$children)$value[[1]][[1]],
      p_subtitle
    )
  }
)

# aesthetic modifications work ---------------------------------------------

test_that(
  desc = "aesthetic modifications work",
  code = {
    skip_on_cran()

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

    # check data
    set.seed(123)
    expect_snapshot(pb$data)

    # both quoted
    expect_s3_class(p, "gg")
  }
)

# labeling input variations ---------------------------------------------

test_that(
  desc = "checking ggscatterstats with different kinds of inputs to labeling",
  code = {
    skip_on_cran()

    # both quoted
    expect_true(inherits(
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
    expect_true(inherits(
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
    expect_true(inherits(
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

test_that(
  desc = "with marginals",
  code = {
    skip_on_cran()

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

    expect_identical(
      class(p),
      c("ggExtraPlot", "gtable", "gTree", "grob", "gDesc")
    )
  }
)

# subtitle output ----------------------------------------------------------

test_that(
  desc = "subtitle output",
  code = {
    skip_on_cran()

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
    expect_identical(p_sub, fun_sub)
  }
)
