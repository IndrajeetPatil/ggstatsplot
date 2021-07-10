

# pearson's r with NAs ---------------------------------------------

test_that(
  desc = "checking ggscatterstats - without NAs - pearson's r",
  code = {
    skip_on_cran()

    # creating the plot
    set.seed(123)
    p <-
      ggscatterstats(
        data = dplyr::filter(ggplot2::msleep, conservation == "lc"),
        x = sleep_total,
        y = "sleep_cycle",
        label.var = "name",
        label.expression = sleep_cycle > 0.3,
        xlab = "sleep (total)",
        ylab = "sleep cycle",
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
    expect_snapshot(list(pb$data[[1]], head(pb$data[[2]]), pb$data[[3]]))

    # subtitle
    set.seed(123)
    p_subtitle <-
      statsExpressions::corr_test(
        data = dplyr::filter(ggplot2::msleep, conservation == "lc"),
        x = "sleep_total",
        y = sleep_cycle,
        type = "p"
      )$expression[[1]]

    # subtitle
    set.seed(123)
    p_cap <-
      statsExpressions::corr_test(
        data = dplyr::filter(ggplot2::msleep, conservation == "lc"),
        x = "sleep_total",
        y = sleep_cycle,
        top.text = "ggplot2 dataset",
        type = "bayes"
      )$expression[[1]]

    # checking plot labels
    expect_identical(pb$plot$labels$caption, p_cap)
    expect_identical(pb$plot$labels$subtitle, p_subtitle)
    expect_snapshot(within(pb$plot$labels, rm(subtitle, caption)))
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
      ggscatterstats(
        data = dplyr::filter(ggplot2::msleep, conservation == "lc"),
        x = "sleep_total",
        y = sleep_cycle,
        type = "np",
        conf.level = 0.99,
        marginal = FALSE
      )

    # build the plot
    pb <- ggplot2::ggplot_build(p)

    # check data
    set.seed(123)
    expect_snapshot(pb$data[[1]])

    # subtitle
    set.seed(123)
    p_subtitle <-
      statsExpressions::corr_test(
        data = dplyr::filter(ggplot2::msleep, conservation == "lc"),
        x = sleep_total,
        y = sleep_cycle,
        type = "np",
        conf.level = 0.99
      )$expression[[1]]

    # testing data and annotations
    expect_identical(pb$plot$labels$subtitle, p_subtitle)
    expect_snapshot(within(pb$plot$labels, rm(subtitle)))
  }
)


# winsorized Pearson with NAs ---------------------------------------------

test_that(
  desc = "checking ggscatterstats - without NAs - winsorized Pearson",
  code = {
    skip_on_cran()

    # creating the plot
    set.seed(123)
    p <-
      ggscatterstats(
        data = dplyr::filter(ggplot2::msleep, conservation == "lc"),
        x = sleep_total,
        y = sleep_cycle,
        type = "r",
        conf.level = 0.90,
        point.args = list(color = "red", size = 5, stroke = 0),
        marginal = FALSE
      )

    # subtitle
    set.seed(123)
    p_subtitle <-
      statsExpressions::corr_test(
        data = dplyr::filter(ggplot2::msleep, conservation == "lc"),
        x = sleep_total,
        y = sleep_cycle,
        type = "r",
        conf.level = 0.90
      )$expression[[1]]

    pb <- ggplot2::ggplot_build(p)

    # check data
    set.seed(123)
    expect_snapshot(pb$data[[1]])

    expect_identical(pb$plot$labels$subtitle, p_subtitle)
    expect_snapshot(within(pb$plot$labels, rm(subtitle)))
  }
)

# bayes factor plus class of object -----------------------------------------

test_that(
  desc = "bayes factor plus class of object",
  code = {
    skip_on_cran()
    skip_if_not_installed("ggExtra")

    # creating the plot
    set.seed(123)
    p <-
      ggscatterstats(
        data = dplyr::filter(ggplot2::msleep, conservation == "lc"),
        x = sleep_total,
        y = sleep_cycle,
        xlab = "total sleep",
        ylab = "sleep cycle",
        title = "mammalian sleep dataset",
        caption = "source: ggplot2 package",
        type = "bayes",
        ggplot.component = ggplot2::scale_y_continuous(breaks = seq(0, 6000, 1000))
      )

    # subtitle
    set.seed(123)
    p_subtitle <-
      statsExpressions::corr_test(
        data = dplyr::filter(ggplot2::msleep, conservation == "lc"),
        x = sleep_total,
        y = sleep_cycle,
        type = "bayes"
      )$expression[[1]]

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
      "sleep cycle"
    )
    expect_identical(
      enframe(p$grobs[[22]]$children)$value[[1]][[1]]$expr,
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
      ggscatterstats(
        data = dplyr::filter(ggplot2::msleep, conservation == "lc"),
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
    expect_snapshot(list(pb$data[[1]], head(pb$data[[2]]), pb$data[[3]]))
    expect_snapshot(pb$plot$labels)

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
      ggscatterstats(
        data = dplyr::filter(ggplot2::msleep, conservation == "lc"),
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
      ggscatterstats(
        data = dplyr::filter(ggplot2::msleep, conservation == "lc"),
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
      ggscatterstats(
        data = dplyr::sample_frac(dplyr::filter(ggplot2::msleep, conservation == "lc"), 0.1),
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
    skip_if_not_installed("ggExtra")

    # creating the plot
    set.seed(123)
    p <-
      ggscatterstats(
        data = dplyr::filter(ggplot2::msleep, conservation == "lc"),
        x = sleep_total,
        y = sleep_cycle,
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
      ggscatterstats(
        data = dplyr::starwars,
        x = mass,
        y = height,
        conf.level = 0.90,
        type = "r",
        output = "subtitle"
      )

    fun_sub <-
      statsExpressions::corr_test(
        data = dplyr::starwars,
        x = mass,
        y = height,
        conf.level = 0.90,
        type = "r",
        output = "subtitle"
      )$expression[[1]]

    # checking captured messages
    expect_identical(p_sub, fun_sub)
  }
)
