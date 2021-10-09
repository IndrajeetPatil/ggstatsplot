# checking labels and data from plot -------------------------------------

test_that(
  desc = "checking labels and data from plot",
  code = {
    skip_on_cran()
    skip_if(getRversion() < "4.0")

    # creating the plot
    set.seed(123)
    p <- ggbetweenstats(
      data = ggplot2::msleep,
      x = vore,
      y = brainwt,
      title = "mammalian sleep",
      caption = "From ggplot2 package",
      xlab = "vore",
      ylab = "brain weight",
      ggsignif.args = list(textsize = 6, tip_length = 0.01),
      outlier.tagging = TRUE,
      outlier.label = name,
      outlier.label.args = list(color = "darkgreen"),
      conf.level = 0.99,
      k = 5
    )

    # subtitle
    set.seed(123)
    p_subtitle <- statsExpressions::oneway_anova(
      data = ggplot2::msleep,
      x = vore,
      y = brainwt,
      k = 5,
      conf.level = 0.99
    )$expression[[1]]

    # plot build
    pb <- ggplot2::ggplot_build(p)

    # check data
    set.seed(123)
    expect_snapshot(list(pb$data[[1]], pb$data[[2]], pb$data[[4]], pb$data[[5]]))
    expect_snapshot(within(pb$plot$labels, rm(subtitle, caption)))

    # checking x-axis sample size labels
    expect_identical(
      ggplot2::layer_scales(p)$x$labels,
      c(
        "carni\n(n = 9)",
        "herbi\n(n = 20)",
        "insecti\n(n = 5)",
        "omni\n(n = 17)"
      )
    )

    expect_identical(pb$plot$labels$subtitle, p_subtitle)
  }
)

# mean labelling tests work ------------------------------------------

test_that(
  desc = "checking mean labels are working",
  code = {
    skip_on_cran()
    skip_if(getRversion() < "4.0")

    # creating the plot
    set.seed(123)
    p <- ggbetweenstats(
      data = tibble::as_tibble(mtcars, rownames = "name") %>%
        dplyr::rename(n = wt),
      x = cyl,
      y = n,
      type = "np",
      k = 2L,
      pairwise.comparisons = FALSE,
      conf.level = 0.90,
      outlier.tagging = TRUE,
      outlier.label = "name",
      outlier.coef = 2.5,
      results.subtitle = FALSE
    ) +
      ggplot2::coord_cartesian(ylim = c(1, 6)) +
      ggplot2::scale_y_continuous(limits = c(1, 6), breaks = seq(1, 6, 1))

    # plot build
    pb <- ggplot2::ggplot_build(p)

    # check data
    set.seed(123)
    expect_snapshot(list(pb$data[[1]], pb$data[[2]], pb$data[[4]], pb$data[[5]]))
    expect_snapshot(within(pb$plot$labels, rm(caption)))

    # check if the y-axis labels have changed
    expect_identical(
      pb$layout$panel_params[[1]]$x$scale$labels,
      c("4\n(n = 11)", "6\n(n = 7)", "8\n(n = 14)")
    )

    expect_identical(
      pb$layout$panel_params[[1]]$y$breaks,
      c(1, 2, 3, 4, 5, 6)
    )

    # edge case
    a <- data.frame(
      centrality.a = c(1.1, 0.9, 0.94, 1.58, 1.2, 1.4),
      group = c("a", "a", "a", "b", "b", "b")
    )

    # plot
    p1 <- suppressWarnings(ggbetweenstats(
      data = a,
      x = group,
      y = centrality.a,
      results.subtitle = FALSE
    ))

    # build
    pb1 <- ggplot2::ggplot_build(p1)

    expect_identical(
      pb1$data[[6]]$label,
      c(
        "list(~widehat(mu)[mean]=='0.98')",
        "list(~widehat(mu)[mean]=='1.39')"
      )
    )
  }
)

# checking if plot.type argument works --------------------------------------

test_that(
  desc = "checking if plot.type argument works",
  code = {
    skip_on_cran()
    skip_if(getRversion() < "4.0")

    # boxplot
    set.seed(123)
    p1 <- ggbetweenstats(
      data = ToothGrowth,
      x = supp,
      y = len,
      type = "bayes",
      plot.type = "box",
      results.subtitle = FALSE,
      outlier.tagging = TRUE,
      outlier.coef = 0.75,
      outlier.color = "blue",
      centrality.point.args = list(size = 5, color = "darkgreen"),
      centrality.label.args = list(color = "blue", nudge_x = 0.4, segment.linetype = 4)
    )

    # violin
    set.seed(123)
    p2 <- ggbetweenstats(
      data = ToothGrowth,
      x = supp,
      y = len,
      type = "r",
      results.subtitle = FALSE,
      plot.type = "violin",
      outlier.tagging = TRUE,
      outlier.coef = 0.75,
      bf.message = FALSE,
      package = "wesanderson",
      palette = "Royal1"
    ) +
      ggplot2::scale_y_continuous(breaks = seq(0, 30, 5))

    # build the plots
    pb1 <- ggplot2::ggplot_build(p1)
    pb2 <- ggplot2::ggplot_build(p2)

    # check data
    expect_snapshot(list(
      pb1$data,
      list(pb2$data[[1]], pb2$data[[2]], pb2$data[[4]], pb2$data[[5]])
    ))

    expect_snapshot(within(pb1$plot$labels, rm(subtitle, caption)))
    expect_snapshot(within(pb2$plot$labels, rm(subtitle, caption)))
  }
)

# subtitle output works ------------------------------------------------

test_that(
  desc = "subtitle output works",
  code = {
    skip_on_cran()
    skip_if(getRversion() < "4.0")

    df <- mtcars
    df$wt[3] <- NA

    # plot
    set.seed(123)
    subtitle_exp <- ggbetweenstats(
      data = df,
      x = am,
      y = wt,
      output = "subtitle"
    )

    set.seed(123)
    sub <- statsExpressions::two_sample_test(
      data = df,
      x = am,
      y = wt
    )$expression[[1]]

    # test
    expect_identical(subtitle_exp, sub)
  }
)
