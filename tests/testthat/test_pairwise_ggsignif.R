# context -------------------------------------------------------------------
context(desc = "pairwise_p with ggsignif")

# significant display works -------------------------------------------------

testthat::test_that(
  desc = "check comparison significant displays - adjusted",
  code = {

    # creating the plot
    set.seed(123)
    p1 <- ggstatsplot::ggbetweenstats(
      data = ggplot2::msleep,
      x = vore,
      y = brainwt,
      messages = FALSE,
      pairwise.comparisons = TRUE,
      caption = "mammalian sleep"
    )

    # checking dimensions of data
    data_dims <- dim(p1$plot_env$df_pairwise)

    testthat::expect_equal(data_dims[1], 0L)
    testthat::expect_equal(data_dims[2], 12L)

    # checking caption
    testthat::expect_identical(p1$labels$caption,
                               ggplot2::expr(atop(
                                 displaystyle("mammalian sleep"),
                                 expr = paste(
                                   "Pairwise comparisons: ",
                                   bold("Games-Howell test"),
                                   "; Adjustment (p-value): ",
                                   bold("Holm")
                                 )
                               )))
  }
)


# significant display works -------------------------------------------------

testthat::test_that(
  desc = "check non-significant comparison displays - no adjustment",
  code = {

    # creating the plot
    set.seed(123)
    p2 <- ggstatsplot::ggbetweenstats(
      data = ggstatsplot::movies_wide,
      x = mpaa,
      y = votes,
      messages = FALSE,
      pairwise.comparisons = TRUE,
      p.adjust.method = "none",
      pairwise.display = "ns",
      pairwise.annotation = "p.value"
    )

    # data used for pairwise comparisons
    dat <- p2$plot_env$df_pairwise

    # ggsignif layer parameters
    ggsignif_stat <- p2$layers[[6]]$stat_params

    # checking dimensions of data
    data_dims <- dim(dat)

    testthat::expect_equal(data_dims[1], 6L)
    testthat::expect_equal(data_dims[2], 12L)

    # checking comparison groups and labels
    testthat::expect_identical(dat$group1[1], "PG")
    testthat::expect_identical(dat$group2[1], "NC-17")
    testthat::expect_identical(dat$groups[[1]], c("PG", "NC-17"))
    testthat::expect_identical(dat$label[6], "p = 0.482")
    testthat::expect_identical(p2$labels$caption,
                               ggplot2::expr(atop(
                                 displaystyle(NULL),
                                 expr = paste(
                                   "Pairwise comparisons: ",
                                   bold("Games-Howell test"),
                                   "; Adjustment (p-value): ",
                                   bold("None")
                                 )
                               )))
    testthat::expect_identical(ggstatsplot::specify_decimal_p(
      x = dat$p.value[1],
      p.value = TRUE,
      k = 4
    ),
    "0.6400")

    # checking values
    testthat::expect_equal(dat$mean.difference[1], -2756.112, tolerance = 1e-3)
    testthat::expect_equal(dat$mean.difference[6], -3059.195, tolerance = 1e-3)

    # checking ggsignif layers
    testthat::expect_equal(ggsignif_stat$y_position[1], 161548.2, tolerance = 0.01)
    testthat::expect_equal(ggsignif_stat$y_position[6], 208829.1, tolerance = 0.01)

  }
)
