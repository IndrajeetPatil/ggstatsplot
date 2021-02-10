# significant display works -------------------------------------------------

test_that(
  desc = "check comparison significant displays - FDR-corrected",
  code = {
    skip_on_cran()

    # creating the plot
    set.seed(123)
    p <-
      ggstatsplot::ggbetweenstats(
        data = ggplot2::msleep,
        x = vore,
        y = brainwt,
        results.subtitle = FALSE,
        bf.message = FALSE,
        pairwise.display = "s",
        caption = "mammalian sleep",
        k = 3
      )

    # build the plot
    pb <- ggplot2::ggplot_build(p)

    # check data
    set.seed(123)
    expect_snapshot(pb$data[[6]])

    # checking caption
    expect_identical(
      pb$plot$labels$caption,
      ggplot2::expr(atop(
        displaystyle("mammalian sleep"),
        expr = paste(
          "Pairwise test: ",
          bold("Games-Howell test"),
          "; Comparisons shown: ",
          bold("only significant")
        )
      ))
    )
  }
)


# non-significant display works ---------------------------------------------

test_that(
  desc = "check non-significant comparison displays - no adjustment",
  code = {
    skip_on_cran()

    # creating the plot
    set.seed(123)
    p <-
      ggstatsplot::ggbetweenstats(
        data = ggstatsplot::movies_wide,
        x = mpaa,
        y = votes,
        results.subtitle = FALSE,
        bf.message = FALSE,
        p.adjust.method = "none",
        pairwise.display = "ns",
        pairwise.annotation = "p.value",
        k = 3
      )

    # build the plot
    pb <- ggplot2::ggplot_build(p)

    # check data
    set.seed(123)
    expect_snapshot(pb$data[[6]])

    # checking caption
    expect_identical(
      pb$plot$labels$caption,
      ggplot2::expr(atop(
        displaystyle(NULL),
        expr = paste(
          "Pairwise test: ",
          bold("Games-Howell test"),
          "; Comparisons shown: ",
          bold("only non-significant")
        )
      ))
    )
  }
)

# mixed display works -------------------------------------------------

test_that(
  desc = "check mixed comparison displays - FDR-corrected",
  code = {
    skip_on_cran()

    # creating the plot
    set.seed(123)
    p <-
      ggstatsplot::ggbetweenstats(
        data = dplyr::filter(
          ggstatsplot::movies_long,
          genre %in% c("Action", "Comedy", "RomCom")
        ),
        x = genre,
        y = rating,
        results.subtitle = FALSE,
        type = "np",
        bf.message = FALSE,
        p.adjust.method = "fdr",
        pairwise.display = "all",
        k = 3,
        palette = "Set3"
      )

    # build the plot
    pb <- ggplot2::ggplot_build(p)

    # check data
    set.seed(123)
    expect_snapshot(pb$data[[6]])

    # data used for pairwise comparisons
    dat <- pb$plot$plot_env$df_pairwise

    # ggsignif layer parameters
    ggsignif_stat <- pb$plot$layers[[7]]$stat_params

    # checking dimensions of data
    data_dims <- dim(dat)

    # data for geom_signif layer
    data_signif <- as_tibble(pb$data[[7]])

    # checking comparison groups and labels
    expect_identical(
      pb$plot$labels$caption$expr,
      ggplot2::expr(paste(
        "Pairwise test: ",
        bold("Dunn test"),
        "; Comparisons shown: ",
        bold("all")
      ))
    )

    # checking values
    expect_equal(dat$statistic,
      c(0.238306686320387, 3.63442246865882, 3.69050171235682),
      tolerance = 0.001
    )

    # checking ggsignif layers
    expect_equal(ggsignif_stat$y_position,
      c(9.2250, 9.7725, 10.3200),
      tolerance = 0.001
    )
    expect_equal(
      ggsignif_stat$comparisons[[1]],
      c("Action", "Comedy")
    )
    expect_equal(ggsignif_stat$annotations, dat$label)
  }
)

# robust test works -------------------------------------------------

test_that(
  desc = "check robust test display - FDR-corrected",
  code = {
    skip_on_cran()

    # creating the plot
    set.seed(123)
    p <-
      ggstatsplot::ggbetweenstats(
        data = ggplot2::mpg,
        x = drv,
        y = cty,
        results.subtitle = FALSE,
        bf.message = FALSE,
        k = 3,
        type = "r",
        nboot = 20,
        pairwise.display = "s",
        pairwise.annotation = "p.value"
      )

    # build the plot
    pb <- ggplot2::ggplot_build(p)

    # check data
    set.seed(123)
    expect_snapshot(pb$data[[6]])

    # data used for pairwise comparisons
    dat <- pb$plot$plot_env$df_pairwise

    # ggsignif layer parameters
    ggsignif_stat <- pb$plot$layers[[7]]$stat_params

    # checking dimensions of data
    data_dims <- dim(dat)

    # data for geom_signif layer
    data_signif <- as_tibble(pb$data[[7]])

    # checking comparison groups and labels
    expect_identical(
      pb$plot$labels$caption,
      ggplot2::expr(atop(
        displaystyle(NULL),
        expr = paste(
          "Pairwise test: ",
          bold("Yuen's trimmed means test"),
          "; Comparisons shown: ",
          bold("only significant")
        )
      ))
    )

    # checking ggsignif layers
    expect_equal(
      ggsignif_stat$y_position,
      c(35.875, 37.825, 39.775),
      tolerance = 0.001
    )
    expect_equal(ggsignif_stat$comparisons[[2]], c("f", "r"))
  }
)

# student's t test works -------------------------------------------------

test_that(
  desc = "check student's t test display - FDR-corrected",
  code = {
    skip_on_cran()

    # creating the plot
    set.seed(123)
    p <-
      ggstatsplot::ggbetweenstats(
        data = mtcars,
        x = cyl,
        y = wt,
        results.subtitle = FALSE,
        bf.message = FALSE,
        k = 3,
        type = "p",
        p.adjust.method = "bonferroni",
        var.equal = TRUE,
        pairwise.display = "everything",
        pairwise.annotation = "p"
      )

    # build the plot
    pb <- ggplot2::ggplot_build(p)

    # check data
    set.seed(123)
    expect_snapshot(pb$data[[6]])

    # data used for pairwise comparisons
    dat <- pb$plot$plot_env$df_pairwise

    # ggsignif layer parameters
    ggsignif_stat <- pb$plot$layers[[7]]$stat_params

    # checking dimensions of data
    data_dims <- dim(dat)

    # data for geom_signif layer
    data_signif <- as_tibble(pb$data[[7]])

    # checking comparison groups and labels)
    expect_identical(
      pb$plot$labels$caption,
      ggplot2::expr(atop(
        displaystyle(NULL),
        expr = paste(
          "Pairwise test: ",
          bold("Student's t-test"),
          "; Comparisons shown: ",
          bold("all")
        )
      ))
    )
  }
)
