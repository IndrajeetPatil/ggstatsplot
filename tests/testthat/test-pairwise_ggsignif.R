# significant display works -------------------------------------------------

testthat::test_that(
  desc = "check comparison significant displays - FDR-corrected",
  code = {
    testthat::skip_on_cran()

    # creating the plot
    set.seed(123)
    p <-
      ggstatsplot::ggbetweenstats(
        data = ggplot2::msleep,
        x = vore,
        y = brainwt,
        results.subtitle = FALSE,
        bf.message = FALSE,
        messages = FALSE,
        pairwise.comparisons = TRUE,
        pairwise.display = "s",
        caption = "mammalian sleep",
        k = 3
      )

    # build the plot
    pb <- ggplot2::ggplot_build(p)

    # checking dimensions of data
    testthat::expect_equal(length(pb$data), 6L)

    # checking caption
    testthat::expect_identical(
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

testthat::test_that(
  desc = "check non-significant comparison displays - no adjustment",
  code = {
    testthat::skip_on_cran()

    # creating the plot
    set.seed(123)
    p <-
      ggstatsplot::ggbetweenstats(
        data = ggstatsplot::movies_wide,
        x = mpaa,
        y = votes,
        results.subtitle = FALSE,
        bf.message = FALSE,
        messages = FALSE,
        pairwise.comparisons = TRUE,
        p.adjust.method = "none",
        pairwise.display = "ns",
        pairwise.annotation = "p.value",
        k = 3
      )

    # build the plot
    pb <- ggplot2::ggplot_build(p)

    # checking comparison groups and labels
    testthat::expect_equal(
      unique(as.character(pb$data[[7]]$annotation)),
      c(
        "list(~italic(p)[uncorrected]==0.139)",
        "list(~italic(p)[uncorrected]==0.825)",
        "list(~italic(p)[uncorrected]==0.079)"
      )
    )

    testthat::expect_equal(
      unique(as.character(pb$data[[7]]$group)),
      c("PG-PG-13-1", "PG-R-2", "PG-13-R-3")
    )

    # checking caption
    testthat::expect_identical(
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

testthat::test_that(
  desc = "check mixed comparison displays - FDR-corrected",
  code = {
    testthat::skip_on_cran()

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
        messages = FALSE,
        bf.message = FALSE,
        pairwise.comparisons = TRUE,
        p.adjust.method = "fdr",
        pairwise.display = "all",
        k = 3,
        palette = "Set3"
      )

    # build the plot
    pb <- ggplot2::ggplot_build(p)

    # data used for pairwise comparisons
    dat <- pb$plot$plot_env$df_pairwise

    # ggsignif layer parameters
    ggsignif_stat <- pb$plot$layers[[7]]$stat_params

    # checking dimensions of data
    data_dims <- dim(dat)

    # data for geom_signif layer
    data_signif <- as_tibble(pb$data[[7]])

    # checking comparison groups and labels
    testthat::expect_identical(dat$group1, c("Action", "Action", "Comedy"))
    testthat::expect_identical(dat$group2, c("Comedy", "RomCom", "RomCom"))
    testthat::expect_identical(dat$significance, c("ns", "***", "***"))
    testthat::expect_identical(
      dat$label,
      c(
        "list(~italic(p)[FDR-corrected]==0.812)",
        "list(~italic(p)[FDR-corrected]==4.18e-04)",
        "list(~italic(p)[FDR-corrected]==4.18e-04)"
      )
    )
    testthat::expect_identical(
      pb$plot$labels$caption$expr,
      ggplot2::expr(paste(
        "Pairwise test: ",
        bold("Dunn test"),
        "; Comparisons shown: ",
        bold("all")
      ))
    )
    testthat::expect_identical(
      ggstatsplot::specify_decimal_p(
        x = dat$p.value[1],
        p.value = TRUE,
        k = 4
      ),
      "0.8116"
    )

    # checking values
    testthat::expect_equal(dat$statistic,
      c(0.238306686320387, 3.63442246865882, 3.69050171235682),
      tolerance = 0.001
    )

    # checking ggsignif layers
    testthat::expect_equal(ggsignif_stat$y_position,
      c(9.2250, 9.7725, 10.3200),
      tolerance = 0.001
    )
    testthat::expect_equal(
      ggsignif_stat$comparisons[[1]],
      c("Action", "Comedy")
    )
    testthat::expect_equal(ggsignif_stat$annotations, dat$label)
  }
)

# robust test works -------------------------------------------------

testthat::test_that(
  desc = "check robust test display - FDR-corrected",
  code = {
    testthat::skip_on_cran()

    # creating the plot
    set.seed(123)
    p <-
      ggstatsplot::ggbetweenstats(
        data = ggplot2::mpg,
        x = drv,
        y = cty,
        results.subtitle = FALSE,
        bf.message = FALSE,
        messages = FALSE,
        k = 3,
        type = "r",
        nboot = 20,
        pairwise.comparisons = TRUE,
        pairwise.display = "s",
        pairwise.annotation = "p.value"
      )

    # build the plot
    pb <- ggplot2::ggplot_build(p)

    # data used for pairwise comparisons
    dat <- pb$plot$plot_env$df_pairwise

    # ggsignif layer parameters
    ggsignif_stat <- pb$plot$layers[[7]]$stat_params

    # checking dimensions of data
    data_dims <- dim(dat)

    # data for geom_signif layer
    data_signif <- as_tibble(pb$data[[7]])

    # checking comparison groups and labels
    testthat::expect_identical(dat$group1, c("4", "4", "f"))
    testthat::expect_identical(dat$group2, c("f", "r", "r"))
    testthat::expect_identical(dat$significance, c("***", "ns", "***"))
    testthat::expect_identical(
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

    # checking values
    testthat::expect_equal(dat$estimate,
      c(-5.3769964, 0.1927711, 5.5697674),
      tolerance = 0.001
    )

    # checking ggsignif layers
    testthat::expect_equal(
      ggsignif_stat$y_position,
      c(35.875, 37.825, 39.775),
      tolerance = 0.001
    )
    testthat::expect_equal(ggsignif_stat$comparisons[[2]], c("f", "r"))
    testthat::expect_equal(
      ggsignif_stat$annotations,
      c(
        "list(~italic(p)[Holm-corrected]==0e+00)",
        "list(~italic(p)[Holm-corrected]==7.1e-11)"
      )
    )
  }
)

# student's t test works -------------------------------------------------

testthat::test_that(
  desc = "check student's t test display - FDR-corrected",
  code = {
    testthat::skip_on_cran()

    # creating the plot
    set.seed(123)
    p <-
      ggstatsplot::ggbetweenstats(
        data = mtcars,
        x = cyl,
        y = wt,
        bf.message = FALSE,
        messages = FALSE,
        k = 3,
        type = "p",
        p.adjust.method = "bonferroni",
        nboot = 50,
        var.equal = TRUE,
        pairwise.comparisons = TRUE,
        pairwise.display = "everything",
        pairwise.annotation = "p"
      )

    # build the plot
    pb <- ggplot2::ggplot_build(p)

    # data used for pairwise comparisons
    dat <- pb$plot$plot_env$df_pairwise

    # ggsignif layer parameters
    ggsignif_stat <- pb$plot$layers[[7]]$stat_params

    # checking dimensions of data
    data_dims <- dim(dat)

    # data for geom_signif layer
    data_signif <- as_tibble(pb$data[[7]])

    # checking comparison groups and labels)
    testthat::expect_identical(dat$group1, c("4", "4", "6"))
    testthat::expect_identical(dat$group2, c("6", "8", "8"))
    testthat::expect_identical(dat$significance, c("*", "***", "*"))
    testthat::expect_identical(
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

    # checking values
    # checking ggsignif layers
    testthat::expect_equal(ggsignif_stat$y_position,
      c(5.559600, 5.852925, 6.146250),
      tolerance = 0.001
    )
    testthat::expect_equal(ggsignif_stat$comparisons[[2]], c("4", "8"))
    testthat::expect_equal(
      ggsignif_stat$annotations,
      c(
        "list(~italic(p)[Bonferroni-corrected]==0.032)",
        "list(~italic(p)[Bonferroni-corrected]==6.21e-07)",
        "list(~italic(p)[Bonferroni-corrected]==0.015)"
      )
    )

    # geom_signif data layers
    testthat::expect_equal(dim(data_signif), c(9L, 19L))
    testthat::expect_identical(
      as.character(data_signif$group),
      c(
        "4-6-1",
        "4-6-1",
        "4-6-1",
        "4-8-2",
        "4-8-2",
        "4-8-2",
        "6-8-3",
        "6-8-3",
        "6-8-3"
      )
    )
  }
)
