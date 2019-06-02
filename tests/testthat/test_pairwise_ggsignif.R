context(desc = "pairwise_p with ggsignif")

# significant display works -------------------------------------------------

testthat::test_that(
  desc = "check comparison significant displays - adjusted",
  code = {

    # creating the plot
    set.seed(123)
    p <- ggstatsplot::ggbetweenstats(
      data = ggplot2::msleep,
      x = vore,
      y = brainwt,
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
      p$labels$caption,
      ggplot2::expr(atop(
        displaystyle("mammalian sleep"),
        expr = paste(
          "Pairwise comparisons: ",
          bold("Games-Howell test"),
          "; Adjustment (p-value): ",
          bold("Holm")
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
    p <- ggstatsplot::ggbetweenstats(
      data = ggstatsplot::movies_wide,
      x = mpaa,
      y = votes,
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

    # data used for pairwise comparisons
    dat <- pb$plot$plot_env$df_pairwise

    # ggsignif layer parameters
    ggsignif_stat <- pb$plot$layers[[7]]$stat_params

    # checking dimensions of data
    data_dims <- dim(dat)

    # data for geom_signif layer
    data_signif <- tibble::as_tibble(pb$data[[7]])


    testthat::expect_equal(length(pb$data), 7L)
    testthat::expect_identical(
      as.character(unique(data_signif$annotation)),
      c("p = 0.079", "p = 0.139", "p = 0.825")
    )
    testthat::expect_equal(data_dims, c(3L, 9L))

    # checking comparison groups and labels
    testthat::expect_identical(dat$group1, c("PG-13", "PG-13", "R"))
    testthat::expect_identical(dat$group2, c("R", "PG", "PG"))
    testthat::expect_identical(dat$p.value.label, c("p = 0.079", "p = 0.139", "p = 0.825"))
    testthat::expect_identical(
      p$labels$caption,
      ggplot2::expr(atop(
        displaystyle(NULL),
        expr = paste(
          "Pairwise comparisons: ",
          bold("Games-Howell test"),
          "; Adjustment (p-value): ",
          bold("None")
        )
      ))
    )
    testthat::expect_identical(
      ggstatsplot::specify_decimal_p(
        x = dat$p.value[1],
        p.value = TRUE,
        k = 4
      ),
      "0.0790"
    )

    # checking values
    testthat::expect_equal(dat$mean.difference,
      c(-1904.886, -2707.920, -803.034),
      tolerance = 1e-3
    )

    # checking ggsignif layers
    testthat::expect_equal(ggsignif_stat$y_position,
      c(161548.2, 173368.2, 185188.2),
      tolerance = 0.01
    )
    testthat::expect_equal(ggsignif_stat$comparisons[[2]], c("PG-13", "PG"))
    testthat::expect_equal(
      ggsignif_stat$annotations,
      c("p = 0.079", "p = 0.139", "p = 0.825")
    )
  }
)

# mixed display works -------------------------------------------------

testthat::test_that(
  desc = "check mixed comparison displays - adjusted",
  code = {
    testthat::skip_on_cran()

    # creating the plot
    set.seed(123)
    p <- ggstatsplot::ggbetweenstats(
      data = dplyr::filter(
        ggstatsplot::movies_long,
        genre %in% c("Action", "Comedy", "RomCom")
      ),
      x = genre,
      y = rating,
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
    data_signif <- tibble::as_tibble(pb$data[[7]])

    # checking comparison groups and labels
    testthat::expect_identical(dat$group1, c("Action", "Action", "Comedy"))
    testthat::expect_identical(dat$group2, c("Comedy", "RomCom", "RomCom"))
    testthat::expect_identical(dat$significance, c("ns", "***", "***"))
    testthat::expect_identical(
      dat$p.value.label,
      c("p = 0.687", "p = 0.001", "p <= 0.001")
    )
    testthat::expect_identical(
      p$labels$caption,
      ggplot2::expr(atop(
        displaystyle(NULL),
        expr = paste(
          "Pairwise comparisons: ",
          bold("Dwass-Steel-Crichtlow-Fligner test"),
          "; Adjustment (p-value): ",
          bold("Benjamini & Hochberg")
        )
      ))
    )
    testthat::expect_identical(
      ggstatsplot::specify_decimal_p(
        x = dat$p.value[1],
        p.value = TRUE,
        k = 4
      ),
      "0.6870"
    )

    # checking values
    testthat::expect_equal(dat$W,
      c(0.5702297, 4.8458729, 5.4434648),
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
    testthat::expect_equal(ggsignif_stat$annotations, dat$significance)
  }
)


# robust test works -------------------------------------------------

testthat::test_that(
  desc = "check robust test display - adjusted",
  code = {
    testthat::skip_on_cran()

    # creating the plot
    set.seed(123)
    p <- ggstatsplot::ggbetweenstats(
      data = ggplot2::mpg,
      x = drv,
      y = cty,
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
    data_signif <- tibble::as_tibble(pb$data[[7]])

    # checking comparison groups and labels
    testthat::expect_equal(data_dims, c(3L, 8L))
    testthat::expect_identical(dat$group1, c("4", "4", "f"))
    testthat::expect_identical(dat$group2, c("f", "r", "r"))
    testthat::expect_identical(dat$significance, c("***", "ns", "***"))
    testthat::expect_identical(
      p$labels$caption,
      ggplot2::expr(atop(
        displaystyle(NULL),
        expr = paste(
          "Pairwise comparisons: ",
          bold("Yuen's trimmed means test"),
          "; Adjustment (p-value): ",
          bold("Holm")
        )
      ))
    )

    # checking values
    testthat::expect_equal(dat$psihat,
      c(-5.3769964, 0.1927711, 5.5697674),
      tolerance = 0.001
    )

    # checking ggsignif layers
    testthat::expect_equal(ggsignif_stat$y_position,
      c(35.875, 37.825, 39.775),
      tolerance = 0.001
    )
    testthat::expect_equal(ggsignif_stat$comparisons[[2]], c("f", "r"))
    testthat::expect_equal(
      ggsignif_stat$annotations,
      c(
        "p <= 0.001",
        "p <= 0.001"
      )
    )
  }
)

# student's t test works -------------------------------------------------

testthat::test_that(
  desc = "check student's t test display - adjusted",
  code = {
    testthat::skip_on_cran()

    # creating the plot
    set.seed(123)
    p <- ggstatsplot::ggbetweenstats(
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
    data_signif <- tibble::as_tibble(pb$data[[7]])

    # checking comparison groups and labels
    testthat::expect_equal(data_dims, c(3L, 6L))
    testthat::expect_identical(dat$group1, c("6", "8", "8"))
    testthat::expect_identical(dat$group2, c("4", "4", "6"))
    testthat::expect_identical(dat$significance, c("*", "***", "*"))
    testthat::expect_identical(
      p$labels$caption,
      ggplot2::expr(atop(
        displaystyle(NULL),
        expr = paste(
          "Pairwise comparisons: ",
          bold("Student's t-test"),
          "; Adjustment (p-value): ",
          bold("Bonferroni")
        )
      ))
    )

    # checking values
    testthat::expect_equal(dat$mean.difference,
      c(0.8314156, 1.7134870, 0.8820714),
      tolerance = 0.001
    )

    # checking ggsignif layers
    testthat::expect_equal(ggsignif_stat$y_position,
      c(5.559600, 5.852925, 6.146250),
      tolerance = 0.001
    )
    testthat::expect_equal(ggsignif_stat$comparisons[[2]], c("8", "4"))
    testthat::expect_equal(
      ggsignif_stat$annotations,
      c(
        "p = 0.032",
        "p <= 0.001",
        "p = 0.015"
      )
    )

    # geom_signif data layers
    testthat::expect_equal(dim(data_signif), c(9L, 19L))
    testthat::expect_identical(
      as.character(data_signif$group),
      c(
        "6-4-1",
        "6-4-1",
        "6-4-1",
        "8-4-2",
        "8-4-2",
        "8-4-2",
        "8-6-3",
        "8-6-3",
        "8-6-3"
      )
    )
  }
)
