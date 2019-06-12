context(desc = "ggbarstats")

# contingency tab (with counts) ----------------------------------------------

testthat::test_that(
  desc = "checking labels with counts",
  code = {

    # condition variable is not options for ggbarstats
    testthat::expect_error(ggstatsplot::ggbarstats(
      data = as.data.frame(Titanic),
      main = Sex
    ))

    # plot
    set.seed(123)
    p <- ggstatsplot::ggbarstats(
      data = as.data.frame(Titanic),
      main = Sex,
      condition = Survived,
      counts = "Freq",
      perc.k = 2,
      conf.level = 0.95,
      xlab = "Passenger sex",
      ylab = "proportion",
      label.separator = "\n",
      bf.message = FALSE,
      messages = TRUE
    )

    # build plot
    pb <- ggplot2::ggplot_build(p)

    # subtitle
    set.seed(123)
    p_subtitle <- ggstatsplot::subtitle_contingency_tab(
      data = as.data.frame(Titanic),
      main = "Sex",
      condition = "Survived",
      counts = Freq,
      conf.level = 0.95,
      messages = FALSE
    )

    # checking data used to create a plot
    dat <- p$data %>%
      dplyr::mutate_if(
        .tbl = .,
        .predicate = is.factor,
        .funs = ~ as.character(.)
      )

    # checking dimensions of data
    data_dims <- dim(dat)

    # testing everything is okay with data
    testthat::expect_equal(data_dims, c(4L, 5L))
    testthat::expect_equal(dat$perc, c(8.46, 48.38, 91.54, 51.62), tolerance = 1e-3)
    testthat::expect_equal(dat$condition[1], "No")
    testthat::expect_equal(dat$condition[4], "Yes")
    testthat::expect_equal(dat$main[2], "Female")
    testthat::expect_equal(dat$main[3], "Male")
    testthat::expect_identical(dat$counts, c(126L, 344L, 1364L, 367L))

    # checking plot labels
    testthat::expect_identical(pb$plot$labels$subtitle, p_subtitle)
    testthat::expect_identical(pb$plot$labels$caption, NULL)
    testthat::expect_identical(pb$plot$labels$x, "Passenger sex")
    testthat::expect_identical(pb$plot$labels$y, "proportion")

    # checking data layers
    testthat::expect_equal(length(pb$data), 4L)
    testthat::expect_equal(unique(pb$data[[3]]$y), 1.05, tolerance = 0.01)
    testthat::expect_equal(unique(pb$data[[4]]$y), -0.05, tolerance = 0.01)
    testthat::expect_equal(pb$data[[2]]$y,
      c(0.4577181, 0.9577181, 0.2580872, 0.7580872),
      tolerance = 0.0001
    )
    testthat::expect_identical(
      pb$data[[2]]$label,
      c("91.54%", "8.46%", "51.62%", "48.38%")
    )
    testthat::expect_identical(pb$data[[3]]$label, c("***", "ns"))
    testthat::expect_identical(pb$data[[4]]$label, c("(n = 1490)", "(n = 711)"))

    # checking panel parameters
    testthat::expect_equal(pb$layout$panel_params[[1]]$x.range,
      c(0.4, 2.6),
      tolerance = 0.01
    )
    testthat::expect_identical(
      pb$layout$panel_params[[1]]$x.labels,
      c("No", "Yes")
    )
    testthat::expect_equal(pb$layout$panel_params[[1]]$x.major,
      c(0.2727273, 0.7272727),
      tolerance = 0.001
    )
    testthat::expect_equal(pb$layout$panel_params[[1]]$y.range,
      c(-0.105, 1.105),
      tolerance = 0.01
    )
    testthat::expect_identical(
      pb$layout$panel_params[[1]]$y.labels,
      c("0%", "10%", "20%", "30%", "40%", "50%", "60%", "70%", "80%", "90%", "100%")
    )
    testthat::expect_equal(pb$layout$panel_params[[1]]$y.minor_source,
      c(0.05, 0.15, 0.25, 0.35, 0.45, 0.55, 0.65, 0.75, 0.85, 0.95),
      tolerance = 0.01
    )
    testthat::expect_equal(pb$layout$panel_params[[1]]$y.major_source,
      c(0.0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1.0),
      tolerance = 0.01
    )
  }
)

# aesthetic modifications --------------------------------------------------

testthat::test_that(
  desc = "aesthetic modifications",
  code = {
    testthat::skip_on_cran()

    # plot
    set.seed(123)
    p <- suppressWarnings(ggstatsplot::ggbarstats(
      data = mtcars,
      main = vs,
      condition = "cyl",
      bf.message = TRUE,
      nboot = 10,
      data.label = "both",
      package = "wesanderson",
      palette = "Royal2",
      labels.legend = c("0 = V-shaped", "1 = straight"),
      legend.title = "Engine",
      x.axis.orientation = "slant",
      legend.position = "top",
      label.separator = "\n",
      messages = FALSE
    ))

    p1 <- suppressWarnings(ggstatsplot::ggbarstats(
      data = mtcars,
      main = vs,
      condition = cyl,
      bar.label = "counts",
      bf.message = FALSE,
      nboot = 10,
      x.axis.orientation = "vertical",
      messages = FALSE
    ))

    # build plot
    pb <- ggplot2::ggplot_build(p)
    pb1 <- ggplot2::ggplot_build(p1)

    # checking data used to create a plot
    dat <- p$data %>%
      dplyr::mutate_if(
        .tbl = .,
        .predicate = is.factor,
        .funs = ~ as.character(.)
      )

    # checking dimensions of data
    data_dims <- dim(dat)

    # testing everything is okay with data
    testthat::expect_equal(data_dims, c(5L, 5L))
    testthat::expect_identical(
      pb$data[[2]]$label,
      c(
        "n = 1\n(9%)",
        "n = 10\n(91%)",
        "n = 3\n(43%)",
        "n = 4\n(57%)",
        "n = 14\n(100%)"
      )
    )
    testthat::expect_identical(
      pb1$data[[2]]$label,
      c("n = 1", "n = 10", "n = 3", "n = 4", "n = 14")
    )

    # checking layered data
    testthat::expect_identical(
      pb$data[[1]]$fill,
      c("#F5CDB4", "#9A8822", "#F5CDB4", "#9A8822", "#F5CDB4")
    )
    testthat::expect_identical(pb$plot$guides$fill$title, "Engine")
    testthat::expect_equal(pb$plot$theme$axis.text.x$angle, 45L)
    testthat::expect_equal(pb1$plot$theme$axis.text.x$angle, 90L)
    testthat::expect_identical(pb$plot$theme$legend.position, "top")
    testthat::expect_identical(pb1$plot$theme$legend.position, "right")
  }
)

# subtitle return --------------------------------------------------

testthat::test_that(
  desc = "subtitle return",
  code = {
    testthat::skip_on_cran()

    # subtitle return
    set.seed(123)
    p_sub <- ggstatsplot::ggbarstats(
      data = dplyr::sample_frac(tbl = forcats::gss_cat, size = 0.1),
      main = race,
      condition = marital,
      return = "subtitle",
      k = 4,
      messages = FALSE
    )

    # caption return
    set.seed(123)
    p_cap <- ggstatsplot::ggbarstats(
      data = dplyr::sample_frac(tbl = forcats::gss_cat, size = 0.1),
      main = race,
      condition = marital,
      return = "caption",
      k = 4,
      messages = FALSE
    )

    # tests
    testthat::expect_identical(p_sub, ggplot2::expr(
      paste(
        NULL,
        italic(chi)^2,
        "(",
        "8",
        ") = ",
        "109.2007",
        ", ",
        italic("p"),
        " = ",
        "< 0.001",
        ", ",
        italic("V")["Cramer"],
        " = ",
        "0.1594",
        ", CI"["95%"],
        " [",
        "0.0916",
        ", ",
        "0.1278",
        "]",
        ", ",
        italic("n"),
        " = ",
        2148L
      )
    ))

    testthat::expect_identical(
      p_cap,
      ggplot2::expr(atop(
        displaystyle(NULL),
        expr = paste(
          "In favor of null: ",
          "log"["e"],
          "(BF"["01"],
          ") = ",
          "-36.8983",
          ", sampling = ",
          "independent multinomial",
          ", ",
          italic("a"),
          " = ",
          "1.0000"
        )
      ))
    )
  }
)
