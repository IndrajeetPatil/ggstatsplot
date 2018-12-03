# context ------------------------------------------------------------
context(desc = "ggpiestats")

# one sample proportion test -----------------------------------------

testthat::test_that(
  desc = "checking one sample proportion test",
  code = {
    # creating the plot
    set.seed(123)
    p <- ggstatsplot::ggpiestats(
      data = ggplot2::msleep,
      main = vore,
      bf.message = TRUE,
      title = "mammalian sleep",
      legend.title = "vorarephilia",
      caption = "From ggplot2 package",
      perc.k = 2,
      messages = FALSE
    )

    # checking data used to create a plot
    dat <- p$data

    # subtitle used
    set.seed(123)
    p_subtitle <-
      ggstatsplot::subtitle_onesample_proptest(
        data = ggplot2::msleep,
        main = vore
      )

    # checking dimensions of data
    data_dims <- dim(dat)

    # testing everything is okay with data
    testthat::expect_equal(data_dims[1], 4L)
    testthat::expect_equal(data_dims[2], 4L)
    testthat::expect_equal(dat$perc[1], 26.30, tolerance = 1e-3)
    testthat::expect_equal(dat$perc[2], 6.58, tolerance = 1e-3)
    testthat::expect_equal(dat$perc[3], 42.10, tolerance = 1e-3)
    testthat::expect_equal(dat$perc[4], 25.00, tolerance = 1e-3)

    # checking plot labels
    testthat::expect_identical(p$labels$subtitle, p_subtitle)
    testthat::expect_identical(p$labels$title, "mammalian sleep")
    testthat::expect_identical(p$labels$caption, "From ggplot2 package")
    testthat::expect_null(p$labels$x, NULL)
    testthat::expect_null(p$labels$y, NULL)
  }
)

# contingency tab ---------------------------------------------------------

testthat::test_that(
  desc = "checking labels with contingency tab",
  code = {
    # creating the plot
    set.seed(123)
    p <- suppressWarnings(ggstatsplot::ggpiestats(
      data = mtcars,
      main = "am",
      condition = "cyl",
      bf.message = TRUE,
      perc.k = 2,
      legend.title = "transmission",
      factor.levels = c("0 = automatic", "1 = manual"),
      facet.wrap.name = "cylinders",
      messages = FALSE
    ))

    # subtitle used
    set.seed(123)
    p_subtitle <-
      ggstatsplot::subtitle_contingency_tab(
        data = mtcars,
        main = "am",
        condition = "cyl",
        messages = FALSE
      )

    # extracting plot details
    pb <- ggplot2::ggplot_build(p)

    # checking data used to create a plot
    dat <- p$data

    # checking dimensions of data
    data_dims <- dim(dat)

    # testing everything is okay with data
    testthat::expect_equal(data_dims, c(6L, 5L))
    testthat::expect_equal(dat$perc[1], 72.73, tolerance = 1e-3)
    testthat::expect_equal(dat$perc[2], 42.86, tolerance = 1e-3)
    testthat::expect_equal(dat$perc[3], 14.29, tolerance = 1e-3)
    testthat::expect_equal(dat$perc[4], 27.27, tolerance = 1e-3)
    testthat::expect_equal(dat$perc[5], 57.14, tolerance = 1e-3)
    testthat::expect_equal(dat$perc[6], 85.71, tolerance = 1e-3)

    # checking plot labels
    testthat::expect_identical(p$labels$subtitle, p_subtitle)
    testthat::expect_identical(pb$plot$plot_env$facet.wrap.name, "cylinders")
    testthat::expect_identical(pb$plot$plot_env$legend.labels[1], "0 = automatic")
    testthat::expect_identical(pb$plot$plot_env$legend.labels[2], "1 = manual")
    testthat::expect_identical(pb$plot$labels$caption, ggplot2::expr(atop(
      displaystyle(NULL),
      expr = paste(
        "In favor of null: ",
        "log"["e"],
        "(BF"["01"],
        ") = ",
        "-2.82",
        ", sampling = ",
        "independent multinomial",
        ", ",
        italic("a"),
        " = ",
        "1.00"
      )
    )))
    testthat::expect_null(p$labels$x, NULL)
    testthat::expect_null(p$labels$y, NULL)
    testthat::expect_null(pb$plot$plot_env$stat.title, NULL)
    testthat::expect_identical(pb$plot$guides$fill$title[1], "transmission")
  }
)

# contingency tab (with counts) ----------------------------------------------

testthat::test_that(
  desc = "checking labels with counts",
  code = {

    # plot
    set.seed(123)
    p <- ggstatsplot::ggpiestats(
      data = as.data.frame(Titanic),
      main = Sex,
      condition = Survived,
      counts = Freq,
      perc.k = 2,
      conf.level = 0.95,
      messages = FALSE
    )

    # subtitle
    set.seed(123)
    p_subtitle <- ggstatsplot::subtitle_contingency_tab(
      data = as.data.frame(Titanic),
      main = Sex,
      condition = Survived,
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
    testthat::expect_equal(dat$perc[1], 8.46, tolerance = 1e-3)
    testthat::expect_equal(dat$perc[2], 48.38, tolerance = 1e-3)
    testthat::expect_equal(dat$perc[3], 91.54, tolerance = 1e-3)
    testthat::expect_equal(dat$perc[4], 51.62, tolerance = 1e-3)
    testthat::expect_equal(dat$condition[1], "No")
    testthat::expect_equal(dat$condition[4], "Yes")
    testthat::expect_equal(dat$main[2], "Female")
    testthat::expect_equal(dat$main[3], "Male")
    testthat::expect_identical(dat$counts[1], 126L)
    testthat::expect_identical(dat$counts[2], 344L)
    testthat::expect_identical(dat$counts[3], 1364L)
    testthat::expect_identical(dat$counts[4], 367L)

    # checking plot labels
    testthat::expect_identical(p$labels$subtitle, p_subtitle)
  }
)


# mcnemar test ---------------------------------------------------------

testthat::test_that(
  desc = "checking labels with contingency tab (paired)",
  code = {
    # data
    set.seed(123)
    survey.data <- data.frame(
      `1st survey` = c("Approve", "Approve", "Disapprove", "Disapprove"),
      `2nd survey` = c("Approve", "Disapprove", "Approve", "Disapprove"),
      `Counts` = c(794, 150, 86, 570),
      check.names = FALSE
    )

    # plot
    set.seed(123)
    p <- ggstatsplot::ggpiestats(
      data = survey.data,
      main = `1st survey`,
      condition = `2nd survey`,
      counts = Counts,
      paired = TRUE,
      conf.level = 0.90,
      messages = FALSE
    )

    # subtitle
    set.seed(123)
    p_subtitle <- ggstatsplot::subtitle_contingency_tab(
      data = survey.data,
      main = `1st survey`,
      condition = `2nd survey`,
      counts = Counts,
      paired = TRUE,
      conf.level = 0.90,
      messages = FALSE
    )

    # checking plot labels
    testthat::expect_identical(p$labels$subtitle, p_subtitle)
  }
)
