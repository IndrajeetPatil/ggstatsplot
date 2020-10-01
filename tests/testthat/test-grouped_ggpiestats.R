testthat::test_that(
  desc = "grouped_ggpiestats works",
  code = {
    testthat::skip_on_cran()

    #--------------------- only main variable -------------------------------

    ## expecting error
    testthat::expect_error(
      ggstatsplot::grouped_ggpiestats(
        data = mtcars,
        main = cyl,
        messages = FALSE
      )
    )

    ## without counts

    # when arguments are entered as bare expressions
    set.seed(123)
    testthat::expect_true(inherits(suppressWarnings(
      ggstatsplot::grouped_ggpiestats(
        data = mtcars,
        grouping.var = am,
        main = "cyl",
        results.subtitle = FALSE,
        messages = FALSE
      )
    ),
    what = "gg"
    ))

    #------------------ both main and condition variables ------------------

    ## without counts

    # creating a smaller dataframe
    mpg_short <-
      ggplot2::mpg %>%
      dplyr::filter(
        .data = .,
        drv %in% c("4", "f"),
        class %in% c("suv", "midsize"),
        trans %in% c("auto(l4)", "auto(l5)")
      )

    # when arguments are entered as bare expressions
    set.seed(123)
    testthat::expect_true(inherits(suppressWarnings(
      ggstatsplot::grouped_ggpiestats(
        data = mpg_short,
        x = cyl,
        y = "class",
        results.subtitle = FALSE,
        grouping.var = drv,
        label.repel = TRUE,
        messages = FALSE
      )
    ),
    what = "gg"
    ))

    ## with counts

    # when arguments are entered as bare expressions
    set.seed(123)
    testthat::expect_true(inherits(suppressWarnings(
      ggstatsplot::grouped_ggpiestats(
        data = as.data.frame(Titanic),
        grouping.var = Class,
        main = Sex,
        results.subtitle = FALSE,
        condition = Survived,
        counts = "Freq",
        messages = FALSE
      )
    ),
    what = "gg"
    ))
  }
)

# subtitle output --------------------------------------------------

testthat::test_that(
  desc = "subtitle output",
  code = {
    testthat::skip_on_cran()

    # should output a list of length 3
    set.seed(123)
    ls_results <-
      suppressWarnings(ggstatsplot::grouped_ggpiestats(
        data = dplyr::sample_frac(tbl = forcats::gss_cat, size = 0.1),
        main = relig,
        condition = marital,
        grouping.var = race,
        output = "subtitle",
        messages = FALSE
      ))

    # tests
    testthat::expect_equal(
      ls_results,
      list(
        Other = ggplot2::expr(paste(
          NULL,
          chi["Pearson"]^2,
          "(",
          "40",
          ") = ",
          "40.27",
          ", ",
          italic("p"),
          " = ",
          "0.458",
          ", ",
          widehat(italic("V"))["Cramer"],
          " = ",
          "0.01",
          ", CI"["95%"],
          " [",
          "-0.30",
          ", ",
          "-0.02",
          "]",
          ", ",
          italic("n")["obs"],
          " = ",
          182L
        )),
        Black = ggplot2::expr(paste(
          NULL,
          chi["Pearson"]^
            2,
          "(",
          "32",
          ") = ",
          "25.11",
          ", ",
          italic("p"),
          " = ",
          "0.801",
          ", ",
          widehat(italic("V"))["Cramer"],
          " = ",
          "0.00",
          ", CI"["95%"],
          " [",
          "-0.17",
          ", ",
          "0.01",
          "]",
          ", ",
          italic("n")["obs"],
          " = ",
          317L
        )),
        White = ggplot2::expr(paste(
          NULL,
          chi["Pearson"]^
            2,
          "(",
          "52",
          ") = ",
          "109.65",
          ", ",
          italic("p"),
          " = ",
          "5.33e-06",
          ", ",
          widehat(italic("V"))["Cramer"],
          " = ",
          "0.09",
          ", CI"["95%"],
          " [",
          "0.03",
          ", ",
          "0.10",
          "]",
          ", ",
          italic("n")["obs"],
          " = ",
          1649L
        ))
      )
    )
  }
)
