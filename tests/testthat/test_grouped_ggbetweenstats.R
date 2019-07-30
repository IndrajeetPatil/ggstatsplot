# context -----------------------------------------------------------------
context(desc = "grouped_ggbetweenstats")

# outlier labeling works --------------------------------------------------

testthat::test_that(
  desc = "grouping.var works across vector types",
  code = {
    testthat::skip_on_cran()

    # creating a smaller dataframe
    set.seed(123)
    dat <- dplyr::sample_frac(tbl = ggstatsplot::movies_long, size = 0.25) %>%
      dplyr::filter(
        .data = ., mpaa %in% c("R", "PG-13"),
        genre %in% c("Drama", "Comedy")
      )

    # expect error when no grouping.var is specified
    testthat::expect_error(
      ggstatsplot::grouped_ggbetweenstats(
        data = dat,
        x = genre,
        y = rating
      )
    )

    # expect error when x and grouping.var are same
    testthat::expect_output(
      ggstatsplot::grouped_ggbetweenstats(
        data = dat,
        x = genre,
        y = rating,
        grouping.var = genre
      )
    )

    # outlier tagging is not required
    ggstatsplot::grouped_ggbetweenstats(
      data = dat,
      x = genre,
      y = rating,
      grouping.var = mpaa,
      outlier.tagging = FALSE,
      messages = TRUE
    )

    # `outlier.label` is not specified
    set.seed(123)
    testthat::expect_true(inherits(
      ggstatsplot::grouped_ggbetweenstats(
        data = dat,
        x = genre,
        y = "rating",
        grouping.var = mpaa,
        type = "p",
        plot.type = "box",
        bf.message = TRUE,
        outlier.tagging = TRUE,
        pairwise.comparisons = TRUE,
        pairwise.annotation = "p.value",
        messages = TRUE
      ),
      what = "gg"
    ))

    # `outlier.label` is factor
    set.seed(123)
    testthat::expect_true(inherits(
      ggstatsplot::grouped_ggbetweenstats(
        data = dat,
        x = "genre",
        y = rating,
        grouping.var = "mpaa",
        type = "np",
        plot.type = "violin",
        pairwise.comparisons = TRUE,
        messages = FALSE,
        outlier.tagging = TRUE,
        outlier.label = title
      ),
      what = "gg"
    ))


    # `outlier.label` is character
    # also x, y, and outlier.label arguments as characters
    set.seed(123)
    dat$title <- as.character(dat$title)

    testthat::expect_true(inherits(
      ggstatsplot::grouped_ggbetweenstats(
        data = dat,
        x = "genre",
        y = "rating",
        grouping.var = mpaa,
        messages = FALSE,
        type = "r",
        pairwise.comparisons = TRUE,
        outlier.tagging = TRUE,
        outlier.label = "title",
        outlier.coef = 5
      ),
      what = "gg"
    ))
  }
)

# outlier labeling works --------------------------------------------------

testthat::test_that(
  desc = "grouping.var works across vector types",
  code = {
    testthat::skip_on_cran()

    testthat::expect_true(inherits(
      ggstatsplot::grouped_ggbetweenstats(
        data = dplyr::filter(
          ggstatsplot::movies_long,
          genre %in% c("Action", "Comedy"),
          mpaa %in% c("R", "PG")
        ),
        x = genre,
        y = rating,
        grouping.var = mpaa,
        results.subtitle = FALSE,
        ggplot.component = ggplot2::scale_y_continuous(breaks = seq(1, 9, 1)),
        messages = FALSE
      ),
      what = "gg"
    ))
  }
)

# subtitle return --------------------------------------------------

testthat::test_that(
  desc = "subtitle return",
  code = {
    testthat::skip_on_cran()

    # should return a list of length 5
    set.seed(123)
    ls_results <- ggstatsplot::grouped_ggbetweenstats(
      data = dplyr::sample_frac(forcats::gss_cat, 0.25),
      x = race,
      y = tvhours,
      grouping.var = marital,
      return = "subtitle",
      messages = FALSE
    )

    # tests
    testthat::expect_equal(length(ls_results), 5L)
    testthat::expect_identical(
      ls_results[[1]],
      ggplot2::expr(
        paste(
          NULL,
          italic("F"),
          "(",
          "2",
          ",",
          "287.06",
          ") = ",
          "14.02",
          ", ",
          italic("p"),
          " = ",
          "< 0.001",
          ", ",
          omega["p"]^2,
          " = ",
          "0.03",
          ", CI"["95%"],
          " [",
          "0.01",
          ", ",
          "0.07",
          "]",
          ", ",
          italic("n"),
          " = ",
          779L
        )
      )
    )
    testthat::expect_identical(
      ls_results[[2]],
      ggplot2::expr(
        paste(
          NULL,
          italic("F"),
          "(",
          "2",
          ",",
          "39.53",
          ") = ",
          "3.24",
          ", ",
          italic("p"),
          " = ",
          "0.050",
          ", ",
          omega["p"]^2,
          " = ",
          "0.06",
          ", CI"["95%"],
          " [",
          "-0.04",
          ", ",
          "0.18",
          "]",
          ", ",
          italic("n"),
          " = ",
          107L
        )
      )
    )
    testthat::expect_identical(
      ls_results[[3]],
      ggplot2::expr(
        paste(
          NULL,
          italic("F"),
          "(",
          "2",
          ",",
          "60.53",
          ") = ",
          "4.46",
          ", ",
          italic("p"),
          " = ",
          "0.016",
          ", ",
          omega["p"]^2,
          " = ",
          "0.02",
          ", CI"["95%"],
          " [",
          "-0.01",
          ", ",
          "0.05",
          "]",
          ", ",
          italic("n"),
          " = ",
          451L
        )
      )
    )
    testthat::expect_identical(
      ls_results[[4]],
      ggplot2::expr(
        paste(
          NULL,
          italic("F"),
          "(",
          "2",
          ",",
          "15.50",
          ") = ",
          "4.14",
          ", ",
          italic("p"),
          " = ",
          "0.036",
          ", ",
          omega["p"]^2,
          " = ",
          "0.05",
          ", CI"["95%"],
          " [",
          "-0.01",
          ", ",
          "0.12",
          "]",
          ", ",
          italic("n"),
          " = ",
          249L
        )
      )
    )
    testthat::expect_identical(
      ls_results[[5]],
      ggplot2::expr(
        paste(
          NULL,
          italic("F"),
          "(",
          "2",
          ",",
          "163.64",
          ") = ",
          "6.96",
          ", ",
          italic("p"),
          " = ",
          "0.001",
          ", ",
          omega["p"]^2,
          " = ",
          "0.02",
          ", CI"["95%"],
          " [",
          "0.00",
          ", ",
          "0.04",
          "]",
          ", ",
          italic("n"),
          " = ",
          1264L
        )
      )
    )
  }
)
