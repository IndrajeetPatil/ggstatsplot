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
      results.subtitle = FALSE,
      grouping.var = mpaa,
      outlier.tagging = FALSE,
      messages = FALSE
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
        output = "plot",
        results.subtitle = FALSE,
        effsize.type = "biased",
        plot.type = "box",
        bf.message = TRUE,
        outlier.tagging = TRUE,
        pairwise.comparisons = TRUE,
        pairwise.annotation = "p.value",
        messages = FALSE
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
        results.subtitle = FALSE,
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
        results.subtitle = FALSE,
        pairwise.comparisons = TRUE,
        outlier.tagging = TRUE,
        outlier.label = "title",
        outlier.coef = 5,
        ggplot.component = ggplot2::scale_y_continuous(breaks = seq(1, 9, 1)),
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

    # should output a list of length 5
    set.seed(123)
    ls_results <-
      ggstatsplot::grouped_ggbetweenstats(
        data = dplyr::sample_frac(forcats::gss_cat, 0.25),
        x = race,
        y = "tvhours",
        grouping.var = "marital",
        output = "subtitle",
        k = 4,
        messages = FALSE
      )

    # tests
    testthat::expect_equal(length(ls_results), 5L)
    testthat::expect_equal(
      ls_results,
      list(
        `Never married` = ggplot2::expr(paste(
          NULL,
          italic("F"),
          "(",
          "2",
          ",",
          "287.0552",
          ") = ",
          "14.0191",
          ", ",
          italic("p"),
          " = ",
          "< 0.001",
          ", ",
          widehat(omega["p"]^2),
          " = ",
          "0.0316",
          ", CI"["95%"],
          " [",
          "0.0053",
          ", ",
          "0.0660",
          "]",
          ", ",
          italic("n")["obs"],
          " = ",
          779L
        )),
        Separated = ggplot2::expr(paste(
          NULL,
          italic("F"),
          "(",
          "2",
          ",",
          "39.5287",
          ") = ",
          "3.2411",
          ", ",
          italic("p"),
          " = ",
          "0.0497",
          ", ",
          widehat(omega["p"]^2),
          " = ",
          "0.0634",
          ", CI"["95%"],
          " [",
          "-0.0354",
          ", ",
          "0.1806",
          "]",
          ", ",
          italic("n")["obs"],
          " = ",
          107L
        )),
        Divorced = ggplot2::expr(paste(
          NULL,
          italic("F"),
          "(",
          "2",
          ",",
          "60.5312",
          ") = ",
          "4.4639",
          ", ",
          italic("p"),
          " = ",
          "0.0155",
          ", ",
          widehat(omega["p"]^2),
          " = ",
          "0.0170",
          ", CI"["95%"],
          " [",
          "-0.0073",
          ", ",
          "0.0520",
          "]",
          ", ",
          italic("n")["obs"],
          " = ",
          451L
        )),
        Widowed = ggplot2::expr(paste(
          NULL,
          italic("F"),
          "(",
          "2",
          ",",
          "15.4966",
          ") = ",
          "4.1371",
          ", ",
          italic("p"),
          " = ",
          "0.0363",
          ", ",
          widehat(omega["p"]^2),
          " = ",
          "0.0464",
          ", CI"["95%"],
          " [",
          "-0.0149",
          ", ",
          "0.1264",
          "]",
          ", ",
          italic("n")["obs"],
          " = ",
          249L
        )),
        Married = ggplot2::expr(paste(
          NULL,
          italic("F"),
          "(",
          "2",
          ",",
          "163.6435",
          ") = ",
          "6.9614",
          ", ",
          italic("p"),
          " = ",
          "0.0013",
          ", ",
          widehat(omega["p"]^2),
          " = ",
          "0.0215",
          ", CI"["95%"],
          " [",
          "3e-04",
          ", ",
          "0.0427",
          "]",
          ", ",
          italic("n")["obs"],
          " = ",
          1264L
        ))
      )
    )
  }
)
