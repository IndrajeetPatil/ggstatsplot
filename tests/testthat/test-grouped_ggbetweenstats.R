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

    # outlier tagging is not required
    ggstatsplot::grouped_ggbetweenstats(
      data = dat,
      x = genre,
      y = rating,
      results.subtitle = FALSE,
      grouping.var = mpaa,
      outlier.tagging = FALSE
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
        pairwise.annotation = "p.value"
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

    set.seed(123)
    df <- dplyr::sample_frac(forcats::gss_cat, 0.25) %>%
      dplyr::filter(
        .data = ., marital %in% c("Never married"),
        race %in% c("White", "Black")
      )


    set.seed(123)
    ls_results <-
      ggstatsplot::grouped_ggbetweenstats(
        data = df,
        x = race,
        y = "tvhours",
        grouping.var = "marital",
        output = "subtitle",
        bf.message = FALSE,
        k = 4
      )


    set.seed(123)
    basic_results <-
      ggstatsplot::ggbetweenstats(
        data = df,
        x = race,
        y = "tvhours",
        output = "subtitle",
        bf.message = FALSE,
        k = 4
      )
    # tests
    testthat::expect_equal(ls_results$`Never married`, basic_results)
  }
)
