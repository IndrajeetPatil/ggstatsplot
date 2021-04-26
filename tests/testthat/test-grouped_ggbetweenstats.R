# outlier labeling works --------------------------------------------------

test_that(
  desc = "grouping.var works across vector types",
  code = {
    skip_on_cran()

    # creating a smaller dataframe
    set.seed(123)
    dat <- dplyr::sample_frac(tbl = movies_long, size = 0.25) %>%
      dplyr::filter(
        mpaa %in% c("R", "PG-13"),
        genre %in% c("Drama", "Comedy")
      )

    # expect error when no grouping.var is specified
    expect_error(
      grouped_ggbetweenstats(
        data = dat,
        x = genre,
        y = rating
      )
    )

    # outlier tagging is not required
    grouped_ggbetweenstats(
      data = dat,
      x = genre,
      y = rating,
      results.subtitle = FALSE,
      grouping.var = mpaa,
      outlier.tagging = FALSE
    )

    # `outlier.label` is not specified
    set.seed(123)
    expect_true(inherits(
      grouped_ggbetweenstats(
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
    expect_true(inherits(
      grouped_ggbetweenstats(
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

    expect_true(inherits(
      grouped_ggbetweenstats(
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

test_that(
  desc = "subtitle output",
  code = {
    skip_on_cran()

    set.seed(123)
    df <- dplyr::sample_frac(forcats::gss_cat, 0.25) %>%
      dplyr::filter(
        marital %in% c("Never married"),
        race %in% c("White", "Black")
      )


    set.seed(123)
    ls_results <-
      grouped_ggbetweenstats(
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
      ggbetweenstats(
        data = df,
        x = race,
        y = "tvhours",
        output = "subtitle",
        bf.message = FALSE,
        k = 4
      )
    # tests
    expect_equal(ls_results$`Never married`, basic_results)
  }
)
