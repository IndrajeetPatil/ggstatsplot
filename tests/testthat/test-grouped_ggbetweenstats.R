# outlier labeling works --------------------------------------------------

test_that(
  desc = "grouped_ggbetweenstats works - vdiffr",
  code = {


    # creating a smaller dataframe
    set.seed(123)
    dat <- dplyr::sample_frac(movies_long, size = 0.25) %>%
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
    set.seed(123)
    vdiffr::expect_doppelganger(
      title = "no outlier tagging",
      fig = grouped_ggbetweenstats(
        data = dat,
        x = genre,
        y = rating,
        results.subtitle = FALSE,
        grouping.var = mpaa,
        outlier.tagging = FALSE
      )
    )

    # `outlier.label` is not specified
    set.seed(123)
    vdiffr::expect_doppelganger(
      title = "outlier.label not specified",
      fig = grouped_ggbetweenstats(
        data = dat,
        x = genre,
        y = rating,
        grouping.var = mpaa,
        output = "plot",
        ggtheme = ggplot2::theme_linedraw(),
        results.subtitle = FALSE,
        outlier.tagging = TRUE
      )
    )

    # `outlier.label` is factor
    set.seed(123)
    vdiffr::expect_doppelganger(
      title = "outlier.label is factor",
      fig = grouped_ggbetweenstats(
        data = dat,
        x = genre,
        y = rating,
        grouping.var = mpaa,
        type = "np",
        palette = "default_jama",
        package = "ggsci",
        outlier.tagging = TRUE,
        results.subtitle = FALSE,
        outlier.label = title
      )
    )

    # `outlier.label` is character
    set.seed(123)
    dat$title <- as.character(dat$title)

    set.seed(123)
    vdiffr::expect_doppelganger(
      title = "outlier.label is character",
      fig = grouped_ggbetweenstats(
        data = dat,
        x = genre,
        y = rating,
        grouping.var = mpaa,
        type = "r",
        results.subtitle = FALSE,
        outlier.tagging = TRUE,
        outlier.label = title,
        outlier.coef = 5,
        ggplot.component = ggplot2::scale_y_continuous(breaks = seq(1, 9, 1)),
      )
    )
  }
)


# subtitle output --------------------------------------------------

test_that(
  desc = "subtitle output",
  code = {
    set.seed(123)
    df <- dplyr::sample_frac(forcats::gss_cat, 0.25) %>%
      dplyr::filter(
        marital %in% c("Never married"),
        race %in% c("White", "Black")
      )

    set.seed(123)
    ls_results <- grouped_ggbetweenstats(
      data = df,
      x = race,
      y = tvhours,
      grouping.var = marital,
      output = "subtitle",
      bf.message = FALSE,
      k = 4
    )

    set.seed(123)
    basic_results <- ggbetweenstats(
      data = df,
      x = race,
      y = tvhours,
      output = "subtitle",
      bf.message = FALSE,
      k = 4
    )
    # tests
    expect_equal(ls_results$`Never married`, basic_results)
  }
)
