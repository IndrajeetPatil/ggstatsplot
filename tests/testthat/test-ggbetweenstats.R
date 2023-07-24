# pairwise comparisons testing is done `test-pairwise_ggsignif.R`
skip_if_not_installed("PMCMRplus")
skip_if_not_installed("rstantools")

# checking labels and data from plot -------------------------------------

test_that(
  desc = "plotting features work as expected",
  code = {
    set.seed(123)
    expect_doppelganger(
      title = "modification with ggplot2 works as expected",
      fig = ggbetweenstats(
        data = mtcars,
        x = am,
        y = wt,
        pairwise.display = "none",
        results.subtitle = FALSE
      ) +
        ggplot2::labs(x = "Transmission", y = "Weight")
    )

    # edge case
    df_small <- data.frame(
      centrality.a = c(1.1, 0.9, 0.94, 1.58, 1.2, 1.4),
      group = c("a", "a", "a", "b", "b", "b")
    )

    set.seed(123)
    expect_doppelganger(
      title = "mean shown with scarce data",
      fig = suppressWarnings(ggbetweenstats(
        data = df_small,
        x = group,
        y = centrality.a,
        pairwise.display = "none",
        results.subtitle = FALSE
      ))
    )

    set.seed(123)
    expect_doppelganger(
      title = "specific geoms removed",
      ggbetweenstats(
        data = mtcars,
        x = am,
        y = wt,
        xlab = "Transmission",
        ylab = "Weight",
        violin.args = list(width = 0),
        boxplot.args = list(width = 0),
        point.args = list(alpha = 0),
        title = "Bayesian Test"
      )
    )
  }
)

# subtitle output works ------------------------------------------------

test_that(
  desc = "subtitle output works",
  code = {
    skip_on_cran()

    df <- mtcars
    df$wt[3] <- NA

    # plot
    set.seed(123)
    subtitle_exp <- ggbetweenstats(
      data = df,
      x = am,
      y = wt
    ) %>%
      extract_subtitle()

    set.seed(123)
    sub <- two_sample_test(
      data = df,
      x = am,
      y = wt
    )$expression[[1L]]

    expect_identical(as.character(subtitle_exp), as.character(sub))
  }
)

# grouped_ggbetweenstats defaults --------------------------------------------------

test_that(
  desc = "grouped_ggbetweenstats defaults",
  code = {
    # expect error when no grouping.var is specified
    expect_snapshot_error(grouped_ggbetweenstats(mtcars, x = am, y = wt))

    # creating a smaller data frame
    set.seed(123)
    dat <- dplyr::sample_frac(movies_long, size = 0.25) %>%
      dplyr::filter(
        mpaa %in% c("R", "PG-13"),
        genre %in% c("Drama", "Comedy")
      )

    set.seed(123)
    expect_doppelganger(
      title = "default plot as expected",
      fig = grouped_ggbetweenstats(
        data = dat,
        x = genre,
        y = rating,
        grouping.var = mpaa,
        ggplot.component = ggplot2::labs(x = "Movie Genre")
      )
    )

    set.seed(123)
    expect_doppelganger(
      title = "plot with outliers as expected",
      fig = grouped_ggbetweenstats(
        data             = dplyr::filter(movies_long, genre %in% c("Action", "Comedy")),
        x                = mpaa,
        y                = length,
        grouping.var     = genre,
        ggsignif.args    = list(textsize = 4, tip_length = 0.01),
        p.adjust.method  = "bonferroni",
        palette          = "default_jama",
        package          = "ggsci",
        plotgrid.args    = list(nrow = 1),
        annotation.args  = list(title = "Differences in movie length by mpaa ratings for different genres")
      )
    )
  }
)
