# pairwise comparisons are tested in `test-pairwise-ggsignif.R`

skip_if_not_installed("rstantools")

# plotting features -------------------------------------

test_that("plotting features work as expected", {
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
    fig = ggbetweenstats(
      data = mtcars,
      x = am,
      y = wt,
      xlab = "Transmission",
      ylab = "Weight",
      violin.args = list(width = 0, linewidth = 0),
      boxplot.args = list(width = 0),
      point.args = list(alpha = 0),
      title = "Bayesian Test"
    )
  )
})

# sample size labels with centrality.plotting = FALSE (#695) ----------

test_that("sample size labels visible when centrality.plotting is FALSE", {
  set.seed(123)
  expect_doppelganger(
    title = "n labels visible without centrality",
    fig = ggbetweenstats(
      data = mtcars,
      x = am,
      y = wt,
      centrality.plotting = FALSE,
      pairwise.display = "none",
      results.subtitle = FALSE
    )
  )
})

# grouped_ggbetweenstats defaults --------------------------------------------------

test_that("grouped_ggbetweenstats defaults", {
  # expect error when no grouping.var is specified
  expect_snapshot_error(grouped_ggbetweenstats(mtcars, x = am, y = wt))

  # creating a smaller data frame
  set.seed(123)
  dat <- dplyr::sample_frac(movies_long, size = 0.25) |>
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
      data = dplyr::filter(movies_long, genre %in% c("Action", "Comedy")),
      x = mpaa,
      y = length,
      grouping.var = genre,
      ggsignif.args = list(textsize = 4, tip_length = 0.01),
      p.adjust.method = "bonferroni",
      palette = "ggsci::default_jama",
      plotgrid.args = list(nrow = 1),
      annotation.args = list(
        title = "Differences in movie length by mpaa ratings for different genres"
      )
    )
  )
})

# user caption is kept when no Bayes Factor caption is shown ----------

test_that("user caption is retained without a Bayes Factor caption", {
  p <- ggbetweenstats(
    data = mtcars,
    x = am,
    y = wt,
    type = "np",
    pairwise.display = "none",
    caption = "my caption"
  )

  expect_identical(ggplot2::get_labs(p)$caption, "my caption")
})
