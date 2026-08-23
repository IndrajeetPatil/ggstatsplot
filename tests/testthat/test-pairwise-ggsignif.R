# between-subjects -------------------------------------------------

test_that("check pairwise displays - between-subjects", {
  set.seed(123)
  expect_doppelganger(
    title = "between - parametric - only non-significant",
    fig = ggbetweenstats(
      ggplot2::msleep,
      vore,
      brainwt,
      type = "p",
      results.subtitle = FALSE,
      p.adjust.method = "fdr",
      pairwise.display = "ns",
      digits = 3L
    )
  )

  set.seed(123)
  expect_doppelganger(
    title = "between - parametric - only significant",
    fig = ggbetweenstats(
      ggplot2::msleep,
      vore,
      brainwt,
      type = "p",
      results.subtitle = FALSE,
      p.adjust.method = "fdr",
      pairwise.display = "s",
      digits = 3L
    )
  )

  set.seed(123)
  expect_doppelganger(
    title = "between - parametric - all",
    fig = ggbetweenstats(
      ggplot2::msleep,
      vore,
      brainwt,
      type = "p",
      results.subtitle = FALSE,
      p.adjust.method = "fdr",
      pairwise.display = "all",
      digits = 3L
    )
  )

  set.seed(123)
  expect_doppelganger(
    title = "between - non-parametric - only non-significant",
    fig = ggbetweenstats(
      movies_long,
      mpaa,
      rating,
      type = "np",
      results.subtitle = FALSE,
      p.adjust.method = "bonferroni",
      pairwise.display = "ns",
      digits = 3L
    )
  )

  set.seed(123)
  expect_doppelganger(
    title = "between - non-parametric - only significant",
    fig = ggbetweenstats(
      movies_long,
      mpaa,
      rating,
      type = "np",
      results.subtitle = FALSE,
      p.adjust.method = "bonferroni",
      pairwise.display = "s",
      digits = 3L
    )
  )

  set.seed(123)
  expect_doppelganger(
    title = "between - non-parametric - all",
    fig = ggbetweenstats(
      movies_long,
      mpaa,
      rating,
      type = "np",
      results.subtitle = FALSE,
      p.adjust.method = "bonferroni",
      pairwise.display = "all",
      digits = 3L
    )
  )

  set.seed(123)
  expect_doppelganger(
    title = "between - robust - only non-significant",
    fig = ggbetweenstats(
      ggplot2::msleep,
      vore,
      sleep_rem,
      type = "r",
      results.subtitle = FALSE,
      p.adjust.method = "holm",
      pairwise.display = "ns",
      digits = 3L
    )
  )

  set.seed(123)
  expect_doppelganger(
    title = "between - robust - only significant",
    fig = ggbetweenstats(
      ggplot2::msleep,
      vore,
      sleep_rem,
      type = "r",
      results.subtitle = FALSE,
      p.adjust.method = "holm",
      pairwise.display = "s",
      digits = 3L
    )
  )

  set.seed(123)
  expect_doppelganger(
    title = "between - robust - all",
    fig = ggbetweenstats(
      ggplot2::msleep,
      vore,
      sleep_rem,
      type = "r",
      results.subtitle = FALSE,
      p.adjust.method = "holm",
      pairwise.display = "all",
      digits = 3L
    )
  )

  set.seed(123)
  expect_doppelganger(
    title = "between - bayes",
    fig = ggbetweenstats(
      mtcars,
      cyl,
      mpg,
      type = "bayes",
      results.subtitle = FALSE,
      digits = 3L
    )
  )

  set.seed(123)
  expect_doppelganger(
    title = "between - parametric - stricter alpha",
    fig = ggbetweenstats(
      mtcars,
      cyl,
      mpg,
      type = "p",
      results.subtitle = FALSE,
      p.adjust.method = "holm",
      pairwise.display = "s",
      pairwise.alpha = 0.001,
      digits = 3L
    )
  )
})

# within-subjects -------------------------------------------------

test_that("check pairwise displays - within-subjects", {
  set.seed(123)
  expect_doppelganger(
    title = "within - parametric - only non-significant",
    fig = ggwithinstats(
      bugs_long,
      condition,
      desire,
      type = "p",
      results.subtitle = FALSE,
      p.adjust.method = "fdr",
      pairwise.display = "ns",
      digits = 3L
    )
  )

  set.seed(123)
  expect_doppelganger(
    title = "within - parametric - only significant",
    fig = ggwithinstats(
      bugs_long,
      condition,
      desire,
      type = "p",
      results.subtitle = FALSE,
      p.adjust.method = "fdr",
      pairwise.display = "s",
      digits = 3L
    )
  )

  set.seed(123)
  expect_doppelganger(
    title = "within - parametric - all",
    fig = ggwithinstats(
      bugs_long,
      condition,
      desire,
      type = "p",
      results.subtitle = FALSE,
      p.adjust.method = "fdr",
      pairwise.display = "all",
      digits = 3L
    )
  )

  set.seed(123)
  expect_doppelganger(
    title = "within - non-parametric - only non-significant",
    fig = ggwithinstats(
      bugs_long,
      condition,
      desire,
      type = "np",
      results.subtitle = FALSE,
      p.adjust.method = "bonferroni",
      pairwise.display = "ns",
      digits = 3L
    )
  )

  set.seed(123)
  expect_doppelganger(
    title = "within - non-parametric - only significant",
    fig = ggwithinstats(
      bugs_long,
      condition,
      desire,
      type = "np",
      results.subtitle = FALSE,
      p.adjust.method = "bonferroni",
      pairwise.display = "s",
      digits = 3L
    )
  )

  set.seed(123)
  expect_doppelganger(
    title = "within - non-parametric - all",
    fig = ggwithinstats(
      bugs_long,
      condition,
      desire,
      type = "np",
      results.subtitle = FALSE,
      p.adjust.method = "bonferroni",
      pairwise.display = "all",
      digits = 3L
    )
  )

  set.seed(123)
  expect_doppelganger(
    title = "within - robust - only non-significant",
    fig = ggwithinstats(
      bugs_long,
      condition,
      desire,
      type = "r",
      results.subtitle = FALSE,
      p.adjust.method = "holm",
      pairwise.display = "ns",
      digits = 3L
    )
  )

  set.seed(123)
  expect_doppelganger(
    title = "within - robust - only significant",
    fig = ggwithinstats(
      bugs_long,
      condition,
      desire,
      type = "r",
      results.subtitle = FALSE,
      p.adjust.method = "holm",
      pairwise.display = "s",
      digits = 3L
    )
  )

  set.seed(123)
  expect_doppelganger(
    title = "within - robust - all",
    fig = ggwithinstats(
      bugs_long,
      condition,
      desire,
      type = "r",
      results.subtitle = FALSE,
      p.adjust.method = "holm",
      pairwise.display = "all",
      digits = 3L
    )
  )

  set.seed(123)
  expect_doppelganger(
    title = "within - bayes",
    fig = ggwithinstats(
      bugs_long,
      condition,
      desire,
      type = "bayes",
      results.subtitle = FALSE,
      digits = 3L
    )
  )

  set.seed(123)
  expect_doppelganger(
    title = "within - parametric - stricter alpha",
    fig = ggwithinstats(
      bugs_long,
      condition,
      desire,
      type = "p",
      subject.id = subject,
      results.subtitle = FALSE,
      p.adjust.method = "fdr",
      pairwise.display = "s",
      pairwise.alpha = 0.001,
      digits = 3L
    )
  )
})


# caption -------------------------------------------------

test_that("adding caption works", {
  set.seed(123)
  expect_doppelganger(
    title = "adding caption works",
    fig = ggwithinstats(
      bugs_long,
      condition,
      desire,
      type = "p",
      results.subtitle = FALSE,
      bf.message = FALSE,
      p.adjust.method = "fdr",
      pairwise.display = "ns",
      digits = 3L
    )
  )
})

test_that("pairwise brackets stay above negative outcomes", {
  set.seed(123)
  df <- tibble::tibble(
    group = rep(letters[1:3], each = 3L),
    value = -103:-95
  )

  plot <- ggbetweenstats(
    df,
    group,
    value,
    results.subtitle = FALSE,
    centrality.plotting = FALSE,
    pairwise.display = "all"
  )
  bracket_data <- ggplot2::ggplot_build(plot)$data |>
    tail(1L) |>
    purrr::pluck(1L)
  horizontal_brackets <- bracket_data[bracket_data$y == bracket_data$yend, ]

  expect_gt(min(bracket_data$y), max(df$value))
  expect_length(unique(horizontal_brackets$y), 3L)
})

test_that("pairwise brackets have finite positions for constant outcomes", {
  df <- tibble::tibble(
    group = rep(c("a", "b"), each = 2L),
    value = 10
  )
  mpc_df <- tibble::tibble(
    group1 = "a",
    group2 = "b",
    p.value = 0.01,
    expression = "italic(p)==0.01"
  )
  plot <- ggplot2::ggplot(df, ggplot2::aes(group, value)) +
    ggplot2::geom_point()
  bracket_data <- .ggsignif_adder(
    plot,
    df,
    group,
    value,
    mpc_df,
    pairwise.display = "all"
  ) |>
    ggplot2::ggplot_build() |>
    purrr::pluck("data") |>
    tail(1L) |>
    purrr::pluck(1L)

  expect_true(all(is.finite(bracket_data$y)))
  expect_true(all(is.finite(bracket_data$yend)))
  expect_gt(min(bracket_data$y), max(df$value))
})
