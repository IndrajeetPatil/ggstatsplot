# between-subjects -------------------------------------------------

test_that("check pairwise displays - between-subjects", {
  expect_pairwise_displays(
    "between - parametric",
    ggbetweenstats,
    ggplot2::msleep,
    vore,
    brainwt,
    type = "p",
    p.adjust.method = "fdr"
  )

  expect_pairwise_displays(
    "between - non-parametric",
    ggbetweenstats,
    movies_long,
    mpaa,
    rating,
    type = "np",
    p.adjust.method = "bonferroni"
  )

  expect_pairwise_displays(
    "between - robust",
    ggbetweenstats,
    ggplot2::msleep,
    vore,
    sleep_rem,
    type = "r",
    p.adjust.method = "holm"
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
  expect_pairwise_displays(
    "within - parametric",
    ggwithinstats,
    bugs_long,
    condition,
    desire,
    type = "p",
    p.adjust.method = "fdr"
  )

  expect_pairwise_displays(
    "within - non-parametric",
    ggwithinstats,
    bugs_long,
    condition,
    desire,
    type = "np",
    p.adjust.method = "bonferroni"
  )

  expect_pairwise_displays(
    "within - robust",
    ggwithinstats,
    bugs_long,
    condition,
    desire,
    type = "r",
    p.adjust.method = "holm"
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

# bracket positions -------------------------------------------------

test_that("pairwise brackets stay above negative outcomes", {
  df <- tibble::tibble(
    group = rep(letters[1:3], each = 3L),
    value = -103:-95
  )

  set.seed(123)
  expect_doppelganger(
    title = "brackets above negative outcomes",
    fig = ggbetweenstats(
      df,
      group,
      value,
      results.subtitle = FALSE,
      centrality.plotting = FALSE,
      pairwise.display = "all"
    )
  )
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
    signif_layer_data()

  expect_true(all(is.finite(bracket_data$y)))
  expect_true(all(is.finite(bracket_data$yend)))
})
