# helpers -------------------------------------------------

# snapshot one plot per `pairwise.display` value; `title` is the common prefix
# of the snapshot titles and `...` is passed on to `plot_fn`
expect_pairwise_displays <- function(title, plot_fn, ...) {
  display_labels <- c(
    ns = "only non-significant",
    s = "only significant",
    all = "all"
  )

  for (display in names(display_labels)) {
    set.seed(123)
    expect_doppelganger(
      title = paste(title, display_labels[[display]], sep = " - "),
      fig = plot_fn(
        ...,
        results.subtitle = FALSE,
        pairwise.display = display,
        digits = 3L
      )
    )
  }
}

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
})
