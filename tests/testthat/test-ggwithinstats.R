# graphical pairwise comparisons are tested in `test-pairwise-ggsignif.R`

skip_if_not_installed("afex")
skip_if_not_installed("WRS2")
skip_if_not_installed("rstantools")

test_that("defaults plots", {
  set.seed(123)
  expect_doppelganger(
    title = "defaults plots - two groups",
    fig = ggwithinstats(
      data = data_bugs_2,
      x = condition,
      y = desire,
      subject.id = subject,
      pairwise.display = "none",
      ggsignif.args = list(textsize = 6, tip_length = 0.01),
      point.path.args = list(color = "red"),
      centrality.path.args = list(color = "blue", size = 2, alpha = 0.8),
      centrality.point.args = list(size = 3, color = "darkgreen", alpha = 0.5),
      title = "bugs dataset"
    )
  )

  set.seed(123)
  expect_doppelganger(
    title = "defaults plots - more than two groups",
    fig = ggwithinstats(
      data = WRS2::WineTasting,
      x = Wine,
      y = Taste,
      subject.id = Taster,
      pairwise.display = "none",
      title = "wine tasting data"
    )
  )
})

# sample size labels with centrality.plotting = FALSE (#695) ----------

test_that("sample size labels visible when centrality.plotting is FALSE", {
  set.seed(123)
  expect_doppelganger(
    title = "n labels visible without centrality - within",
    fig = ggwithinstats(
      data = data_bugs_2,
      x = condition,
      y = desire,
      subject.id = subject,
      centrality.plotting = FALSE,
      pairwise.display = "none",
      results.subtitle = FALSE
    )
  )
})

# aesthetic modifications work ------------------------------------------

test_that("aesthetic modifications work", {
  set.seed(123)
  expect_doppelganger(
    title = "ggplot2 commands work",
    fig = ggwithinstats(
      data = WRS2::WineTasting,
      x = Wine,
      y = Taste,
      subject.id = Taster,
      results.subtitle = FALSE,
      pairwise.display = "none",
      ggplot.component = ggplot2::labs(y = "Taste rating")
    )
  )

  set.seed(123)
  expect_doppelganger(
    title = "centrality path can be turned off",
    fig = ggwithinstats(
      iris_long,
      condition,
      value,
      subject.id = id,
      centrality.point.args = list(size = 5, alpha = 0.5, color = "darkred"),
      centrality.path = FALSE,
      results.subtitle = FALSE,
      pairwise.display = "none"
    )
  )
})

test_that("grouped plots work", {
  expect_snapshot_error(grouped_ggwithinstats(
    bugs_long,
    x = condition,
    y = desire
  ))

  set.seed(123)
  snapshot_variant <- if (getRversion() >= "4.7.0") "r-4.7" else NULL
  expect_doppelganger(
    title = "grouped plots - default",
    variant = snapshot_variant,
    fig = grouped_ggwithinstats(
      data = filter(bugs_long, condition %in% c("HDHF", "HDLF")),
      x = condition,
      y = desire,
      subject.id = subject,
      grouping.var = gender,
      type = "np"
    )
  )
})

test_that("type remains the fourth positional argument", {
  expect_no_error(
    ggwithinstats(
      data_bugs_2,
      condition,
      desire,
      "np",
      subject.id = subject,
      pairwise.display = "none",
      results.subtitle = FALSE
    )
  )
})

test_that("subject.id follows type in the function signature", {
  arg_names <- names(formals(ggwithinstats))

  expect_identical(
    arg_names[seq_len(6L)],
    c("data", "x", "y", "type", "subject.id", "pairwise.display")
  )
})

test_that("subject.id keeps partially observed subjects in the plotting data", {
  df_missing <- data.frame(
    condition = c("A", "B", "A", "B", "A", "B"),
    score = c(1, 2, 3, NA, 4, 5),
    id = c(1, 1, 2, 2, 3, 3)
  )

  point_data <- build_within_plot(df_missing, subject.id = id)$data[[1L]]

  expect_shape(point_data, nrow = 5L)
  expect_length(unique(point_data$group), 3L)
})

test_that("incomplete anonymous pairs are excluded from the plotting data", {
  df_missing <- data.frame(
    condition = c("A", "A", "A", "B", "B", "B"),
    score = c(1, 3, 4, 2, NA, 5)
  )

  built_plot <- build_within_plot(df_missing)

  expect_shape(built_plot$data[[1L]], nrow = 4L)
  expect_setequal(unique(built_plot$plot$data$.rowid), c(1, 3))
})

test_that("missing subject.id values are excluded from paired grouping", {
  df_missing_id <- data.frame(
    condition = c("A", "B", "A", "B", "A", "B"),
    score = c(1, 2, 3, 4, 5, 6),
    id = c(1, 1, NA, NA, 2, 2)
  )

  point_data <- build_within_plot(df_missing_id, subject.id = id)$data[[1L]]

  expect_shape(point_data, nrow = 4L)
  expect_false(anyNA(point_data$group))
  expect_setequal(unique(point_data$group), c(1, 2))
})

test_that("empty condition levels are dropped after filtering missing subject.id values", {
  df_missing_id_levels <- data.frame(
    condition = factor(
      c("A", "B", "C", "A", "B", "C"),
      levels = c("A", "B", "C")
    ),
    score = c(1, 2, 3, 4, 5, 6),
    id = c(1, 1, NA, 2, 2, NA)
  )

  built_plot <- build_within_plot(
    df_missing_id_levels,
    subject.id = id,
    centrality.plotting = FALSE
  )

  expect_identical(
    sum(vapply(
      built_plot$plot$layers,
      \(layer) inherits(layer$geom, "GeomPath"),
      logical(1)
    )),
    1L
  )
})

# user caption is kept when no Bayes Factor caption is shown ----------

test_that("user caption is retained without a Bayes Factor caption", {
  p <- ggwithinstats(
    data = data_bugs_2,
    x = condition,
    y = desire,
    subject.id = subject,
    bf.message = FALSE,
    pairwise.display = "none",
    caption = "my caption"
  )

  expect_identical(ggplot2::get_labs(p)$caption, "my caption")
})
