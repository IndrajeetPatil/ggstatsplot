# graphical pairwise comparisons are tested in `test-pairwise_ggsignif.R`

skip_if_not_installed("afex")
skip_if_not_installed("WRS2")
skip_if_not_installed("rstantools")

data_bugs_2 <- dplyr::filter(
  bugs_long,
  subject <= 30L,
  condition %in% c("HDLF", "HDHF")
)

test_that("defaults plots", {
  expect_snapshot_error(grouped_ggbetweenstats(
    bugs_long,
    x = condition,
    y = desire
  ))

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
  expect_snapshot_error(grouped_ggbetweenstats(
    bugs_long,
    x = condition,
    y = desire
  ))

  set.seed(123)
  expect_doppelganger(
    title = "grouped plots - default",
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

test_that("pairwise.alpha controls displayed pairwise comparisons", {
  fig <- ggwithinstats(
    data = bugs_long,
    x = condition,
    y = desire,
    type = "p",
    subject.id = subject,
    pairwise.display = "non-significant",
    pairwise.alpha = 0.001,
    p.adjust.method = "fdr",
    results.subtitle = FALSE
  )

  layer_params <- fig$layers[[length(fig$layers)]]$stat_params
  sec_axis_name <- paste(
    deparse(fig$scales$get_scales("y")$secondary.axis$name),
    collapse = " "
  )

  expect_identical(
    layer_params$comparisons,
    list(
      c("HDHF", "HDLF"),
      c("HDHF", "LDHF"),
      c("HDLF", "LDHF"),
      c("HDLF", "LDLF")
    )
  )
  expect_match(sec_axis_name, "alpha == 0\\.001")
})

test_that("subject.id keeps partially observed subjects in the plotting data", {
  df_missing <- data.frame(
    condition = c("A", "B", "A", "B", "A", "B"),
    score = c(1, 2, 3, NA, 4, 5),
    id = c(1, 1, 2, 2, 3, 3)
  )

  point_data <- ggplot2::ggplot_build(
    ggwithinstats(
      data = df_missing,
      x = condition,
      y = score,
      type = "p",
      subject.id = id,
      pairwise.display = "none",
      results.subtitle = FALSE
    )
  )$data[[1L]]

  expect_identical(nrow(point_data), 5L)
  expect_length(unique(point_data$group), 3L)
})

test_that("missing subject.id values are excluded from paired grouping", {
  df_missing_id <- data.frame(
    condition = c("A", "B", "A", "B", "A", "B"),
    score = c(1, 2, 3, 4, 5, 6),
    id = c(1, 1, NA, NA, 2, 2)
  )

  point_data <- ggplot2::ggplot_build(
    ggwithinstats(
      data = df_missing_id,
      x = condition,
      y = score,
      type = "p",
      subject.id = id,
      pairwise.display = "none",
      results.subtitle = FALSE
    )
  )$data[[1L]]

  expect_identical(nrow(point_data), 4L)
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

  built_plot <- ggplot2::ggplot_build(
    ggwithinstats(
      data = df_missing_id_levels,
      x = condition,
      y = score,
      type = "p",
      subject.id = id,
      pairwise.display = "none",
      results.subtitle = FALSE,
      centrality.plotting = FALSE
    )
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
