# data for paired tests
set.seed(123)
survey_data <- dplyr::tibble(
  `1st survey` = c("Approve", "Approve", "Disapprove", "Disapprove"),
  `2nd survey` = c("Approve", "Disapprove", "Approve", "Disapprove"),
  `Counts` = c(794L, 150L, 86L, 570L)
)

survey_data_NA <- dplyr::tibble(
  `1st survey` = c("Approve", "Approve", "Disapprove", "Disapprove"),
  `2nd survey` = c("Approve", "Disapprove", "Approve", "Disapprove"),
  `Counts` = c(794L, 150L, NA_integer_, 570L)
)


# checking default outputs -----------------------------------------

test_that(
  desc = "checking default outputs",
  code = {
    set.seed(123)
    expect_doppelganger(
      title = "checking unpaired two-way table - without NA",
      fig = ggbarstats(mtcars, am, cyl)
    )

    set.seed(123)
    expect_doppelganger(
      title = "checking unpaired two-way table - with NA",
      fig = ggbarstats(ggplot2::msleep, conservation, vore)
    )

    set.seed(123)
    expect_doppelganger(
      title = "checking paired two-way table - without NA",
      fig = ggbarstats(survey_data, `1st survey`, `2nd survey`,
        counts = Counts,
        paired = TRUE
      )
    )

    set.seed(123)
    expect_doppelganger(
      title = "checking paired two-way table - with NA",
      fig = ggbarstats(
        data = survey_data_NA,
        x = `1st survey`,
        y = `2nd survey`,
        counts = Counts,
        paired = TRUE
      )
    )
  }
)

# changing labels and aesthetics -------------------------------------------

test_that(
  desc = "changing labels and aesthetics",
  code = {
    set.seed(123)
    expect_doppelganger(
      title = "checking percentage labels",
      fig = ggbarstats(
        data = mtcars, x = cyl, y = am, label = "percentage",
        results.subtitle = FALSE
      )
    )

    set.seed(123)
    expect_doppelganger(
      title = "checking count labels",
      fig = ggbarstats(
        data = mtcars, x = cyl, y = am, label = "counts",
        results.subtitle = FALSE
      )
    )

    set.seed(123)
    expect_doppelganger(
      title = "checking percentage and count labels",
      fig = ggbarstats(
        data = mtcars, x = cyl, y = am, label = "both",
        results.subtitle = FALSE
      )
    )

    set.seed(123)
    expect_doppelganger(
      title = "changing aesthetics works",
      fig = suppressWarnings(
        ggbarstats(
          data = mtcars,
          x = am,
          y = cyl,
          perc.k = 2L,
          title = "mtcars dataset",
          package = "wesanderson",
          palette = "Royal2",
          ggtheme = ggplot2::theme_bw(),
          label = "counts",
          legend.title = "transmission",
          results.subtitle = FALSE
        )
      )
    )

    # data
    df <- structure(
      list(
        epoch = structure(
          c(1L, 2L, 1L, 2L, 1L, 2L, 1L, 2L),
          .Label = c("Before", "After"),
          class = "factor"
        ),
        mode = structure(c(1L, 1L, 2L, 2L, 3L, 3L, 4L, 4L),
          .Label = c("A", "P", "C", "T"), class = "factor"
        ),
        counts = c(30916L, 21117L, 7676L, 1962L, 1663L, 462L, 7221L, 197L),
        perc = c(65.119, 88.958, 16.168, 8.265, 3.502, 1.946, 15.209, 0.829),
        label = c("65%", "89%", "16%", "8%", "4%", "2%", "15%", "1%")
      ),
      row.names = c(NA, -8L),
      class = c("tbl_df", "tbl", "data.frame")
    )

    set.seed(123)
    expect_doppelganger(
      title = "label repelling works",
      fig = ggbarstats(
        df,
        mode,
        epoch,
        counts = counts,
        label.repel = TRUE,
        type = "bayes"
      )
    )
  }
)

# edge cases ---------------------------------------------------------

test_that(
  desc = "edge cases",
  code = {
    # dropped level dataset
    mtcars_small <- dplyr::filter(mtcars, am == "0")

    # TODO: should one-way table results be shown in the subtitle?
    set.seed(123)
    expect_doppelganger(
      title = "works with dropped levels",
      fig = ggbarstats(mtcars_small, cyl, am)
    )

    set.seed(123)
    expect_doppelganger(
      title = "prop test fails with dropped levels",
      fig = ggbarstats(mtcars_small, am, cyl)
    )
  }
)

# expression output --------------------------------------------------

test_that(
  desc = "expression output",
  code = {
    set.seed(123)
    p_sub <- ggbarstats(
      data = ggplot2::msleep,
      x = conservation,
      y = vore,
      k = 4L
    ) %>%
      extract_subtitle()

    set.seed(123)
    stats_output <- suppressWarnings(contingency_table(
      data = ggplot2::msleep,
      x = conservation,
      y = vore,
      k = 4L
    ))$expression[[1L]]

    expect_identical(p_sub, stats_output)
  }
)

test_that(
  desc = "grouped_ggbarstats produces error when grouping variable not provided",
  code = {
    expect_snapshot_error(grouped_ggbarstats(mtcars, x = cyl, y = am))
  }
)

test_that(
  desc = "grouped_ggbarstats works",
  code = {
    # creating a smaller data frame
    mpg_short <- ggplot2::mpg %>%
      dplyr::filter(
        drv %in% c("4", "f"),
        class %in% c("suv", "midsize"),
        trans %in% c("auto(l4)", "auto(l5)")
      )

    # when arguments are entered as bare expressions
    set.seed(123)
    expect_doppelganger(
      title = "grouped_ggbarstats with two-way table",
      fig = grouped_ggbarstats(
        data = mpg_short,
        x = cyl,
        y = class,
        grouping.var = drv,
        label.repel = TRUE
      )
    )
  }
)

# edge cases --------------------

test_that(
  desc = "edge case behavior",
  code = {
    df <- data.frame(
      dataset = c("a", "b", "c", "c", "c", "c"),
      measurement = c("old", "old", "old", "old", "new", "new"),
      flag = c("no", "no", "yes", "no", "yes", "no"),
      count = c(6, 8, 8, 62, 6, 33)
    )

    set.seed(123)
    expect_doppelganger(
      title = "common legend when levels are dropped",
      fig = grouped_ggbarstats(
        data = df,
        x = measurement,
        y = flag,
        grouping.var = dataset,
        counts = count,
        results.subtitle = FALSE,
        proportion.test = FALSE
      )
    )
  }
)
