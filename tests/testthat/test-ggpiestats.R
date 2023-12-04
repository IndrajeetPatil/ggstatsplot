# data for paired tests
set.seed(123)
survey_data <- dplyr::tibble(
  `1st survey` = c("Approve", "Approve", "Disapprove", "Disapprove"),
  `2nd survey` = c("Approve", "Disapprove", "Approve", "Disapprove"),
  Counts = c(794L, 150L, 86L, 570L)
)

survey_data_NA <- dplyr::tibble(
  `1st survey` = c("Approve", "Approve", "Disapprove", "Disapprove"),
  `2nd survey` = c("Approve", "Disapprove", "Approve", "Disapprove"),
  Counts = c(794L, 150L, NA_integer_, 570L)
)

# checking default outputs -----------------------------------------

test_that(
  "checking default outputs",
  {
    set.seed(123)
    expect_doppelganger(
      title = "checking one-way table - without NA",
      fig = ggpiestats(mtcars, cyl)
    )

    set.seed(123)
    expect_doppelganger(
      title = "checking one-way table - with NA",
      fig = ggpiestats(ggplot2::msleep, vore)
    )

    set.seed(123)
    expect_doppelganger(
      title = "checking unpaired two-way table - without NA",
      fig = ggpiestats(mtcars, am, cyl)
    )

    set.seed(123)
    expect_doppelganger(
      title = "checking unpaired two-way table - with NA",
      fig = ggpiestats(ggplot2::msleep, conservation, vore)
    )

    set.seed(123)
    expect_doppelganger(
      title = "checking paired two-way table - without NA",
      fig = ggpiestats(
        survey_data,
        `1st survey`,
        `2nd survey`,
        counts = Counts,
        paired = TRUE
      )
    )

    set.seed(123)
    expect_doppelganger(
      title = "checking paired two-way table - with NA",
      fig = ggpiestats(
        survey_data_NA,
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
  "changing labels and aesthetics",
  {
    set.seed(123)
    expect_doppelganger(
      title = "checking percentage labels",
      fig = ggpiestats(
        mtcars,
        x = cyl,
        label = "percentage",
        results.subtitle = FALSE
      )
    )

    set.seed(123)
    expect_doppelganger(
      title = "checking count labels",
      fig = ggpiestats(
        mtcars,
        x = cyl,
        label = "counts",
        results.subtitle = FALSE
      )
    )

    set.seed(123)
    expect_doppelganger(
      title = "checking percentage and count labels",
      fig = ggpiestats(
        mtcars,
        x = cyl,
        label = "both",
        results.subtitle = FALSE
      )
    )

    set.seed(123)
    expect_doppelganger(
      title = "changing aesthetics works",
      fig = suppressWarnings(
        ggpiestats(
          mtcars,
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

    set.seed(123)
    expect_doppelganger(
      title = "label repelling works",
      fig = ggpiestats(
        mtcars,
        am,
        vs,
        label.repel = TRUE,
        results.subtitle = FALSE
      )
    )
  }
)

# edge cases ---------------------------------------------------------

test_that(
  "edge cases",
  {
    # dropped level dataset
    mtcars_small <- dplyr::filter(mtcars, am == "0")

    # TODO: should one-way table results be shown in the subtitle?
    set.seed(123)
    expect_doppelganger(
      title = "works with dropped levels",
      fig = ggpiestats(mtcars_small, cyl, am)
    )

    set.seed(123)
    expect_doppelganger(
      title = "prop test fails with dropped levels",
      fig = ggpiestats(mtcars_small, am, cyl)
    )
  }
)

# expression output --------------------------------------------------

test_that(
  "expression output",
  {
    set.seed(123)
    p_sub <- ggpiestats(
      ggplot2::msleep,
      x = conservation,
      y = vore,
      k = 4L
    ) %>%
      extract_subtitle()

    set.seed(123)
    stats_output <- suppressWarnings(contingency_table(
      ggplot2::msleep,
      x = conservation,
      y = vore,
      k = 4L
    ))$expression[[1L]]

    expect_identical(p_sub, stats_output)
  }
)

# grouped_ggpiestats works as expected ---------------------

test_that(
  "grouped_ggpiestats produces error when grouping variable not provided",
  {
    expect_snapshot_error(grouped_ggpiestats(mtcars, x = cyl))
  }
)

test_that(
  "grouped_ggpiestats works",
  {
    set.seed(123)
    expect_doppelganger(
      title = "grouped_ggpiestats with one-way table",
      fig = grouped_ggpiestats(
        mtcars,
        grouping.var = am,
        x = cyl
      )
    )

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
      title = "grouped_ggpiestats with two-way table",
      fig = grouped_ggpiestats(
        mpg_short,
        x = cyl,
        y = class,
        grouping.var = drv,
        label.repel = TRUE
      )
    )
  }
)

# grouped_ggpiestats edge cases --------------------

test_that(
  "edge case behavior",
  {
    df <- data.frame(
      dataset = c("a", "b", "c", "c", "c", "c"),
      measurement = c("old", "old", "old", "old", "new", "new"),
      flag = c("no", "no", "yes", "no", "yes", "no"),
      count = c(6, 8, 8, 62, 6, 33)
    )

    set.seed(123)
    expect_doppelganger(
      title = "common legend when levels are dropped",
      fig = grouped_ggpiestats(
        df,
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
