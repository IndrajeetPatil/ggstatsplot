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
    skip_if_not_installed("vdiffr")
    skip_if(getRversion() < "4.1")
    skip_if(getRversion() >= "4.2")

    set.seed(123)
    vdiffr::expect_doppelganger(
      title = "checking one-way table - without NA",
      fig = ggpiestats(mtcars, cyl)
    )

    set.seed(123)
    vdiffr::expect_doppelganger(
      title = "checking one-way table - with NA",
      fig = ggpiestats(ggplot2::msleep, vore)
    )

    set.seed(123)
    vdiffr::expect_doppelganger(
      title = "checking unpaired two-way table - without NA",
      fig = ggpiestats(mtcars, am, cyl)
    )

    set.seed(123)
    vdiffr::expect_doppelganger(
      title = "checking unpaired two-way table - with NA",
      fig = ggpiestats(ggplot2::msleep, conservation, vore)
    )

    set.seed(123)
    vdiffr::expect_doppelganger(
      title = "checking paired two-way table - without NA",
      fig = ggpiestats(survey_data, `1st survey`, `2nd survey`,
        counts = Counts,
        paired = TRUE
      )
    )

    set.seed(123)
    vdiffr::expect_doppelganger(
      title = "checking paired two-way table - with NA",
      fig = ggpiestats(
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
    vdiffr::expect_doppelganger(
      title = "checking percentage labels",
      fig = ggpiestats(
        data = mtcars, x = cyl, label = "percentage",
        results.subtitle = FALSE
      )
    )

    set.seed(123)
    vdiffr::expect_doppelganger(
      title = "checking count labels",
      fig = ggpiestats(
        data = mtcars, x = cyl, label = "counts",
        results.subtitle = FALSE
      )
    )

    set.seed(123)
    vdiffr::expect_doppelganger(
      title = "checking percentage and count labels",
      fig = ggpiestats(
        data = mtcars, x = cyl, label = "both",
        results.subtitle = FALSE
      )
    )

    set.seed(123)
    vdiffr::expect_doppelganger(
      title = "changing aesthetics works",
      fig = suppressWarnings(
        ggpiestats(
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
    df <- structure(list(
      epoch = structure(
        c(1L, 2L, 1L, 2L, 1L, 2L, 1L, 2L),
        .Label = c("Before", "After"),
        class = "factor"
      ),
      mode = structure(c(1L, 1L, 2L, 2L, 3L, 3L, 4L, 4L),
        .Label = c("A", "P", "C", "T"), class = "factor"
      ),
      counts = c(30916L, 21117L, 7676L, 1962L, 1663L, 462L, 7221L, 197L),
      perc = c(
        65.1192181312663,
        88.9586317297161,
        16.1681691802174,
        8.26522874715646,
        3.50282247872609,
        1.94624652455978,
        15.2097902097902,
        0.829892998567697
      ),
      label = c(
        "65%", "89%", "16%", "8%",
        "4%", "2%", "15%", "1%"
      )
    ),
    row.names = c(NA, -8L),
    class = c("tbl_df", "tbl", "data.frame")
    )

    set.seed(123)
    vdiffr::expect_doppelganger(
      title = "label repelling works",
      fig = ggpiestats(
        df,
        mode,
        epoch,
        counts = counts,
        label.repel = TRUE,
        results.subtitle = FALSE
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
    vdiffr::expect_doppelganger(
      title = "when levels are dropped, the function still works",
      fig = ggpiestats(mtcars_small, cyl, am)
    )

    set.seed(123)
    vdiffr::expect_doppelganger(
      title = "when levels are dropped, proportion tests fail but function works",
      fig = ggpiestats(mtcars_small, am, cyl)
    )
  }
)

# expression output --------------------------------------------------

test_that(
  desc = "expression output",
  code = {
    set.seed(123)
    p_sub <- ggpiestats(
      data = ggplot2::msleep,
      x = conservation,
      y = vore,
      k = 4,
      output = "subtitle"
    )

    set.seed(123)
    stats_output <- suppressWarnings(statsExpressions::contingency_table(
      data = ggplot2::msleep,
      x = conservation,
      y = vore,
      k = 4
    ))$expression[[1]]

    expect_equal(p_sub, stats_output)
  }
)
