test_that(
  desc = "grouped_ggpiestats produces error when grouping variable not provided",
  code = {
    expect_snapshot_error(grouped_ggpiestats(mtcars, x = cyl))
  }
)

test_that(
  desc = "grouped_ggpiestats works",
  code = {
    skip_if(getRversion() < "4.1")


    set.seed(123)
    vdiffr::expect_doppelganger(
      title = "grouped_ggpiestats with one-way table",
      fig = grouped_ggpiestats(
        data = mtcars,
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
    vdiffr::expect_doppelganger(
      title = "grouped_ggpiestats with two-way table",
      fig = grouped_ggpiestats(
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
    skip_if(getRversion() < "4.1")


    df <- data.frame(
      dataset = c("a", "b", "c", "c", "c", "c"),
      measurement = c("old", "old", "old", "old", "new", "new"),
      flag = c("no", "no", "yes", "no", "yes", "no"),
      count = c(6, 8, 8, 62, 6, 33)
    )

    set.seed(123)
    vdiffr::expect_doppelganger(
      title = "common legend when levels are dropped",
      fig = grouped_ggpiestats(
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

# expression output --------------------

test_that(
  desc = "expression output is as expected",
  code = {
    set.seed(123)
    grouped_expr <- grouped_ggpiestats(
      mtcars,
      grouping.var = am,
      x = cyl,
      output = "subtitle"
    )

    set.seed(123)
    base_expr <- ggpiestats(dplyr::filter(mtcars, am == "0"), cyl, output = "subtitle")

    expect_equal(grouped_expr$`0`, base_expr)
  }
)
