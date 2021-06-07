# contingency tab (with counts) ----------------------------------------------

test_that(
  desc = "checking labels with counts",
  code = {
    skip_on_cran()

    # y variable is not optional for `ggbarstats`
    expect_error(
      ggbarstats(
        data = as.data.frame(Titanic),
        x = Sex
      )
    )

    # plot
    set.seed(123)
    p <-
      ggbarstats(
        data = as.data.frame(Titanic),
        x = Sex,
        y = Survived,
        counts = "Freq",
        perc.k = 2,
        xlab = "Passenger sex",
        ylab = "proportion",
        label.separator = "\n",
        bf.message = FALSE
      )

    # build plot
    pb <- ggplot2::ggplot_build(p)

    # subtitle
    set.seed(123)
    p_subtitle <-
      statsExpressions::contingency_table(
        data = as.data.frame(Titanic),
        x = "Sex",
        y = "Survived",
        counts = Freq
      )$expression[[1]]

    # check data
    set.seed(123)
    expect_snapshot(pb$data)
    expect_snapshot(within(pb$plot$labels, rm(subtitle)))

    # checking plot labels
    expect_identical(pb$plot$labels$subtitle, p_subtitle)
  }
)

# aesthetic modifications --------------------------------------------------

test_that(
  desc = "aesthetic modifications",
  code = {
    skip_on_cran()

    # plot
    set.seed(123)
    p <-
      suppressWarnings(ggbarstats(
        data = mtcars,
        x = vs,
        y = "cyl",
        label = "both",
        package = "wesanderson",
        palette = "Royal2",
        legend.title = "Engine"
      ))

    p1 <-
      suppressWarnings(ggbarstats(
        data = mtcars,
        x = vs,
        y = cyl,
        label = "counts",
        bf.message = FALSE
      ))

    # build plot
    pb <- ggplot2::ggplot_build(p)
    pb1 <- ggplot2::ggplot_build(p1)

    # check data
    set.seed(123)
    expect_snapshot(list(pb$data, pb1$data))

    # checking layered data
    expect_identical(pb$plot$guides$fill$title, "Engine")
  }
)

# dropped factor levels --------------------------------------------------

test_that(
  desc = "dropped factor levels",
  code = {
    skip_on_cran()

    # dropped level dataset
    mtcars_small <- dplyr::filter(mtcars, am == "0")

    set.seed(123)
    p <-
      ggbarstats(
        data = mtcars_small,
        x = cyl,
        y = am,
        results.subtitle = NULL
      )

    pb <- ggplot2::ggplot_build(p)

    # check data
    set.seed(123)
    expect_snapshot(pb$data)
    expect_snapshot(pb$plot$labels)
  }
)

# expression output --------------------------------------------------

test_that(
  desc = "expression output",
  code = {
    skip_on_cran()

    set.seed(123)
    df <- dplyr::sample_frac(tbl = forcats::gss_cat, size = 0.1) %>%
      dplyr::mutate_if(., is.factor, droplevels)

    # subtitle output
    set.seed(123)
    p_sub <-
      ggbarstats(
        data = df,
        x = race,
        y = marital,
        output = "subtitle",
        k = 4
      )

    set.seed(123)
    stats_output <-
      statsExpressions::contingency_table(
        data = df,
        x = race,
        y = marital,
        k = 4
      )$expression[[1]]

    # caption output
    set.seed(123)
    p_cap <-
      ggbarstats(
        data = df,
        x = race,
        y = "marital",
        type = "bayes",
        output = "subtitle",
        k = 4
      )

    # caption output
    set.seed(123)
    p_cap_exp <-
      statsExpressions::contingency_table(
        data = df,
        x = "race",
        y = marital,
        type = "bayes",
        k = 4
      )$expression[[1]]

    # tests
    expect_identical(p_sub, stats_output)
    expect_identical(p_cap, p_cap_exp)
  }
)
