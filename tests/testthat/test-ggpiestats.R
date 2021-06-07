# one sample proportion test -----------------------------------------

test_that(
  desc = "checking one sample proportion test",
  code = {
    skip_on_cran()

    # creating the plot
    set.seed(123)
    p <-
      ggpiestats(
        data = ggplot2::msleep,
        x = vore,
        title = "mammalian sleep",
        legend.title = "vore",
        caption = "From ggplot2 package",
        perc.k = 2,
        label = "both"
      )

    # built plot
    pb <- ggplot2::ggplot_build(p)

    # subtitle used
    set.seed(123)
    p_subtitle <-
      statsExpressions::contingency_table(
        data = ggplot2::msleep,
        x = "vore"
      )$expression[[1]]

    # caption
    set.seed(123)
    p_cap <-
      statsExpressions::contingency_table(
        data = ggplot2::msleep,
        type = "bayes",
        x = "vore",
        top.text = "From ggplot2 package"
      )$expression[[1]]

    # check data
    set.seed(123)
    expect_snapshot(pb$data)

    # checking plot labels
    expect_identical(pb$plot$labels$subtitle, p_subtitle)
    expect_identical(pb$plot$labels$title, "mammalian sleep")
    expect_identical(pb$plot$labels$caption, p_cap)
    expect_null(pb$plot$labels$x, NULL)
    expect_null(pb$plot$labels$y, NULL)
    expect_identical(pb$plot$plot_env$legend.title, "vore")
  }
)

# contingency tab ---------------------------------------------------------

test_that(
  desc = "checking labels with contingency tab",
  code = {
    skip_on_cran()

    # creating the plot
    set.seed(123)
    p <-
      suppressWarnings(
        ggpiestats(
          data = mtcars,
          x = "am",
          y = "cyl",
          perc.k = 2,
          package = "wesanderson",
          palette = "Royal2",
          ggtheme = ggplot2::theme_bw(),
          label = "counts",
          legend.title = "transmission"
        )
      )

    # dropped level dataset
    mtcars_small <- dplyr::filter(mtcars, am == "0")

    # plot
    p1 <-
      ggpiestats(
        data = mtcars_small,
        x = cyl,
        y = am
      )

    expect_s3_class(
      suppressWarnings(ggpiestats(
        data = mtcars_small,
        y = cyl,
        x = am
      )),
      "ggplot"
    )

    # build plot
    pb <- ggplot2::ggplot_build(p)
    pb1 <- ggplot2::ggplot_build(p1)

    # subtitle used
    set.seed(123)
    p_subtitle <-
      suppressWarnings(statsExpressions::contingency_table(
        data = mtcars,
        x = "am",
        y = "cyl"
      )$expression[[1]])

    # subtitle used
    set.seed(123)
    p_cap <-
      suppressWarnings(statsExpressions::contingency_table(
        data = mtcars,
        x = "am",
        y = "cyl",
        type = "bayes"
      )$expression[[1]])

    # check data
    set.seed(123)
    expect_snapshot(list(pb$data, pb1$data))
    expect_snapshot(within(pb$plot$labels, rm(subtitle, caption)))

    # checking plot labels
    expect_identical(pb$plot$labels$subtitle, p_subtitle)
    expect_identical(pb$plot$labels$caption, p_cap)
  }
)

# contingency tab (with counts) ----------------------------------------------

test_that(
  desc = "checking labels with counts",
  code = {
    skip_on_cran()

    # plot
    set.seed(123)
    p <-
      ggpiestats(
        data = as.data.frame(Titanic),
        x = Sex,
        y = Survived,
        bf.message = FALSE,
        counts = "Freq",
        perc.k = 2,
        legend.title = NULL,
        ggtheme = ggplot2::theme_minimal()
      )

    # subtitle
    set.seed(123)
    p_subtitle <-
      statsExpressions::contingency_table(
        data = as.data.frame(Titanic),
        x = Sex,
        y = Survived,
        counts = Freq
      )$expression[[1]]

    # build the plot
    pb <- ggplot2::ggplot_build(p)

    # checking data
    expect_snapshot(pb$data)
    expect_snapshot(within(pb$plot$labels, rm(subtitle)))

    # checking plot labels
    expect_identical(pb$plot$labels$subtitle, p_subtitle)
  }
)

# mcnemar test ---------------------------------------------------------

test_that(
  desc = "checking labels with contingency tab (paired)",
  code = {
    skip_on_cran()

    # data
    set.seed(123)
    survey.data <- data.frame(
      `1st survey` = c("Approve", "Approve", "Disapprove", "Disapprove"),
      `2nd survey` = c("Approve", "Disapprove", "Approve", "Disapprove"),
      `Counts` = c(794, 150, 86, 570),
      check.names = FALSE
    )

    # plot
    set.seed(123)
    p <-
      ggpiestats(
        data = survey.data,
        x = `1st survey`,
        y = `2nd survey`,
        counts = Counts,
        paired = TRUE,
        conf.level = 0.90
      )

    # build the plot
    pb <- ggplot2::ggplot_build(p)

    # subtitle
    set.seed(123)
    p_subtitle <-
      statsExpressions::contingency_table(
        data = survey.data,
        x = `1st survey`,
        y = `2nd survey`,
        counts = Counts,
        paired = TRUE,
        conf.level = 0.90
      )$expression[[1]]

    # checking plot labels
    expect_snapshot(within(pb$plot$labels, rm(subtitle)))
    expect_identical(pb$plot$labels$subtitle, p_subtitle)
    expect_snapshot(pb$data[[3]])
  }
)

# repelling labels -------------------------------------------------------------

test_that(
  desc = "repelling labels",
  code = {
    skip_on_cran()
    set.seed(123)

    # data
    df <-
      structure(list(
        epoch = structure(c(1L, 2L, 1L, 2L, 1L, 2L, 1L, 2L),
          .Label = c("Before", "After"),
          class = "factor"
        ),
        mode = structure(c(1L, 1L, 2L, 2L, 3L, 3L, 4L, 4L),
          .Label = c("A", "P", "C", "T"), class = "factor"
        ),
        counts = c(
          30916L, 21117L, 7676L, 1962L, 1663L, 462L, 7221L,
          197L
        ),
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
      class = c(
        "tbl_df",
        "tbl", "data.frame"
      )
      )

    # plot
    set.seed(123)
    p <-
      ggpiestats(
        df,
        mode,
        epoch,
        counts = counts,
        label.repel = TRUE,
        results.subtitle = FALSE,
        proportion.test = FALSE
      )

    # build plot
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
      ggpiestats(
        data = df,
        x = race,
        y = marital,
        k = 4,
        output = "subtitle"
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
      ggpiestats(
        data = df,
        x = race,
        y = "marital",
        type = "bayes",
        k = 4,
        output = "subtitle"
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
