testthat::test_that(
  desc = "grouped_ggpiestats works",
  code = {
    testthat::skip_on_cran()

    #--------------------- only x variable -------------------------------

    ## expecting error
    testthat::expect_error(
      ggstatsplot::grouped_ggpiestats(
        data = mtcars,
        x = cyl,
        messages = FALSE
      )
    )

    ## without counts

    # when arguments are entered as bare expressions
    set.seed(123)
    testthat::expect_true(inherits(suppressWarnings(
      ggstatsplot::grouped_ggpiestats(
        data = mtcars,
        grouping.var = am,
        x = "cyl",
        results.subtitle = FALSE,
        messages = FALSE
      )
    ),
    what = "gg"
    ))

    #------------------ both x and y variables ------------------

    ## without counts

    # creating a smaller dataframe
    mpg_short <-
      ggplot2::mpg %>%
      dplyr::filter(
        .data = .,
        drv %in% c("4", "f"),
        class %in% c("suv", "midsize"),
        trans %in% c("auto(l4)", "auto(l5)")
      )

    # when arguments are entered as bare expressions
    set.seed(123)
    testthat::expect_true(inherits(suppressWarnings(
      ggstatsplot::grouped_ggpiestats(
        data = mpg_short,
        x = cyl,
        y = "class",
        results.subtitle = FALSE,
        grouping.var = drv,
        label.repel = TRUE,
        messages = FALSE
      )
    ),
    what = "gg"
    ))

    ## with counts

    # when arguments are entered as bare expressions
    set.seed(123)
    testthat::expect_true(inherits(suppressWarnings(
      ggstatsplot::grouped_ggpiestats(
        data = as.data.frame(Titanic),
        grouping.var = Class,
        x = Sex,
        results.subtitle = FALSE,
        y = Survived,
        counts = "Freq",
        messages = FALSE
      )
    ),
    what = "gg"
    ))
  }
)

# subtitle output --------------------------------------------------

testthat::test_that(
  desc = "subtitle output",
  code = {
    testthat::skip_on_cran()

    # should output a list of length 3
    set.seed(123)
    ls_results <-
      suppressWarnings(ggstatsplot::grouped_ggpiestats(
        data = dplyr::sample_frac(tbl = forcats::gss_cat, size = 0.1),
        x = relig,
        y = marital,
        grouping.var = race,
        output = "subtitle",
        messages = FALSE
      ))

    set.seed(123)
    sexpr_results <-
      suppressWarnings(statsExpressions::expr_contingency_tab(
        data = dplyr::sample_frac(tbl = forcats::gss_cat, size = 0.1) %>%
          dplyr::filter(race == "Other"),
        x = relig,
        y = marital,
        output = "subtitle",
        messages = FALSE
      ))

    # checking subtitle
    testthat::expect_equal(ls_results$Other, sexpr_results)
  }
)
