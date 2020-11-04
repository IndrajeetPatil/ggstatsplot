testthat::test_that(
  desc = "grouped_ggbarstats works",
  code = {
    testthat::skip_on_cran()

    # --------------------- without counts -----------------------------------

    # creating a smaller dataframe
    mpg_short <- dplyr::filter(.data = ggplot2::mpg, drv %in% c("4", "f"))

    ## expecting error message
    testthat::expect_error(ggstatsplot::grouped_ggbarstats(
      data = mpg_short,
      x = cyl,
      grouping.var = class,
      messages = FALSE
    ))

    testthat::expect_error(ggstatsplot::grouped_ggbarstats(
      data = mpg_short,
      x = cyl,
      messages = FALSE
    ))

    testthat::expect_s3_class(
      ggstatsplot::grouped_ggbarstats(
        data = mpg_short,
        x = cyl,
        y = class,
        grouping.var = class,
        messages = FALSE
      ),
      "ggplot"
    )

    # when arguments are entered as bare expressions
    set.seed(123)
    testthat::expect_true(inherits(suppressWarnings(
      ggstatsplot::grouped_ggbarstats(
        data = mpg_short,
        x = "cyl",
        y = class,
        grouping.var = drv,
        x.axis.orientation = "horizontal",
        messages = FALSE
      )
    ),
    what = "gg"
    ))

    # when arguments are entered as character
    set.seed(123)
    testthat::expect_true(inherits(suppressWarnings(
      ggstatsplot::grouped_ggbarstats(
        data = mpg_short,
        x = cyl,
        y = "class",
        grouping.var = "drv",
        x.axis.orientation = "slant",
        messages = FALSE
      )
    ),
    what = "gg"
    ))

    # --------------------- with counts -----------------------------------

    # when arguments are entered as bare expressions
    set.seed(123)
    testthat::expect_true(inherits(suppressWarnings(
      ggstatsplot::grouped_ggbarstats(
        data = as.data.frame(Titanic),
        grouping.var = Class,
        x = Sex,
        y = Survived,
        counts = Freq,
        messages = FALSE
      )
    ),
    what = "gg"
    ))

    # when arguments are entered as character
    set.seed(123)
    testthat::expect_true(inherits(suppressWarnings(
      ggstatsplot::grouped_ggbarstats(
        data = as.data.frame(Titanic),
        grouping.var = "Class",
        x = "Sex",
        y = "Survived",
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
      suppressWarnings(ggstatsplot::grouped_ggbarstats(
        data = dplyr::sample_frac(tbl = forcats::gss_cat, size = 0.1),
        x = relig,
        y = marital,
        grouping.var = "race",
        output = "subtitle",
        k = 3,
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
        k = 3,
        messages = FALSE
      ))

    # checking subtitle
    testthat::expect_equal(ls_results$Other, sexpr_results)
  }
)
