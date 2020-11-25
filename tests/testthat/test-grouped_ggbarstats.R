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
      grouping.var = class
    ))

    testthat::expect_error(ggstatsplot::grouped_ggbarstats(
      data = mpg_short,
      x = cyl
    ))

    testthat::expect_s3_class(
      ggstatsplot::grouped_ggbarstats(
        data = mpg_short,
        x = cyl,
        y = class,
        grouping.var = class
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
        x.axis.orientation = "horizontal"
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
        x.axis.orientation = "slant"
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
        counts = Freq
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
        counts = "Freq"
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

    set.seed(123)
    df <- dplyr::sample_frac(tbl = forcats::gss_cat, size = 0.1) %>%
      dplyr::mutate_if(., is.factor, droplevels)


    # should output a list of length 3
    set.seed(123)
    ls_results <-
      suppressWarnings(ggstatsplot::grouped_ggbarstats(
        data = df,
        x = relig,
        y = marital,
        grouping.var = race,
        output = "subtitle"
      ))

    set.seed(123)
    sexpr_results <-
      suppressWarnings(statsExpressions::expr_contingency_tab(
        data = dplyr::filter(df, race == "Other") %>%
          dplyr::mutate_if(., is.factor, droplevels),
        x = relig,
        y = marital,
        output = "subtitle"
      ))

    # checking subtitle
    testthat::expect_equal(ls_results$Other, sexpr_results)
  }
)
