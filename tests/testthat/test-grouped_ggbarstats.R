context("grouped_ggbarstats")

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
      main = cyl,
      grouping.var = class,
      messages = TRUE
    ))

    testthat::expect_error(ggstatsplot::grouped_ggbarstats(
      data = mpg_short,
      main = cyl,
      messages = TRUE
    ))

    testthat::expect_output(
      ggstatsplot::grouped_ggbarstats(
        data = mpg_short,
        main = cyl,
        condition = class,
        grouping.var = class,
        messages = TRUE
      )
    )

    # when arguments are entered as bare expressions
    set.seed(123)
    testthat::expect_true(inherits(suppressWarnings(
      ggstatsplot::grouped_ggbarstats(
        data = mpg_short,
        main = "cyl",
        condition = class,
        grouping.var = drv,
        x.axis.orientation = "horizontal",
        messages = TRUE
      )
    ),
    what = "gg"
    ))

    # when arguments are entered as character
    set.seed(123)
    testthat::expect_true(inherits(suppressWarnings(
      ggstatsplot::grouped_ggbarstats(
        data = mpg_short,
        main = cyl,
        condition = "class",
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
        main = Sex,
        condition = Survived,
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
        main = "Sex",
        condition = "Survived",
        counts = "Freq",
        messages = FALSE
      )
    ),
    what = "gg"
    ))
  }
)

# subtitle return --------------------------------------------------

testthat::test_that(
  desc = "subtitle return",
  code = {
    testthat::skip_on_cran()

    # should return a list of length 3
    set.seed(123)
    ls_results <- suppressWarnings(ggstatsplot::grouped_ggbarstats(
      data = dplyr::sample_frac(tbl = forcats::gss_cat, size = 0.1),
      main = relig,
      condition = marital,
      grouping.var = race,
      return = "subtitle",
      results.subtitle = FALSE,
      bar.proptest = FALSE,
      messages = FALSE
    ))

    # tests
    testthat::expect_equal(length(ls_results), 3L)
    testthat::expect_null(ls_results[[1]], NULL)
    testthat::expect_null(ls_results[[2]], NULL)
    testthat::expect_null(ls_results[[3]], NULL)

    # checking results
    set.seed(123)
    results_ls <- suppressWarnings(ggstatsplot::grouped_ggbarstats(
      data = as.data.frame(HairEyeColor),
      main = Hair,
      condition = Eye,
      counts = "Freq",
      grouping.var = Sex,
      return = "subtitle",
      messages = FALSE
    ))

    # checking subtitle
    testthat::expect_identical(
      results_ls$Male,
      ggplot2::expr(
        paste(
          NULL,
          chi["Pearson"]^2,
          "(",
          "9",
          ") = ",
          "41.28",
          ", ",
          italic("p"),
          " = ",
          "< 0.001",
          ", ",
          widehat(italic("V")["Cramer"]),
          " = ",
          "0.22",
          ", CI"["95%"],
          " [",
          "0.14",
          ", ",
          "0.26",
          "]",
          ", ",
          italic("n")["obs"],
          " = ",
          279L
        )
      )
    )

    testthat::expect_identical(
      results_ls$Female,
      ggplot2::expr(
        paste(
          NULL,
          chi["Pearson"]^2,
          "(",
          "9",
          ") = ",
          "106.66",
          ", ",
          italic("p"),
          " = ",
          "< 0.001",
          ", ",
          widehat(italic("V")["Cramer"]),
          " = ",
          "0.34",
          ", CI"["95%"],
          " [",
          "0.28",
          ", ",
          "0.38",
          "]",
          ", ",
          italic("n")["obs"],
          " = ",
          313L
        )
      )
    )
  }
)

# checking if results coincide with base version -----------------------------

testthat::test_that(
  desc = "checking if results coincide with base version",
  code = {
    testthat::skip_on_cran()

    # creating new datasets from the existing one
    mtcars2 <- dplyr::mutate(mtcars, grp = "1")
    mtcars3 <- dplyr::filter(mtcars2, cyl != "8")

    set.seed(123)
    p1 <-
      suppressWarnings(
        ggbarstats(
          data = mtcars,
          main = "am",
          condition = cyl,
          messages = FALSE,
          return = "subtitle"
        )
      )

    set.seed(123)
    p2 <-
      suppressWarnings(ggbarstats(
        data = mtcars3,
        x = "am",
        y = cyl,
        messages = FALSE,
        return = "subtitle"
      ))

    set.seed(123)
    p3 <-
      suppressWarnings(grouped_ggbarstats(
        data = mtcars2,
        main = am,
        y = "cyl",
        grouping.var = grp,
        messages = FALSE,
        return = "subtitle"
      ))

    set.seed(123)
    p4 <-
      suppressWarnings(grouped_ggbarstats(
        data = mtcars3,
        x = "am",
        condition = cyl,
        grouping.var = "grp",
        messages = FALSE,
        return = "subtitle"
      ))

    # testing if grouped and base versions results are same
    testthat::expect_identical(p1, p3$`1`)
    testthat::expect_identical(p2, p4$`1`)
  }
)
