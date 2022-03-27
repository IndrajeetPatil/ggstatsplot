test_that(
  desc = "grouped_ggbarstats works",
  code = {


    # --------------------- without counts -----------------------------------

    # creating a smaller dataframe
    mpg_short <- dplyr::filter(ggplot2::mpg, drv %in% c("4", "f"))

    ## expecting error message
    expect_error(grouped_ggbarstats(
      data = mpg_short,
      x = cyl,
      grouping.var = class
    ))

    expect_error(grouped_ggbarstats(
      data = mpg_short,
      x = cyl
    ))

    expect_s3_class(
      grouped_ggbarstats(
        data = mpg_short,
        x = cyl,
        y = class,
        grouping.var = "class"
      ),
      "ggplot"
    )

    # when arguments are entered as bare expressions
    set.seed(123)
    expect_true(inherits(suppressWarnings(
      grouped_ggbarstats(
        data = mpg_short,
        x = cyl,
        y = class,
        grouping.var = drv
      )
    ),
    what = "gg"
    ))

    # when arguments are entered as character
    set.seed(123)
    expect_true(inherits(suppressWarnings(
      grouped_ggbarstats(
        data = mpg_short,
        x = cyl,
        y = class,
        grouping.var = drv
      )
    ),
    what = "gg"
    ))

    # --------------------- with counts -----------------------------------

    # when arguments are entered as bare expressions
    set.seed(123)
    expect_true(inherits(suppressWarnings(
      grouped_ggbarstats(
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
    expect_true(inherits(suppressWarnings(
      grouped_ggbarstats(
        data = as.data.frame(Titanic),
        grouping.var = Class,
        x = Sex,
        y = Survived,
        counts = Freq
      )
    ),
    what = "gg"
    ))
  }
)

# subtitle output --------------------------------------------------

test_that(
  desc = "subtitle output",
  code = {
    set.seed(123)
    df <- dplyr::sample_frac(forcats::gss_cat, size = 0.1) %>%
      dplyr::mutate_if(., is.factor, droplevels)


    # should output a list of length 3
    set.seed(123)
    ls_results <- suppressWarnings(grouped_ggbarstats(
      data = df,
      x = relig,
      y = marital,
      grouping.var = race,
      output = "subtitle"
    ))

    set.seed(123)
    sexpr_results <-
      suppressWarnings(statsExpressions::contingency_table(
        data = dplyr::filter(df, race == "Other") %>%
          dplyr::mutate_if(., is.factor, droplevels),
        x = relig,
        y = marital
      )$expression[[1]])

    # checking subtitle
    expect_equal(ls_results$Other, sexpr_results)
  }
)
