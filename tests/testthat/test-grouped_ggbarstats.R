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

# expression output --------------------

test_that(
  desc = "expression output is as expected",
  code = {
    set.seed(123)
    grouped_expr <- grouped_ggbarstats(
      mtcars,
      grouping.var = am,
      x = cyl,
      y = vs,
      output = "subtitle"
    )

    set.seed(123)
    base_expr <- ggbarstats(dplyr::filter(mtcars, am == "0"), cyl, vs, output = "subtitle")

    expect_equal(grouped_expr$`0`, base_expr)
  }
)
