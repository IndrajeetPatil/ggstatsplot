test_that(
  desc = "grouped_ggscatterstats works",
  code = {
    skip_on_cran()

    # expect error if no grouping variable is specified
    expect_error(
      grouped_ggscatterstats(
        data = iris,
        x = Sepal.Length,
        y = Petal.Width
      )
    )

    # without any labelling
    set.seed(123)
    expect_true(inherits(
      grouped_ggscatterstats(
        data = iris,
        x = Sepal.Length,
        y = Petal.Width,
        grouping.var = Species,
        results.subtitle = FALSE,
        marginal = FALSE
      ),
      what = "gg"
    ))

    # create a smaller dataset
    set.seed(123)
    df <- dplyr::sample_frac(tbl = movies_long, size = 0.25) %>%
      dplyr::filter(
        mpaa %in% c("R", "PG-13"),
        genre %in% c("Drama", "Comedy")
      )

    # both quoted
    set.seed(123)
    expect_true(inherits(
      grouped_ggscatterstats(
        data = df,
        x = length,
        y = rating,
        label.expression = "length > 150",
        label.var = "title",
        grouping.var = mpaa,
        type = "bayes",
        results.subtitle = FALSE,
        marginal = FALSE
      ),
      what = "gg"
    ))

    # both unquoted
    set.seed(123)
    expect_true(inherits(
      grouped_ggscatterstats(
        data = df,
        x = length,
        y = rating,
        label.expression = budget > 150,
        label.var = title,
        grouping.var = mpaa,
        results.subtitle = FALSE,
        marginal = FALSE
      ),
      what = "gg"
    ))

    # one quoted, one unquoted
    set.seed(123)
    expect_true(inherits(
      grouped_ggscatterstats(
        data = df,
        x = length,
        y = rating,
        label.expression = budget > 150,
        label.var = "title",
        grouping.var = mpaa,
        type = "p",
        results.subtitle = FALSE,
        marginal = FALSE
      ),
      what = "gg"
    ))

    set.seed(123)
    expect_true(inherits(
      grouped_ggscatterstats(
        data = df,
        x = "length",
        y = "rating",
        grouping.var = "mpaa",
        type = "r",
        label.expression = "budget > 150",
        label.var = title,
        results.subtitle = FALSE,
        marginal = FALSE
      ),
      what = "gg"
    ))

    # without point labelling
    set.seed(123)
    expect_true(inherits(
      grouped_ggscatterstats(
        data = df,
        x = "length",
        y = rating,
        grouping.var = mpaa,
        label.expression = "length > 150",
        type = "np",
        results.subtitle = FALSE,
        marginal = FALSE
      ),
      what = "gg"
    ))

    # labeling all points (without expression, i.e.)
    set.seed(123)
    expect_true(inherits(
      grouped_ggscatterstats(
        data = dplyr::sample_frac(tbl = df, size = 0.1),
        x = "length",
        y = rating,
        grouping.var = mpaa,
        label.var = title,
        label.expression = NULL,
        type = "np",
        results.subtitle = FALSE,
        marginal = FALSE
      ),
      what = "gg"
    ))

    # checking if ggplot component addition works
    set.seed(123)
    expect_true(inherits(
      grouped_ggscatterstats(
        data = ggplot2::msleep,
        x = sleep_total,
        y = bodywt,
        results.subtitle = FALSE,
        grouping.var = "vore",
        xlab = "total sleep",
        ylab = "body weight",
        caption = "source: ggplot2 package",
        type = "bayes",
        ggplot.component = scale_y_continuous(breaks = seq(0, 6000, 1000)),
        marginal = FALSE
      ),
      what = "gg"
    ))
  }
)

# subtitle output --------------------------------------------------

test_that(
  desc = "subtitle output",
  code = {
    skip_on_cran()

    # data
    df <- dplyr::filter(movies_long, genre %in% c("Action Drama"))

    set.seed(123)
    ls_results <-
      grouped_ggscatterstats(
        data = df,
        x = rating,
        y = "length",
        k = 3,
        conf.level = 0.99,
        grouping.var = genre,
        output = "subtitle"
      )

    set.seed(123)
    basic_results <-
      statsExpressions::corr_test(
        data = df,
        x = "rating",
        y = length,
        k = 3,
        conf.level = 0.99
      )$expression[[1]]

    # tests
    expect_equal(length(ls_results), 1L)
    expect_identical(ls_results$`Action Drama`, basic_results)
  }
)
