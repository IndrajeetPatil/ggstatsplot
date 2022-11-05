# .grouped_list -----------------------------------------------------

test_that(
  desc = ".grouped_list works",
  code = {
    set.seed(123)

    # creating lists
    df1 <- ggstatsplot:::.grouped_list(ggplot2::msleep, grouping.var = vore)
    df2 <- ggstatsplot:::.grouped_list(ggplot2::msleep, grouping.var = NULL)

    # testing lengths of lists
    expect_snapshot(names(df1))
    expect_identical(ggplot2::msleep, df2)
  }
)

test_that(
  desc = ".grouped_list works with non-syntactic group names",
  code = {
    set.seed(123)
    expect_snapshot({
      ggplot2::msleep %>%
        rename("my non-syntactic name" = vore) %>%
        ggstatsplot:::.grouped_list(grouping.var = `my non-syntactic name`) %>%
        tibble::tbl_sum()
    })
  }
)

# .palette_message ------------------------------------

test_that(
  desc = ".palette_message is working",
  code = {
    expect_snapshot_warning(
      ggstatsplot:::.palette_message(
        package = "RColorBrewer",
        palette = "Dark2",
        min_length = 20L
      )
    )
  }
)

# .eval_f ------------------------------------

test_that(
  desc = ".eval_f works as expected",
  code = {
    f <- function() stop("Not working", call. = FALSE)
    expect_null(.eval_f(f))
  }
)
