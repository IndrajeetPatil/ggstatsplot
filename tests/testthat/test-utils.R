# .grouped_list -----------------------------------------------------

test_that(
  desc = ".grouped_list works",
  code = {
    set.seed(123)

    expect_snapshot(glimpse(ggstatsplot:::.grouped_list(sleep, grouping.var = group)))
    expect_snapshot(glimpse(ggstatsplot:::.grouped_list(sleep)))
  }
)

test_that(
  desc = ".grouped_list works with non-syntactic group names",
  code = {
    set.seed(123)
    expect_snapshot({
      sleep %>%
        rename("my non-syntactic name" = group) %>%
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
