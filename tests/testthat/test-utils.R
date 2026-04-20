# .grouped_list -----------------------------------------------------

test_that(
  ".grouped_list works with non-syntactic group names",
  {
    set.seed(123)
    expect_snapshot({
      sleep %>%
        rename("my non-syntactic name" = group) %>%
        .grouped_list(grouping.var = `my non-syntactic name`) %>%
        str()
    })
  }
)

test_that(
  ".grouped_list preserves original order of grouping variable (issue #792)",
  {
    df <- tibble::tibble(
      grp = c(rep("c", 5L), rep("a", 5L), rep("b", 5L)),
      val = 1:15
    )
    result <- .grouped_list(df, grouping.var = grp)
    expect_identical(result$title, c("c", "a", "b"))
  }
)

test_that(
  ".grouped_list works with numeric grouping variable",
  {
    df <- tibble::tibble(grp = c(2L, 2L, 1L, 1L, 3L, 3L), val = 1:6)
    result <- .grouped_list(df, grouping.var = grp)
    expect_identical(result$title, c("2", "1", "3"))
  }
)

# .is_palette_sufficient ------------------------------------

test_that(
  ".is_palette_sufficient is working",
  {
    expect_no_condition(.is_palette_sufficient("RColorBrewer", "Dark2", 2L))

    withr::local_options(list(warn = 0L))
    expect_snapshot(.is_palette_sufficient("RColorBrewer", "Dark2", 20L))
  }
)

# .eval_f ------------------------------------

test_that(
  ".eval_f works as expected",
  {
    f <- function() stop("Not working", call. = FALSE)
    expect_null(.eval_f(f))
  }
)
