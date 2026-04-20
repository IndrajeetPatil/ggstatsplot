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
  ".grouped_list drops NA rows in grouping variable",
  {
    df <- tibble::tibble(grp = c("a", NA, "b", "a"), val = 1:4)
    result <- .grouped_list(df, grouping.var = grp)
    expect_identical(result$title, c("a", "b"))
    expect_identical(nrow(result$data[[1L]]) + nrow(result$data[[2L]]), 3L)
  }
)

# group ordering depends on column type (issue #792)
patrick::with_parameters_test_that(
  ".grouped_list group order: {type}",
  {
    df <- tibble::tibble(grp = grp_vals, val = 1:9)
    expect_identical(.grouped_list(df, grouping.var = grp)$title, expected)
  },
  .cases = tibble::tibble(
    type          = c("character (appearance order)", "integer (sorted)", "factor (level order)"),
    grp_vals      = list(
      c(rep("3", 3L), rep("1", 3L), rep("2", 3L)),
      c(rep(3L,  3L), rep(1L,  3L), rep(2L,  3L)),
      factor(c(rep("3", 3L), rep("1", 3L), rep("2", 3L)), levels = c("3", "1", "2"))
    ),
    expected      = list(
      c("3", "1", "2"),
      c("1", "2", "3"),
      c("3", "1", "2")
    )
  )
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
