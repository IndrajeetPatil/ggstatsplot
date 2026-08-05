# .grouped_list -----------------------------------------------------

test_that(".grouped_list works with non-syntactic group names", {
  set.seed(123)
  expect_snapshot({
    sleep |>
      rename("my non-syntactic name" = group) |>
      .grouped_list(grouping.var = `my non-syntactic name`) |>
      str()
  })
})

test_that(".grouped_list drops NA rows in grouping variable", {
  df <- tibble::tibble(grp = c("a", NA, "b", "a"), val = 1:4)
  result <- .grouped_list(df, grouping.var = grp)
  expect_identical(result$title, c("a", "b"))
  expect_identical(nrow(result$data[[1L]]) + nrow(result$data[[2L]]), 3L)
})

# group ordering depends on column type (issue #792)
if (requireNamespace("patrick", quietly = TRUE)) {
  patrick::with_parameters_test_that(
    ".grouped_list group order: {type}",
    {
      df <- tibble::tibble(grp = grp_vals, val = 1:9)
      expect_identical(.grouped_list(df, grouping.var = grp)$title, expected)
    },
    .cases = tibble::tibble(
      type = c(
        "character (appearance order)",
        "integer (sorted)",
        "factor (level order)"
      ),
      grp_vals = list(
        c(rep("3", 3L), rep("1", 3L), rep("2", 3L)),
        c(rep(3L, 3L), rep(1L, 3L), rep(2L, 3L)),
        factor(
          c(rep("3", 3L), rep("1", 3L), rep("2", 3L)),
          levels = c("3", "1", "2")
        )
      ),
      expected = list(
        c("3", "1", "2"),
        c("1", "2", "3"),
        c("3", "1", "2")
      )
    )
  )
}

# .cat_counter -----------------------------------------------------

test_that(".cat_counter preserves factor order and returns no empty groups", {
  df <- tibble::tibble(
    y = factor(
      c("g1", "g1", "g1", "g2", "g2"),
      levels = c("g2", "g1", "unused")
    ),
    x = factor(c("a", "a", "b", "a", "b"), levels = c("a", "b", "unused"))
  )

  result <- .cat_counter(df, x, y)
  grouped_result <- .cat_counter(dplyr::group_by(df, x), x, y)

  expect_false(dplyr::is_grouped_df(result))
  expect_identical(grouped_result, result)
  expect_identical(as.character(result$y), c("g2", "g1", "g2", "g1"))
  expect_identical(as.character(result$x), c("b", "b", "a", "a"))
  expect_identical(result$counts, c(1L, 1L, 1L, 2L))
  expect_identical(
    result$perc,
    c(1 / 2 * 100, 1 / 3 * 100, 1 / 2 * 100, 2 / 3 * 100)
  )
  expect_identical(levels(result$y), c("g2", "g1", "unused"))
  expect_identical(levels(result$x), c("a", "b", "unused"))
})

# .validate_palette ------------------------------------

test_that(".validate_palette warns and returns default for old-style palette", {
  result <- suppressWarnings(.validate_palette("Dark2"))
  expect_warning(.validate_palette("Dark2"), regexp = "not in the required")
  expect_identical(result, "ggthemes::gdoc")
  expect_identical(.validate_palette("ggthemes::gdoc"), "ggthemes::gdoc")
})

# .is_palette_sufficient ------------------------------------

test_that(".is_palette_sufficient is working", {
  expect_no_condition(.is_palette_sufficient("ggthemes::gdoc", 2L))

  expect_snapshot(.is_palette_sufficient("ggthemes::gdoc", 30L), error = TRUE)
})

# .eval_f ------------------------------------

test_that(".eval_f works as expected", {
  f <- function() stop("Not working", call. = FALSE)
  expect_null(.eval_f(f))
})
