# .grouped_list -----------------------------------------------------

test_that(
  desc = ".grouped_list works with non-syntactic group names",
  code = {
    set.seed(123)
    expect_snapshot({
      sleep %>%
        rename("my non-syntactic name" = group) %>%
        .grouped_list(grouping.var = `my non-syntactic name`) %>%
        str()
    })
  }
)

# .is_palette_sufficient ------------------------------------

test_that(
  desc = ".is_palette_sufficient is working",
  code = {
    expect_no_condition(.is_palette_sufficient("RColorBrewer", "Dark2", 2L))
    expect_snapshot(.is_palette_sufficient("RColorBrewer", "Dark2", 20L))
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
