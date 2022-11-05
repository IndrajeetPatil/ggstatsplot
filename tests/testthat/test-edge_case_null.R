# ggbetweenstats --------------------------------------------

test_that(
  desc = "ggbetweenstats - edge case NULL",
  code = {
    df <- data.frame(
      x = c(rep(1, 50), rep(2, 40), rep(3, 10)),
      y = rnorm(100),
      g = c(rep("a", 49), rep("b", 51))
    )

    df_a <- dplyr::filter(df, g == "a")
    df_b <- dplyr::filter(df, g == "b")

    expect_null(ggbetweenstats(df_a, x, y) %>% extract_subtitle())
    expect_null(ggbetweenstats(df_a, x, y) %>% extract_caption())
    expect_null(ggbetweenstats(df_b, x, y) %>% extract_subtitle())
  }
)

# ggwithinstats --------------------------------------------

test_that(
  desc = " ggwithinstats - edge case NULL",
  code = {
    skip_if_not_installed("afex")

    df <- data.frame(
      x = c(rep(1, 50), rep(2, 40), rep(3, 10)),
      y = rnorm(100),
      g = c(rep("a", 49), rep("b", 51))
    )

    df_a <- dplyr::filter(df, g == "a")
    df_b <- dplyr::filter(df, g == "b")

    expect_null(ggwithinstats(df_a, x, y) %>% extract_subtitle())
    expect_null(ggwithinstats(df_a, x, y) %>% extract_caption())
    expect_null(ggwithinstats(df_b, x, y) %>% extract_subtitle())
  }
)

# gghistostats --------------------------------------------

test_that(
  desc = "gghistostats - edge case NULL",
  code = {
    df <- data.frame(x = 1, y = "a")

    expect_null(gghistostats(df, x) %>% extract_subtitle())
    expect_null(gghistostats(df, x) %>% extract_caption())
  }
)

# ggdotplotstats --------------------------------------------

test_that(
  desc = "ggdotplotstats - edge case NULL",
  code = {
    df <- data.frame(x = 1, y = "a")

    expect_null(ggdotplotstats(df, x, y) %>% extract_subtitle())
    expect_null(ggdotplotstats(df, x, y) %>% extract_caption())
  }
)

# ggpiestats ---------------------------------------------------------

test_that(
  desc = "ggpiestats - edge case NULL",
  code = {
    df <- data.frame(x = "one", y = "one")

    expect_null(ggpiestats(df, x) %>% extract_subtitle())
    expect_null(ggpiestats(df, x) %>% extract_caption())
    expect_null(ggpiestats(df, x, y) %>% extract_subtitle())
    expect_null(ggpiestats(df, x, y) %>% extract_caption())
  }
)

# gggbarstats ---------------------------------------------------------

test_that(
  desc = "ggbarstats - edge case NULL",
  code = {
    df <- data.frame(x = "one", y = "one")

    expect_null(ggbarstats(df, x, y) %>% extract_subtitle())
    expect_null(ggbarstats(df, x, y) %>% extract_caption())
  }
)

# ggscatterstats --------------------------------------------

# Not relevant
