# defining global variables and functions to appease R CMD Check

utils::globalVariables(
  names = c(
    ".",
    "..count..",
    "..density..",
    "N",
    "conf.high",
    "conf.low",
    "counts",
    "df.error",
    "df1",
    "df2",
    "estimate",
    "group1",
    "group2",
    "isanoutlier",
    "label",
    "median",
    "n_obs",
    "p.value",
    "parameter",
    "perc",
    "rowid",
    "sd",
    "significance",
    "statistic",
    "std.error",
    "term",
    "df",
    ".label",
    ".counts",
    ".p.label"
  ),
  package = "ggstatsplot",
  add = FALSE
)
