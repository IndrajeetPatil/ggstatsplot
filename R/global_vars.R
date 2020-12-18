# defining global variables and functions to appease R CMD Check

utils::globalVariables(
  names = c(
    ".",
    "..count..",
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
    ".p.label",
    "trimmed.mean"
  ),
  package = "ggstatsplot",
  add = FALSE
)
