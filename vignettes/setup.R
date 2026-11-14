# to pretty-print all columns in the output tibble
options(
  tibble.width = Inf,
  pillar.bold = TRUE,
  pillar.neg = TRUE,
  pillar.subtle_num = TRUE,
  pillar.min_chars = Inf
)

knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  warning = FALSE,
  message = FALSE,
  out.width = "100%",
  dpi = 300,
  dev = "png"
)

# local library
library(ggstatsplot)

# for reproducibility
set.seed(123)

# attach suggested packages, or skip evaluating code if any is unavailable
load_suggested <- function(pkgs) {
  if (all(purrr::map_lgl(pkgs, requireNamespace, quietly = TRUE))) {
    purrr::walk(pkgs, library, character.only = TRUE)
  } else {
    knitr::opts_chunk$set(eval = FALSE)
  }
}
