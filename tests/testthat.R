library(testthat)
library(ggstatsplot)

# suppress printing environment name (noisy)
invisible({
  loadNamespace("statsExpressions")
  loadNamespace("vdiffr")
})

test_check("ggstatsplot")
