if (identical(Sys.getenv("NOT_CRAN"), "true")) {
  spelling::spell_check_test(vignettes = TRUE, error = FALSE)
}
