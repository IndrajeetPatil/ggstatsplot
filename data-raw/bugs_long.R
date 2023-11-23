data("bugs", package = "jmv")

bugs_long <- bugs |>
  tibble::as_tibble() |>
  tidyr::gather(key = "condition", value = "desire", LDLF:HDHF)

# all column names in lower case
names(bugs_long) <- tolower(names(bugs_long))

# saving the data
save(bugs_long, file = file.path("data", "bugs_long.rdata"))
