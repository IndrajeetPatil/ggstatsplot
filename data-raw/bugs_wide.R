data("bugs", package = "jmv")

bugs_wide <- tibble::as_tibble(bugs)

names(bugs_wide) <- tolower(names(bugs_wide))

save(bugs_wide, file = file.path("data", "bugs_wide.rdata"))
