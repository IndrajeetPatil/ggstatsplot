# Test to ensure roxygenize() has been run on the current PR
library(tools)
library(roxygen2)

old_dir <- file.path(tempdir(), "man")
if (dir.exists(old_dir)) unlink(old_dir, recursive = TRUE)
file.copy("man", tempdir(), recursive = TRUE)
old_files <- list.files(old_dir, pattern = "\\.Rd$")
new_dir <- "man"
.Last <- function() unlink(old_dir, recursive = TRUE)

# Rd2txt() prints to its out= argument, so we'd have to compare file contents;
#   plain parse_Rd() keeps srcref info that encodes the file path, which as.character() strips.
normalize_rd <- function(rd_file) as.character(parse_Rd(rd_file))

rd_equal <- function(f1, f2) isTRUE(all.equal(normalize_rd(f1), normalize_rd(f2)))

check_roxygenize_idempotent <- function(LOCALE) {
  Sys.setlocale("LC_COLLATE", LOCALE)
  roxygenize()
  
  new_files <- list.files(new_dir, pattern = "\\.Rd$")
  
  old_not_new <- setdiff(old_files, new_files)
  if (length(old_not_new) > 0L) {
    stop("Found saved .Rd files gone from a fresh run of roxygenize(): ", toString(old_not_new))
  }
  
  new_not_old <- setdiff(new_files, old_files)
  if (length(new_not_old) > 0L) {
    stop("Found new .Rd files from a fresh run of roxygenize(): ", toString(new_not_old))
  }
    
  for (file in new_files) {
    old_file <- file.path(old_dir, file)
    new_file <- file.path(new_dir, file)
    if (rd_equal(old_file, new_file)) {
      next
    }
    cat(sprintf("roxygenize() output differs from saved output for %s.\n", file))
    cat("Here's the 'diff' comparison of the two files:\n")
    cat("  [---]: saved output in man/ directory\n")
    cat("  [+++]: roxygenize() output of R/ sources\n")
    system2("diff", c("--unified", old_file, new_file))
    stop("Failed in LOCALE=", LOCALE, ".", call. = FALSE)
  }
}

# Run the check in a few locales to ensure there's no idempotency issues w.r.t. sorting, too
for (LOCALE in c("C", "en_US", "hu_HU", "ja_JP")) {
  check_roxygenize_idempotent(LOCALE)
}

unlink(old_dir, recursive = TRUE)
