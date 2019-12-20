# loading the needed libraries
library(jmv)
library(tibble)
library(tidyr)

# loading dataset
data("bugs", package = "jmv")

# converting to long format
bugs_wide <- tibble::as_tibble(bugs)

# all column names in lower case
names(bugs_wide) <- tolower(names(bugs_wide))

# saving the data
save(bugs_wide, file = "data/bugs_wide.rdata")
