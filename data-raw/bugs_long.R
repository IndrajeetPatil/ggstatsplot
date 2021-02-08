# loading the needed libraries
library(jmv)
library(tibble)
library(tidyr)

# loading dataset
data("bugs", package = "jmv")

# converting to long format
bugs_long <- bugs %>%
  tibble::as_tibble(.) %>%
  tidyr::gather(key = "condition", value = "desire", LDLF:HDHF)

# all column names in lower case
names(bugs_long) <- tolower(names(bugs_long))

# saving the data
save(bugs_long, file = "data/bugs_long.rdata")
