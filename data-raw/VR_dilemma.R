# loading the needed libraries
library(dplyr)
library(tidyr)
library(readr)
library(stringr)

# reading the datafile
VR_dilemma <- readr::read_csv(file = "data-raw/VR_dilemma.csv")

# selecting only the decision columns
VR_dilemma %<>%
  dplyr::select(ID:vrD) %>%
  tidyr::gather(
    key = "modality",
    value = "score",
    textD:vrD,
    na.rm = TRUE
  ) %>%
  dplyr::mutate(
    modality = stringr::str_remove(modality, "D$"),
    order = tolower(sjlabelled::as_label(order))
  ) %>%
  dplyr::rename(id = ID)

# saving the data
save(VR_dilemma, file = "data/VR_dilemma.rdata")
