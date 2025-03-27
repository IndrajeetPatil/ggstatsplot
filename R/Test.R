### TEST ####
# Install necessary packages if not installed

# Load libraries
library(ggstatsplot)
library(ggplot2)

# Create a data frame to replicate the 'tips' dataset
set.seed(123)  # For reproducibility

tips <- data.frame(
  total_bill = round(runif(200, 10, 50), 2),
  tip = round(runif(200, 1, 15), 2),
  sex = as.factor(sample(c("Male", "Female"), 200, replace = TRUE)),
  smoker = as.factor(sample(c("Yes", "No"), 200, replace = TRUE)),
  size = sample(1:6, 200, replace = TRUE)
)

# View the first few rows of the dataset
head(tips)

# Attempt a ggstatsplot function
grouped_ggbarstats(
  data = tips,
  x = sex,
  y = tip,  # Ensure this is treated as a categorical variable
  grouping.var = smoker,
  plot.type = "box"
)
