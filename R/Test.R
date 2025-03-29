### TEST ####
# Install necessary packages if not installed

# Load the custom function script
source("R/ggbetweenstats.R")
library(ggplot2)

# Create a data frame to replicate the 'tips' dataset
set.seed(123)  # For reproducibility

fake_data <- data.frame(
  total_bill = round(runif(200, 10, 50), 2),
  tip = round(runif(200, 1, 15), 2),
  sex = as.factor(sample(c("Male", "Female"), 200, replace = TRUE)),
  smoker = as.factor(sample(c("Yes", "No"), 200, replace = TRUE)),
  size = sample(1:6, 200, replace = TRUE)
)

# View the first few rows of the dataset
head(fake_data)

# Use your custom ggbetweenstats function
ggbetweenstats_test(
  data = fake_data,
  x = sex,   # Pass the variable as it is, without quotes
  y = tip,   # Pass the variable as it is, without quotes
  plot.type = "box",  # Specify plot type
  type = "parametric",  # Specify the statistical test type
  pairwise.display = "all"  # Display pairwise comparisons
)
