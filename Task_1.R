install.packages("tidyverse")
library(tidyverse)
# Read the CSV file into a data frame
happiness_data <- read_csv("WHR2024.csv")

# Display a message confirming the data has been loaded
cat("Dataset loaded successfully with", nrow(happiness_data), "rows and", ncol(happiness_data), "columns.\n")


# Display the first 5 rows of the dataset
head(happiness_data, 5)


# Generate summary statistics for all columns
summary(happiness_data)


# Count missing values in each column
missing_values <- colSums(is.na(happiness_data))

# Display columns with missing values (if any)
if (sum(missing_values) > 0) {
  cat("Missing values found:\n")
  print(missing_values[missing_values > 0])
} else {
  cat("No missing values found in the dataset.\n")
}


# Store the original number of rows
original_rows <- nrow(happiness_data)

# Remove rows with any missing values
cleaned_happiness <- happiness_data %>% 
  drop_na()

# Compare row counts
cat("Original rows:", original_rows, "\n")
cat("Rows after cleaning:", nrow(cleaned_happiness), "\n")
cat("Rows removed:", original_rows - nrow(cleaned_happiness), "\n")


# Save the cleaned dataset
write_csv(cleaned_happiness, "cleaned_happiness.csv")

# Confirm the file has been saved
cat("Cleaned dataset saved as 'cleaned_happiness.csv'\n")
