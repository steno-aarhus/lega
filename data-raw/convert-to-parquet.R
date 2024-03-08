library(tidyverse)
library(here)

# Split dataset into core and those with dates because
# there were issues loading them.
data_csv_core <- read_csv(
  here("data/data.csv"),
  col_select = c(-matches("^p4128[0-3]_.*$"))
)
problems(data_csv_core)

# Create a separate dataset of variables that are only dates
# to fix the importing issue.
data_csv_dates <- read_csv(
  here("data/data.csv"),
  col_select = matches("^p4128[0-3]_.*$"),
  # All dates
  col_types = "D"
)
problems(data_csv_dates)

# Combine them together.
data_csv <- list_cbind(list(
  data_csv_core,
  data_csv_dates
))

# Convert to the Parquet format. Why? Parquet is faster and a smaller
# file size.
arrow::write_parquet(data_csv, here("data/data.parquet"))

# Upload to the project RAP folder.
ukbAid::upload_data(here("data/data.parquet"), username = "FieLangmann")
