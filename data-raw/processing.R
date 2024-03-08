# Processing
# Load packages -----------------------------------------------------------
library(tidyverse)

# Load saved data:
source(here::here("R/1_data_start.R")) #loads data
# source(here::here("R/2_data_management.R")) #runs data management
# source(here::here("R/3_diet_data.R")) #runs diet code
# source(here::here("R/4_icd10_outcomes_tte.R")) #runs outcome code

# Save the changes as parquet and upload to the RAP folder for easy download
# next time you sign in
arrow::write_parquet(data, here("data/data.parquet"))

# Upload to the project RAP folder.
ukbAid::upload_data(here("data/data.parquet"), username = "FieLangmann")

# Now you can run your analyses with data as data frame
