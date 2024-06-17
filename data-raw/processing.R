# Processing
# Load packages -----------------------------------------------------------
library(tidyverse)

# Save data to RAP --------------------------------------------------------

# save data locally to be able to upload to RAP
readr::write_csv(data, here::here("data/data_lega.csv"))

# save to RAP
rap_copy_to("data/data_lega.csv", "/users/FieLangmann/data_lega.csv")


# Load data from RAP ------------------------------------------------------

# load data from RAP
data <- readr::read_csv("/users/FieLangmann/data_lega.csv")

