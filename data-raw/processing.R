# Processing
# Load packages -----------------------------------------------------------
library(tidyverse)
library(ukbAid)
# Save data to RAP --------------------------------------------------------

# save data locally to be able to upload to RAP
readr::write_csv(data, here::here("data/data_lega.csv"))

# save to RAP
rap_copy_to("data/data_lega.csv", "/users/FieLangmann/data_lega.csv")


# Load data from RAP ------------------------------------------------------

# load data from RAP
rap_data <- rap_copy_from("/users/FieLangmann/FieLangmann-lega-2026-02-04T12-13-27.csv", "data/data_lega.csv")
# load data into RStudio
data <- readr::read_csv("data/data_lega.csv")
