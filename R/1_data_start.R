library(tidyverse)
library(here)

# This is a "SQL" database object, but because dplyr connects to it, you don't
# need to change your code. Keep using the tidyverse :D
# If you ever want to convert the dataset into a "tibble"/"dataframe", use
# `tibble::as_tibble()`.
data <- ukbAid::read_parquet(here("data/data.parquet"))

# Converting the dataset into a tibble to work with for analyses
data <-tibble::as_tibble(data)
