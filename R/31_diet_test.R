# Load packages -----------------------------------------------------------
library(tidyverse)
library(magrittr)
library(dplyr)

# Load saved data:
targets::tar_make()
source(here::here("R/1_data_start.R"))


# Average dietary intake of food groups -----------------------------------
calculate_total <- function(columns) {
    sum(dplyr::c_across(columns), na.rm = TRUE)
}

calculate_daily <- function(data) {
    data |>
        dplyr::mutate(dplyr::across(dplyr::ends_with("_total"), list(daily = ~ .x / p20077))) |>
        dplyr::rename_with(~ stringr::str_replace("_total_daily", "_daily"))
}

calculate_weekly <- function(data) {
    data |>
        dplyr::mutate(dplyr::across(dplyr::ends_with("_total"), list(weekly = ~ (.x / p20077) * 7))) |>
        dplyr::rename_with(~ stringr::str_replace("_total_weekly", "_weekly"))
}

calculate_food_intake <- function(data) {
    # estimating average daily and weekly intakes of food groups in g
    data <- data %>%
        # creating food groups from UKB Aurora Perez
        rowwise() |>
        mutate(
            cereal_refined_total = calculate_total(matches("p26113|p26079|p26071|p26072|p26073|p26075|p26068|p26083")),
            whole_grain_total = calculate_total(matches("p26074|p26076|p26077|p26078|p26105|p26114"))
        ) |>
        calculate_daily() |>
        calculate_weekly() |>
        ungroup()
    return(diet_data)
}



diet <- calculate_food_intake(data)
