# Script to create variables for time-to-event analyses based on ICD diagnoses
# and OPCS codes

# Load packages -----------------------------------------------------------
library(tidyverse)
library(magrittr)
library(dplyr)
library(here)
library(ggplot2)


# Defining gallbladder disease dates  -------------------------------------

## ICD10 codes ---------------------------------------------------------
icd10_subset <- data %>%
    select(starts_with("p41270"), starts_with("p41280"), "id") %>%
    # splitting diagnoses string-variable each time a | is in the string
    separate_wider_delim(p41270, delim = "|", names = paste0("p41270var_a", 0:258), too_few = "debug") %>%
    select(matches("p41270|p41280|id")) %>%
    pivot_longer(cols = matches("_a[0-9]*$"), names_to = c(".value", "a"), names_sep = "_") %>%

    # creating outcome variables with date info from p41280
    mutate(
        # gallstone (K80.X)
        icd10_gallstone_date = ifelse(str_detect(p41270var, "K80"), as.character(c_across(starts_with("p41280"))), NA),
        icd10_gallstone_date = as.Date(icd10_gallstone_date, format = "%Y-%m-%d"),
        # cholecystitis (K81.X)
        icd10_cholecystit_date = ifelse(str_detect(p41270var, "K81"), as.character(c_across(starts_with("p41280"))), NA),
        icd10_cholecystit_date = as.Date(icd10_cholecystit_date, format = "%Y-%m-%d"),
        # gallbladder obstruction (K82.0)
        icd10_gbobstruction_date = ifelse(str_detect(p41270var, "K82.0"), as.character(c_across(starts_with("p41280"))), NA),
        icd10_gbobstruction_date = as.Date(icd10_gbobstruction_date, format = "%Y-%m-%d"),
        # bile obstruction (K83.1)
        icd10_bileobstruction_date = ifelse(str_detect(p41270var, "K83.1"), as.character(c_across(starts_with("p41280"))), NA),
        icd10_bileobstruction_date = as.Date(icd10_bileobstruction_date, format = "%Y-%m-%d"))

# Retrieve the first non-NA date for each outcome
first_non_na_icd10 <- icd10_subset %>%
    select(id, icd10_gallstone_date, icd10_cholecystit_date, icd10_gbobstruction_date, icd10_bileobstruction_date) %>%
    pivot_longer(cols = starts_with("icd10_"), names_to = "condition", values_to = "date") %>%
    filter(!is.na(date)) %>%
    group_by(id, condition) %>%
    slice(1) %>%
    pivot_wider(names_from = condition, values_from = date) %>%
    ungroup()

# Join the dates back to the original data
data <- data %>%
    left_join(first_non_na_icd10, by = "id")


## ICD9 codes ---------------------------------------------------------
icd9_subset <- data %>%
    select(starts_with("p41271"), starts_with("p41281"), "id") %>%
    # splitting diagnoses string-variable each time a | is in the string
    separate_wider_delim(p41271, delim = "|", names = paste0("p41271var_a", 0:46), too_few = "debug") %>%
    select(matches("p41271|p41281|id")) %>%
    pivot_longer(cols = matches("_a[0-9]*$"), names_to = c(".value", "a"), names_sep = "_") %>%

    # creating outcome variables with date info from p41281
    mutate(
        # gallstone
        icd9_gallstone_date = ifelse(str_detect(p41271var, "574"), as.character(c_across(starts_with("p41281"))), NA),
        icd9_gallstone_date = as.Date(icd9_gallstone_date, format = "%Y-%m-%d"),
        # acute cholecystitis
        icd9_acute_date = ifelse(str_detect(p41271var, "5750"), as.character(c_across(starts_with("p41281"))), NA),
        icd9_acute_date = as.Date(icd9_acute_date, format = "%Y-%m-%d"),
        # other cholecystitis
        icd9_other_date = ifelse(str_detect(p41271var, "5751"), as.character(c_across(starts_with("p41281"))), NA),
        icd9_other_date = as.Date(icd9_other_date, format = "%Y-%m-%d"),
        # bile duct obstruction (5762)
        icd9_bileobstruction_date = ifelse(str_detect(p41271var, "5762"), as.character(c_across(starts_with("p41281"))), NA),
        icd9_bileobstruction_date = as.Date(icd9_bileobstruction_date, format = "%Y-%m-%d")
    )

# Retrieve the first non-NA date for each outcome
first_non_na_icd9 <- icd9_subset %>%
    select(id, icd9_gallstone_date, icd9_acute_date, icd9_other_date, icd9_bileobstruction_date) %>%
    pivot_longer(cols = starts_with("icd9_"), names_to = "condition", values_to = "date") %>%
    filter(!is.na(date)) %>%
    group_by(id, condition) %>%
    slice(1) %>%
    pivot_wider(names_from = condition, values_from = date) %>%
    ungroup()

# Join the dates back to the original data
data <- data %>%
    left_join(first_non_na_icd9, by = "id")


## OPCS 4 codes ---------------------------------------------------------
opcs4 <- data %>%
    select(starts_with("p41272"), starts_with("p41282"), "id") %>%
    # splitting diagnoses string-variable each time a | is in the string
    separate_wider_delim(p41272, delim = "|", names = paste0("p41272var_a", 0:125), too_few = "debug") %>%
    select(matches("p41272|p41282|id")) %>%
    pivot_longer(cols = matches("_a[0-9]*$"), names_to = c(".value", "a"), names_sep = "_") %>%

    # creating outcome variables with date info from p41282
    mutate(
        # gallbladder removal
        opcs4_removal_date = case_when(
            str_detect(p41272var, "J18.1") ~ as.character(c_across(starts_with("p41282"))),
            str_detect(p41272var, "J18.2") ~ as.character(c_across(starts_with("p41282"))),
            str_detect(p41272var, "J18.3") ~ as.character(c_across(starts_with("p41282"))),
            str_detect(p41272var, "J18.8") ~ as.character(c_across(starts_with("p41282"))),
            str_detect(p41272var, "J18.9") ~ as.character(c_across(starts_with("p41282"))),
            TRUE ~ NA_character_),
        opcs4_removal_date = as.Date(opcs4_removal_date, format = "%Y-%m-%d"),
        opcs4_gallstone_date = case_when(
            str_detect(p41272var, "J33.1") ~ as.character(c_across(starts_with("p41282"))),
            str_detect(p41272var, "J33.2") ~ as.character(c_across(starts_with("p41282"))),
            str_detect(p41272var, "J52.1") ~ as.character(c_across(starts_with("p41282"))),
            str_detect(p41272var, "J21.1") ~ as.character(c_across(starts_with("p41282"))),
            str_detect(p41272var, "J41.1") ~ as.character(c_across(starts_with("p41282"))),
            str_detect(p41272var, "J41.3") ~ as.character(c_across(starts_with("p41282"))),
            str_detect(p41272var, "J49.1") ~ as.character(c_across(starts_with("p41282"))),
            str_detect(p41272var, "J49.2") ~ as.character(c_across(starts_with("p41282"))),
            str_detect(p41272var, "J24.2") ~ as.character(c_across(starts_with("p41282"))),
            str_detect(p41272var, "J24.3") ~ as.character(c_across(starts_with("p41282"))),
            str_detect(p41272var, "J26.1") ~ as.character(c_across(starts_with("p41282"))),
            TRUE ~ NA_character_),
        opcs4_gallstone_date = as.Date(opcs4_gallstone_date, format = "%Y-%m-%d")
        )

first_non_na_opcs4 <- opcs4 %>%
    select(id, opcs4_removal_date, opcs4_gallstone_date) %>%
    pivot_longer(cols = starts_with("opcs4_"), names_to = "condition", values_to = "date") %>%
    filter(!is.na(date)) %>%
    group_by(id, condition) %>%
    slice(1) %>%
    pivot_wider(names_from = condition, values_from = date) %>%
    ungroup()

# Join the dates back to the original data
data <- data %>%
    left_join(first_non_na_opcs4, by = "id")


## OPCS 3 codes ---------------------------------------------------------
opcs3 <- data %>%
    select(starts_with("p41273"), starts_with("p41283"), "id") %>%
    # splitting diagnoses string-variable each time a | is in the string
    separate_wider_delim(p41273, delim = "|", names = paste0("p41272var_a", 0:15), too_few = "debug") %>%
    select(matches("p41273|p41283|id")) %>%
    pivot_longer(cols = matches("_a[0-9]*$"), names_to = c(".value", "a"), names_sep = "_") %>%

        # creating outcome variables with date info from p41283
        mutate(
            opcs3_removal_date = case_when(
                str_detect(p41273, "522") ~ as.character(c_across(starts_with("p41283"))),
                str_detect(p41273, "522.2") ~ as.character(c_across(starts_with("p41283"))),
                TRUE ~ NA_character_),
            opcs3_removal_date = as.Date(opcs3_removal_date, format = "%Y-%m-%d"),
            opcs3_gallstone_date = ifelse(str_detect(p41273, "511"),
                                          as.character(c_across(starts_with("p41283"))),
                                          NA),
            opcs3_gallstone_date = as.Date(opcs3_gallstone_date, format = "%Y-%m-%d")
            )

first_non_na_opcs3 <- opcs3 %>%
    select(id, opcs3_removal_date, opcs3_gallstone_date) %>%
    pivot_longer(cols = starts_with("opcs3_"), names_to = "condition", values_to = "date") %>%
    filter(!is.na(date)) %>%
    group_by(id, condition) %>%
    slice(1) %>%
    pivot_wider(names_from = condition, values_from = date) %>%
    ungroup()


# Join the dates back to the original data
data <- data %>%
    left_join(first_non_na_opcs3, by = "id")

# Define variables for survival analysis ----------------------------------

## date of death and loss to follow-up -------------------------------------
data <- data %>%
    mutate(date_of_death = if_else(!is.na(p40000_i0), p40000_i0, p40000_i1),
           date_of_death = as.Date(date_of_death),
           loss_to_follow_up = p191,
           loss_to_follow_up = as.Date(loss_to_follow_up))


## birth date as origin for survival analysis ------------------------------
# Merging birth year and month of birth into one column:

month_names <- c("January", "February", "March", "April", "May", "June",
                 "July", "August", "September", "October", "November", "December")

data <- data %>%
    mutate(month_of_birth_num = sprintf("%02d", match(p52, month_names))) %>%
        unite(date_birth, p34, month_of_birth_num, sep = "-")

remove(month_names)

# adding 15 as DD for all participants:
data$date_birth <- as.Date(paste0(data$date_birth, "-15"))


## Set cut-off date for follow-up ------------------------------------------
# Estimated last follow-up date for cholecystectomy (stagnation of diagnoses)
ggplot(data, aes(x = opcs4_removal_date, y = id)) +
    geom_point() + # Use points to show individual data points
    geom_smooth(method = "lm", se = FALSE) + # Add linear regression line
    annotate("text", x = max(data$opcs4_removal_date), y = min(data$id),
             label = paste("Last observed date:", max(data$opcs4_removal_date)),
             hjust = 1, vjust = -0.5, size = 4) +  # Add annotation for the last observed date
    labs(title = "Dates of Disease Over Time", x = "Date of Disease", y = "Participant ID")

# The density is very high in the right of the plot - I will estimate the last
# diagnosis date in data:
dates <- data %>%
    subset(!is.na(opcs4_removal_date))

# Find the last date of diagnosis
last_date <- max(dates$opcs4_removal_date)
print(last_date)

# # Administrative censoring at October 31st, 2022
data <- data %>%
    mutate(censoring = as.Date("2022-10-31"))


## Estimating survival time and outcome event--------------------------------------------------

# Defining survival time as first occurrence of outcome
data <- data %>%
    mutate(
        # gallstone or removal of gallbladder
        survival_time_gst = case_when(
            !is.na(icd10_gallstone_date) ~ as.numeric(difftime(icd10_gallstone_date, date_birth, units = "days")),
            !is.na(icd10_bileobstruction_date) ~ as.numeric(difftime(icd10_bileobstruction_date, date_birth, units = "days")),
            !is.na(icd10_gbobstruction_date) ~ as.numeric(difftime(icd10_gbobstruction_date, date_birth, units = "days")),
            !is.na(icd9_gallstone_date) ~ as.numeric(difftime(icd9_gallstone_date, date_birth, units = "days")),
            !is.na(icd9_bileobstruction_date) ~ as.numeric(difftime(icd9_bileobstruction_date, date_birth, units = "days")),
            !is.na(opcs4_removal_date) ~ as.numeric(difftime(opcs4_removal_date, date_birth, units = "days")),
            !is.na(opcs4_gallstone_date) ~ as.numeric(difftime(opcs4_gallstone_date, date_birth, units = "days")),
            !is.na(opcs3_removal_date) ~ as.numeric(difftime(opcs3_removal_date, date_birth, units = "days")),
            !is.na(opcs3_gallstone_date) ~ as.numeric(difftime(opcs3_gallstone_date, date_birth, units = "days")),
            !is.na(date_of_death) ~ as.numeric(difftime(date_of_death, date_birth, units = "days")),
            !is.na(loss_to_follow_up) ~ as.numeric(difftime(loss_to_follow_up, date_birth, units = "days")),
            TRUE ~ as.numeric(difftime(censoring, date_birth, units = "days"))
        ),
        # Using pmin to get the minimum survival time across columns
        survival_gallstone = pmin(survival_time_gst, na.rm = TRUE),
        survival_gallstone = survival_gallstone/365.25,
        # Remove temporary variable
        survival_time_gst = NULL,


        # cholecystitis
        survival_time_cholecystit = case_when(
            !is.na(icd10_cholecystit_date) ~ as.numeric(difftime(icd10_cholecystit_date, date_birth, units = "days")),
            !is.na(icd9_acute_date) ~ as.numeric(difftime(icd9_acute_date, date_birth, units = "days")),
            !is.na(icd9_other_date) ~ as.numeric(difftime(icd9_other_date, date_birth, units = "days")),
            !is.na(date_of_death) ~ as.numeric(difftime(date_of_death, date_birth, units = "days")),
            !is.na(loss_to_follow_up) ~ as.numeric(difftime(loss_to_follow_up, date_birth, units = "days")),
            TRUE ~ as.numeric(difftime(censoring, date_birth, units = "days"))
        ),
        # Use min to get the minimum survival time across columns
        survival_cholecystit = pmin(survival_time_cholecystit, na.rm = TRUE),
        survival_cholecystit = survival_cholecystit/365.25,
        # Remove temporary variable
        survival_time_cholecystit = NULL,

        # any gallbladder disease
        survival_time_gbd = case_when(
            !is.na(icd10_gallstone_date) ~ as.numeric(difftime(icd10_gallstone_date, date_birth, units = "days")),
            !is.na(icd10_bileobstruction_date) ~ as.numeric(difftime(icd10_bileobstruction_date, date_birth, units = "days")),
            !is.na(icd10_gbobstruction_date) ~ as.numeric(difftime(icd10_gbobstruction_date, date_birth, units = "days")),
            !is.na(icd9_gallstone_date) ~ as.numeric(difftime(icd9_gallstone_date, date_birth, units = "days")),
            !is.na(icd9_bileobstruction_date) ~ as.numeric(difftime(icd9_bileobstruction_date, date_birth, units = "days")),
            !is.na(opcs4_removal_date) ~ as.numeric(difftime(opcs4_removal_date, date_birth, units = "days")),
            !is.na(opcs4_gallstone_date) ~ as.numeric(difftime(opcs4_gallstone_date, date_birth, units = "days")),
            !is.na(opcs3_removal_date) ~ as.numeric(difftime(opcs3_removal_date, date_birth, units = "days")),
            !is.na(opcs3_gallstone_date) ~ as.numeric(difftime(opcs3_gallstone_date, date_birth, units = "days")),
            !is.na(icd10_cholecystit_date) ~ as.numeric(difftime(icd10_cholecystit_date, date_birth, units = "days")),
            !is.na(icd9_acute_date) ~ as.numeric(difftime(icd9_acute_date, date_birth, units = "days")),
            !is.na(icd9_other_date) ~ as.numeric(difftime(icd9_other_date, date_birth, units = "days")),
            !is.na(date_of_death) ~ as.numeric(difftime(date_of_death, date_birth, units = "days")),
            !is.na(loss_to_follow_up) ~ as.numeric(difftime(loss_to_follow_up, date_birth, units = "days")),
            TRUE ~ as.numeric(difftime(censoring, date_birth, units = "days"))
        ),
        # Use min to get the minimum survival time across columns
        survival_gbd = pmin(survival_time_gbd, na.rm = TRUE),
        survival_gbd = survival_gbd/365.25,
        # Remove temporary variable
        survival_time_gbd = NULL,

        # Create binary variable to indicate if outcome happened
        # gallstone
        gallstone = case_when(
            !is.na(icd10_gallstone_date) | !is.na(icd10_bileobstruction_date) |
                !is.na(icd10_gbobstruction_date) | !is.na(icd9_gallstone_date) |
                !is.na(icd9_bileobstruction_date) |
                !is.na(opcs4_removal_date) | !is.na(opcs4_gallstone_date) |
                !is.na(opcs3_removal_date) | !is.na(opcs3_gallstone_date) ~ 1,
            TRUE ~ 0),
        # cholecystitis
        cholecystit = case_when(
            !is.na(icd10_cholecystit_date) | !is.na(icd9_acute_date) |
                !is.na(icd9_other_date) ~ 1,
            TRUE ~ 0),
        # any gallbladder disease
        gbd = case_when(
            !is.na(icd10_gallstone_date) | !is.na(icd10_bileobstruction_date) |
                !is.na(icd10_gbobstruction_date) | !is.na(icd9_gallstone_date) |
                !is.na(icd9_other_date) | !is.na(icd9_bileobstruction_date) |
                !is.na(opcs4_removal_date) | !is.na(opcs4_gallstone_date) |
                !is.na(opcs3_removal_date) | !is.na(opcs3_gallstone_date) |
                !is.na(icd10_cholecystit_date) | !is.na(icd9_acute_date)
                 ~ 1,
            TRUE ~ 0)
    )


# Remove those with outcome before baseline (=last webQ) --------

# Setting last completed WebQ as baseline
data <- data %>%
    mutate(ques_comp_t0 = p105010_i0,
           ques_comp_t1 = p105010_i1,
           ques_comp_t2 = p105010_i2,
           ques_comp_t3 = p105010_i3,
           ques_comp_t4 = p105010_i4,
           # Removing specific time stamp
           ques_comp_t0 = substr(ques_comp_t0, 1, 10),
           ques_comp_t1 = substr(ques_comp_t1, 1, 10),
           ques_comp_t2 = substr(ques_comp_t2, 1, 10),
           ques_comp_t3 = substr(ques_comp_t3, 1, 10),
           ques_comp_t4 = substr(ques_comp_t4, 1, 10)) %>%

           #baseline start date as last completed questionnaire
           mutate(across(starts_with("ques_"), as.Date)) %>%
               # Gather all columns into key-value pairs
               pivot_longer(cols = starts_with("ques_"), names_to = "questionnaire", values_to = "date_filled") %>%
               # Group by participant ID and select the row with the latest date_filled for each participant
               group_by(id) %>%
               slice(which.max(date_filled)) %>%
               ungroup() %>%
               # Rename the remaining column to indicate the last filled questionnaire
               rename(last_filled_questionnaire = questionnaire) %>%
    mutate(date_filled = as.Date(date_filled))

# Removing those diagnosed before baseline
diagnosed_before <- function(data) {
    filtered_data <- data %>%
        filter(if_any(
            c(icd10_gallstone_date, icd10_bileobstruction_date, icd10_gbobstruction_date,
              icd9_gallstone_date, icd9_bileobstruction_date,
              opcs4_removal_date, opcs4_gallstone_date,
              opcs3_removal_date, opcs3_gallstone_date, icd10_cholecystit_date,
              icd9_acute_date, icd9_other_date),
            ~ . < date_filled
        ))

    data <- data %>%
        anti_join(filtered_data, by = "id")

    return(data)
}

data <- diagnosed_before(data)

# Removing those who were lost to follow-up or died before baseline
left_study <- function(data) {
    filtered_data <- data %>%
        filter(if_any(
            c(date_of_death, loss_to_follow_up),
            ~ . < date_filled
        ))

    data <- data %>%
        anti_join(filtered_data, by = "id")

    return(data)
}

data <- left_study(data)


# Remove redundant variables --------------------------------------------

remove <- c("p191", "p40000_i0", "p40000_i1","p105010_i0", "p105010_i1", "p105010_i2",
            "p105010_i3","p105010_i4", "p41280", "p41270", "p41281", "p41271", "p41282",
            "p41272","p41283", "p41273")
data <- data %>%
    select(-matches(remove))

# save data locally to be able to upload to RAP in processing.R
readr::write_csv(data, here::here("data/data_lega.csv"))
