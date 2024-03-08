# This script will split p41270 (ICD10-codes) and p41271 (ICD9-codes) into
# columns and match the date of diagnosis with the diagnosis code for that
# specific data. This is useful when using ICD10 diagnoses as outcomes in
# time-to-event analyses

# Load packages -----------------------------------------------------------
library(tidyverse)
library(magrittr)
library(dplyr)
library(here)
library(ggplot2)


# Defining gallbladder disease dates  -------------------------------------
## ICD10 codes ---------------------------------------------------------
icd <- data %>%
  select(starts_with("p41270"), starts_with("p41280"), "id") %>%
  separate_wider_delim(p41270,
                       delim = "|", # Split the diagnosis-variable into separate columns based on delimiter "|"
                       names = paste0("p41270var_a", 0:258), too_few = "debug")
icd10_subset <- icd %>%
  select(matches("p41270|p41280|id")) %>%
  pivot_longer(cols = matches("_a[0-9]*$"), # Transform from wide to long format to match ICD-codes with date of diagnosis
               names_to = c(".value", "a"),
               names_sep = "_")

# Defining dates of gallstone (K80.X)
icd10_gallstone <- icd10_subset %>%
  mutate(icd10_gallstone_date = ifelse(str_detect(p41270var, "K80"),
                                   as.character(c_across(starts_with("p41280"))),
                                   NA),
         icd10_gallstone_date = as.Date(icd10_gallstone_date, format = "%Y-%m-%d"))
first_non_na_gallstone <- icd10_gallstone %>%
  filter(!is.na(icd10_gallstone_date)) %>%
  group_by(id) %>%
  slice(1) %>%
  ungroup()
data <- data %>%
  left_join(first_non_na_gallstone %>% select(id, icd10_gallstone_date), by = "id")

#Defining dates for bile duct obstruction (K82.0)
icd10_obstruction <- icd10_subset %>%
    mutate(icd10_obstruction_date = ifelse(str_detect(p41270var, "K82.0"),
                                         as.character(c_across(starts_with("p41280"))),
                                         NA),
           icd10_obstruction_date = as.Date(icd10_obstruction_date, format = "%Y-%m-%d"))
first_non_na_obstruction <- icd10_obstruction %>%
    filter(!is.na(icd10_obstruction_date)) %>%
    group_by(id) %>%
    slice(1) %>%
    ungroup()
data <- data %>%
    left_join(first_non_na_obstruction %>% select(id, icd10_obstruction_date), by = "id")

# Defining dates of cholecystitis (K81.X)
icd10_cholecystit <- icd10_subset %>%
  mutate(icd10_cholecystit_date = ifelse(str_detect(p41270var, "K81"),
                                   as.character(c_across(starts_with("p41280"))),
                                   NA),
         icd10_cholecystit_date = as.Date(icd10_cholecystit_date, format = "%Y-%m-%d"))

first_non_na_cholecystit <- icd10_cholecystit %>%
  filter(!is.na(icd10_cholecystit_date)) %>%
  group_by(id) %>%
  slice(1) %>%
  ungroup()

data <- data %>%
  left_join(first_non_na_cholecystit %>% select(id, icd10_cholecystit_date), by = "id")

### Delete old ICD10 diagnosis and dates ------------------------------------
delete <- c("p41280", "p41270")
data <- data %>%
  select(-matches(paste0(delete)))


## ICD9 codes ---------------------------------------------------------
# Split the diagnosis-variable into separate columns based on delimiter "|"
icd9 <- data %>%
  select(starts_with("p41271"), starts_with("p41281"), "id") %>%
  separate_wider_delim(p41271,
                       delim = "|",
                       names = paste0("p41271var_a", 0:46), too_few = "debug")

# Transform from wide to long format to match ICD-codes with date of diagnosis
icd9_subset <- icd9 %>%
  select(matches("p41271|p41281|id")) %>%
  pivot_longer(cols = matches("_a[0-9]*$"),
               names_to = c(".value", "a"),
               names_sep = "_")

# Defining dates of gallstone (574X)
icd9_gallstone <- icd9_subset %>%
  mutate(icd9_gallstone_date = ifelse(str_detect(p41271var, "574"),
                                   as.character(c_across(starts_with("p41281"))),
                                   NA),
         icd9_gallstone_date = as.Date(icd9_gallstone_date, format = "%Y-%m-%d"))

first_non_na_gallstone <- icd9_gallstone %>%
  filter(!is.na(icd9_gallstone_date)) %>%
  group_by(id) %>%
  slice(1) %>%
  ungroup()

data <- data %>%
  left_join(first_non_na_gallstone %>% select(id, icd9_gallstone_date), by = "id")

# Defining dates of bile duct obstruction
icd9_obstruction <- icd9_subset %>%
  mutate(icd9_obstruction_date = ifelse(str_detect(p41271var, "5762"),
                                  as.character(c_across(starts_with("p41281"))),
                                  NA),
         icd9_obstruction_date = as.Date(icd9_obstruction_date, format = "%Y-%m-%d"))

first_non_na_obstruction <- icd9_obstruction %>%
  filter(!is.na(icd9_obstruction_date)) %>%
  group_by(id) %>%
  slice(1) %>%
  ungroup()

data <- data %>%
  left_join(first_non_na_obstruction %>% select(id, icd9_obstruction_date), by = "id")

# Defining date of acute cholecystitis
icd9_acute <- icd9_subset %>%
    mutate(icd9_acute_date = ifelse(str_detect(p41271var, "5750"),
                                          as.character(c_across(starts_with("p41281"))),
                                          NA),
           icd9_acute_date = as.Date(icd9_acute_date, format = "%Y-%m-%d"))

first_non_na_acute <- icd9_acute %>%
    filter(!is.na(icd9_acute_date)) %>%
    group_by(id) %>%
    slice(1) %>%
    ungroup()

data <- data %>%
    left_join(first_non_na_acute %>% select(id, icd9_acute_date), by = "id")

# Defining date of other cholecystitis
icd9_other <- icd9_subset %>%
    mutate(icd9_other_date = ifelse(str_detect(p41271var, "5751"),
                                    as.character(c_across(starts_with("p41281"))),
                                    NA),
           icd9_other_date = as.Date(icd9_other_date, format = "%Y-%m-%d"))

first_non_na_other <- icd9_other %>%
    filter(!is.na(icd9_other_date)) %>%
    group_by(id) %>%
    slice(1) %>%
    ungroup()

data <- data %>%
    left_join(first_non_na_other %>% select(id, icd9_other_date), by = "id")


### Delete old ICD9 diagnosis and dates ------------------------------------
delete <- c("p41281", "p41271")
data <- data %>%
  select(-matches(paste0(delete)))


## OPCS 4 codes ---------------------------------------------------------
opcs4 <- data %>%
    select(starts_with("p41272"), starts_with("p41282"), "id") %>%
    separate_wider_delim(p41272,
                         delim = "|",
                         names = paste0("p41272var_a", 0:125), too_few = "debug")

# Transform from wide to long format to match ICD-codes with date of diagnosis
opcs4_subset <- opcs4 %>%
    select(matches("p41272|p41282|id")) %>%
    pivot_longer(cols = matches("_a[0-9]*$"),
                 names_to = c(".value", "a"),
                 names_sep = "_")
# Defining dates of cholecystectomy
opcs4_removal <- opcs4_subset %>% mutate(
    opcs4_removal_date = case_when(
        str_detect(p41272var, "J18.1") ~ as.character(c_across(starts_with("p41282"))),
        str_detect(p41272var, "J18.2") ~ as.character(c_across(starts_with("p41282"))),
        str_detect(p41272var, "J18.3") ~ as.character(c_across(starts_with("p41282"))),
        str_detect(p41272var, "J18.8") ~ as.character(c_across(starts_with("p41282"))),
        str_detect(p41272var, "J18.9") ~ as.character(c_across(starts_with("p41282"))),
        TRUE ~ NA_character_),
    opcs4_removal_date = as.Date(opcs4_removal_date, format = "%Y-%m-%d"))

first_non_na_removal <- opcs4_removal %>%
    filter(!is.na(opcs4_removal_date)) %>%
    group_by(id) %>%
    slice(1) %>%
    ungroup()

data <- data %>%
    left_join(first_non_na_removal %>% select(id, opcs4_removal_date), by = "id")

# Defining dates of gallstone removal
opcs4_gallstone <- opcs4_subset %>% mutate(
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
    opcs4_gallstone_date = as.Date(opcs4_gallstone_date, format = "%Y-%m-%d"))

first_non_na_gallstone <- opcs4_gallstone %>%
    filter(!is.na(opcs4_gallstone_date)) %>%
    group_by(id) %>%
    slice(1) %>%
    ungroup()

data <- data %>%
    left_join(first_non_na_gallstone %>% select(id, opcs4_gallstone_date), by = "id")



### Delete old OPCS4 diagnosis and dates ------------------------------------
delete <- c("p41282", "p41272")
data <- data %>%
    select(-matches(paste0(delete)))


## OPCS 3 codes ---------------------------------------------------------
opcs3 <- data %>%
    select(starts_with("p41273"), starts_with("p41283"), "id") %>%
    separate_wider_delim(p41273,
                         delim = "|",
                         names = paste0("p41272var_a", 0:15), too_few = "debug")

# Transform from wide to long format to match ICD-codes with date of diagnosis
opcs3_subset <- opcs3 %>%
    select(matches("p41273|p41283|id")) %>%
    pivot_longer(cols = matches("_a[0-9]*$"),
                 names_to = c(".value", "a"),
                 names_sep = "_")

# Defining dates of cholecystectomy
opcs3_removal <- opcs3_subset %>% mutate(
    opcs3_removal_date = case_when(
        str_detect(p41273var, "522") ~ as.character(c_across(starts_with("p41283"))),
        str_detect(p41273var, "522.2") ~ as.character(c_across(starts_with("p41283"))),
        TRUE ~ NA_character_),
    opcs3_removal_date = as.Date(opcs3_removal_date, format = "%Y-%m-%d"))

first_non_na_removal <- opcs3_removal %>%
    filter(!is.na(opcs3_removal_date)) %>%
    group_by(id) %>%
    slice(1) %>%
    ungroup()

data <- data %>%
    left_join(first_non_na_removal %>% select(id, opcs3_removal_date), by = "id")

# Defining dates of gallstone removal
opcs3_gallstone <- opcs3_subset %>%
    mutate(opcs3_gallstone_date = ifelse(str_detect(p41723var, "511"),
                                         as.character(c_across(starts_with("p41283"))),
                                         NA),
           opcs3_gallstone_date = as.Date(opcs3_gallstone_date, format = "%Y-%m-%d"))

first_non_na_gallstone <- opcs3_gallstone %>%
    filter(!is.na(opcs3_gallstone_date)) %>%
    group_by(id) %>%
    slice(1) %>%
    ungroup()

data <- data %>%
    left_join(first_non_na_gallstone %>% select(id, opcs3_gallstone_date), by = "id")

### Delete old OPCS3 diagnosis and dates ------------------------------------
delete <- c("p41283", "p41273")
data <- data %>%
    select(-matches(paste0(delete)))



# Define variables for survival analysis ----------------------------------

## date of death and loss to follow-up -------------------------------------
data <- data %>%
  mutate(date_of_death = if_else(!is.na(p40000_i0), p40000_i0, p40000_i1),
         date_of_death = as.Date(date_of_death),
         loss_to_follow_up = p191,
         loss_to_follow_up = as.Date(loss_to_follow_up))

# remove p-values
remove <- c("p191", "p40000_i0", "p40000_i1")
data <- data %>%
  select(-matches(remove))

## birth date as origin for survival analysis ------------------------------
# Merging birth year and month of birth into one column:

month_names <- c("January", "February", "March", "April", "May", "June",
                 "July", "August", "September", "October", "November", "December")

data <- data %>%
  mutate(month_of_birth_num = sprintf("%02d", match(p52, month_names)))

data <- data %>%
  unite(date_birth, p34, month_of_birth_num, sep = "-")

remove(month_names)

# adding 15 as DD for all participants:
data$date_birth <- as.Date(paste0(data$date_birth, "-15"))


## Set cut-off date for follow-up ------------------------------------------
# Estimated last follow-up date for cholecystectomy (stagnation of diagnoses)
# Create the plot
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

# Print or use the last date as needed
print(last_date)

# # Administrative censoring at October 31st, 2022
# data <- data %>%
#   mutate(censoring = as.Date("2022-10-31"))


## estimate survival time --------------------------------------------------
# gallstone or removal of gallbladder
data <- data %>%
  mutate(
    survival_time_tmp = case_when(
      !is.na(icd10_gallstone_date) ~ as.numeric(difftime(icd10_gallstone_date, date_birth, units = "days")),
      !is.na(icd10_obstruction_date) ~ as.numeric(difftime(icd10_obstruction_date, date_birth, units = "days")),
      !is.na(icd9_gallstone_date) ~ as.numeric(difftime(icd9_gallstone_date, date_birth, units = "days")),
      !is.na(icd9_obstruction_date) ~ as.numeric(difftime(icd9_obstruction_date, date_birth, units = "days")),
      !is.na(opcs4_removal_date) ~ as.numeric(difftime(opcs4_removal_date, date_birth, units = "days")),
      !is.na(opcs4_gallstone_date) ~ as.numeric(difftime(opcs4_gallstone_date, date_birth, units = "days")),
      !is.na(opcs3_removal_date) ~ as.numeric(difftime(opcs3_removal_date, date_birth, units = "days")),
      !is.na(opcs3_gallstone_date) ~ as.numeric(difftime(opcs3_gallstone_date, date_birth, units = "days")),
      !is.na(date_of_death) ~ as.numeric(difftime(date_of_death, date_birth, units = "days")),
      !is.na(loss_to_follow_up) ~ as.numeric(difftime(loss_to_follow_up, date_birth, units = "days")),
      TRUE ~ as.numeric(difftime(censoring, date_birth, units = "days"))
    ),
    # Use min to get the minimum survival time across columns
    survival_gallstone = pmin(survival_time_tmp, na.rm = TRUE),
    survival_gallstone = survival_gallstone/365.25,
    # Remove temporary variable
    survival_time_tmp = NULL
  )

# binary variable to indicate if gallstone happened
data <- data %>%
  mutate(gallstone = case_when(
    !is.na(icd10_gallstone_date) | !is.na(icd10_obstruction_date) |
      !is.na(icd9_gallstone_date) | !is.na(icd9_obstruction_date) |
        !is.na(opcs4_removal_date) | !is.na(opcs4_gallstone_date) |
        !is.na(opcs3_removal_date) | !is.na(opcs3_gallstone_date) ~ 1,
    TRUE ~ 0))

# cholecystitis
data <- data %>%
    mutate(
        survival_time_tmp = case_when(
            !is.na(icd10_cholecystit_date) ~ as.numeric(difftime(icd10_cholecystit_date, date_birth, units = "days")),
            !is.na(icd9_acute_date) ~ as.numeric(difftime(icd9_acute_date, date_birth, units = "days")),
            !is.na(icd9_other_date) ~ as.numeric(difftime(icd9_other_date, date_birth, units = "days")),
            !is.na(date_of_death) ~ as.numeric(difftime(date_of_death, date_birth, units = "days")),
            !is.na(loss_to_follow_up) ~ as.numeric(difftime(loss_to_follow_up, date_birth, units = "days")),
            TRUE ~ as.numeric(difftime(censoring, date_birth, units = "days"))
        ),
        # Use min to get the minimum survival time across columns
        survival_cholecystit = pmin(survival_time_tmp, na.rm = TRUE),
        survival_cholecystit = survival_cholecystit/365.25,
        # Remove temporary variable
        survival_time_tmp = NULL
    )

# binary variable to indicate if gallstone happened
data <- data %>%
    mutate(cholecystit = case_when(
        !is.na(icd10_cholecystit_date) | !is.na(icd9_acute_date) |
            !is.na(icd9_other_date) ~ 1,
        TRUE ~ 0))

# all GBD
data <- data %>%
    mutate(
        survival_time_tmp = case_when(
            !is.na(icd10_gallstone_date) ~ as.numeric(difftime(icd10_gallstone_date, date_birth, units = "days")),
            !is.na(icd10_obstruction_date) ~ as.numeric(difftime(icd10_obstruction_date, date_birth, units = "days")),
            !is.na(icd9_gallstone_date) ~ as.numeric(difftime(icd9_gallstone_date, date_birth, units = "days")),
            !is.na(icd9_obstruction_date) ~ as.numeric(difftime(icd9_obstruction_date, date_birth, units = "days")),
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
        survival_gbd = pmin(survival_time_tmp, na.rm = TRUE),
        survival_gbd = survival_gbd/365.25,
        # Remove temporary variable
        survival_time_tmp = NULL
    )

# binary variable to indicate if gbd happened
data <- data %>%
    mutate(gbd = case_when(
        !is.na(icd10_gallstone_date) | !is.na(icd10_obstruction_date) |
            !is.na(icd9_gallstone_date) | !is.na(icd9_obstruction_date) |
            !is.na(opcs4_removal_date) | !is.na(opcs4_gallstone_date) |
            !is.na(opcs3_removal_date) | !is.na(opcs3_gallstone_date) |
            !is.na(icd10_cholecystit_date) | !is.na(icd9_acute_date) |
            !is.na(icd9_other_date) ~ 1,
        TRUE ~ 0))



# Remove those with outcome before baseline (=last webQ) --------
# time of last completed 24h recall as baseline date
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
           ques_comp_t4 = substr(ques_comp_t4, 1, 10)
    )


# New column with baseline start date as last completed questionnaire
data <- data %>%
    # Convert ques_0 to ques_4 to date format
    mutate(across(starts_with("ques_"), as.Date)) %>%
    # Gather all columns into key-value pairs
    pivot_longer(cols = starts_with("ques_"), names_to = "questionnaire", values_to = "date_filled") %>%
    # Group by participant ID and select the row with the latest date_filled for each participant
    group_by(id) %>%
    slice(which.max(date_filled)) %>%
    ungroup() %>%
    # Rename the remaining column to indicate the last filled questionnaire
    rename(last_filled_questionnaire = questionnaire)


remove <- c("p105010_i0", "p105010_i1", "p105010_i2", "p105010_i3","p105010_i4")
data <- data %>%
    select(-matches(remove))

data <- data %>%
    mutate(already_diagnosed = case_when(
        icd10_gallstone_date < date_filled | icd10_obstruction_date < date_filled |
            icd9_gallstone_date < date_filled | icd9_obstruction_date < date_filled |
            opcs4_removal_date < date_filled | opcs4_gallstone_date < date_filled |
            opcs3_removal_date < date_filled | opcs3_gallstone_date < date_filled |
            icd10_cholecystit_date < date_filled | icd9_acute_date < date_filled |
            icd9_other_date < date_filled ~ 1,
        TRUE ~ NA_character_
        ))

data <- data %>%
    subset(already_diagnosed != 1)

# Save data ---------------------------------------------------------------
arrow::write_parquet(data, here("data/data.parquet"))
ukbAid::upload_data(here("data/data.parquet"), username = "FieLangmann")
