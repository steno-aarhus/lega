#3. Descriptive

# Load packages -----------------------------------------------------------
library(tidyverse)
library(dplyr)
library(magrittr)
library(tidyr)
library(splines)
library(gtsummary)
library(flextable)

# Load data ---------------------------------------------------------------

# counting number of gallstone (1) and cholecystit (2) cases
data <- data %>% mutate(
  gbd = case_when(gbd==1 ~ "gallstone",
                  gbd==2 ~ "cholecystit",
                  gbd==0 ~ "no event")
)
table(data$gbd)
# 3295 gallstone, 446 cholecystit cases


# Table 1 -----------------------------------------------------------------

# baseline characteristics across legume intakes
# subset to only include consumers
consumers <- data %>%
  subset(legumes_weekly != 0)
# define and print the tertile of legume consumption among consumers
cons <- consumers %>%
  mutate(legume_tert = ntile(legumes_weekly, 3))
tertile_breaks <- quantile(cons$legumes_weekly, probs = seq(0, 1, 1/3))
print(tertile_breaks)

# include the tertiles in the dataframe
data <- data %>% mutate(
  legume_groups = case_when(legumes_weekly == 0 ~ 0,
                            legumes_weekly >0 & legumes_weekly < 163.3 ~ 1,
                            legumes_weekly >= 163.3 & legumes_weekly < 358.75 ~ 2,
                            legumes_weekly >= 358.75 ~ 3
  )
)

# making the descriptive table
table1 <- data %>%
  select(legume_groups, legumes_weekly, gbd, age, sex, yearly_income, education,
         deprivation, cohabitation, ethnicity, physical_activity, smoking,
         region, bmi30, hrt, oral_contraceptive, pregnancies, related_conditions, family_diabetes,
         meats_weekly, poultry_weekly, fish_weekly,
         cereal_refined_weekly, whole_grain_weekly, mixed_dish_weekly,
         dairy_weekly, fats_weekly, fruit_weekly, nut_weekly, veggie_weekly,
         potato_weekly, egg_weekly, non_alc_beverage_weekly,
         alc_beverage_weekly, snack_weekly, sauce_weekly, food_weight_weekly) %>%
  tbl_summary(by = legume_groups,
              statistic = list(all_continuous() ~  "{median} ({p10}, {p90})",
                               all_categorical() ~ "{n} ({p}%)"),
              digits = all_continuous() ~ 1,
              missing_text = "n missing") %>%
  add_overall() %>%
  bold_labels() %>%
  modify_caption("Table 1. Baseline characteristics across consumption of legumes in the UK Biobank cohort") %>%
  as_flex_table()

flextable::save_as_html(table1, path = here("doc", "table1.html"))


# Supplementary Table 2 -----------------------------------------------------------------
suppl_table2 <- data %>%
  select(gbd, age, sex, yearly_income, education,
         deprivation, cohabitation, ethnicity, physical_activity, smoking,
         region, bmi30, hrt, oral_contraceptive, pregnancies, related_conditions, family_diabetes) %>%
  tbl_summary(by = gbd,
              statistic = list(all_continuous() ~  "{median} ({p10}, {p90})",
                               all_categorical() ~ "{n} ({p}%)"),
              digits = all_continuous() ~ 1,
              missing_text = "n missing") %>%
  add_overall() %>%
  bold_labels() %>%
  modify_caption("Supplementary Table 2. Baseline characteristics of participants across incident gbd in the UK Biobank Cohort") %>%
  as_flex_table()

flextable::save_as_html(suppl_table2, path = here("doc", "suppl_table2.html"))



# Estimate follow-up time -------------------------------------------------

# all gallbladder diseases
# defining time in study
data <- data %>%
  mutate(
    survival_icd10_gallstone_date = case_when(
      !is.na(icd10_gallstone_date) ~ as.numeric(difftime(icd10_gallstone_date, date_filled, units = "days")),
      TRUE ~ NA),
    survival_loss_to_follow_up = case_when(
      !is.na(loss_to_follow_up) ~ as.numeric(difftime(loss_to_follow_up, date_filled, units = "days")),
      TRUE ~ NA),
    survival_date_of_death = case_when(
      !is.na(date_of_death) ~ as.numeric(difftime(date_of_death, date_filled, units = "days")),
      TRUE ~ NA),
    survival_opcs3_gallstone_date = case_when(
      !is.na(opcs3_gallstone_date) ~ as.numeric(difftime(opcs3_gallstone_date, date_filled, units = "days")),
      TRUE ~ NA),
    survival_opcs3_removal_date = case_when(
      !is.na(opcs3_removal_date) ~ as.numeric(difftime(opcs3_removal_date, date_filled, units = "days")),
      TRUE ~ NA),
    survival_opcs4_gallstone_date = case_when(
      !is.na(opcs4_gallstone_date) ~ as.numeric(difftime(opcs4_gallstone_date, date_filled, units = "days")),
      TRUE ~ NA),
    survival_opcs4_removal_date = case_when(
      !is.na(opcs4_removal_date) ~ as.numeric(difftime(opcs4_removal_date, date_filled, units = "days")),
      TRUE ~ NA),
    survival_icd9_bileobstruction_date = case_when(
      !is.na(icd9_bileobstruction_date) ~ as.numeric(difftime(icd9_bileobstruction_date, date_filled, units = "days")),
      TRUE ~ NA),
    survival_icd9_gallstone_date = case_when(
      !is.na(icd9_gallstone_date) ~ as.numeric(difftime(icd9_gallstone_date, date_filled, units = "days")),
      TRUE ~ NA),
    survival_icd10_gbobstruction_date = case_when(
      !is.na(icd10_gbobstruction_date) ~ as.numeric(difftime(icd10_gbobstruction_date, date_filled, units = "days")),
      TRUE ~ NA),
    survival_icd10_bileobstruction_date = case_when(
      !is.na(icd10_bileobstruction_date) ~ as.numeric(difftime(icd10_bileobstruction_date, date_filled, units = "days")),
      TRUE ~ NA),
    survival_icd10_cholecystit_date = case_when(
      !is.na(icd10_cholecystit_date) ~ as.numeric(difftime(icd10_cholecystit_date, date_filled, units = "days")),
      TRUE ~ NA),
    survival_icd9_acute_date = case_when(
      !is.na(icd9_acute_date) ~ as.numeric(difftime(icd9_acute_date, date_filled, units = "days")),
      TRUE ~ NA),
    survival_icd9_other_date = case_when(
      !is.na(icd9_other_date) ~ as.numeric(difftime(icd9_other_date, date_filled, units = "days")),
      TRUE ~ NA),
    survival_time_cenc = difftime(censoring, date_filled, units = "days"),
    gbd_time = pmin(survival_icd10_gallstone_date, survival_time_cenc,
                survival_icd10_bileobstruction_date, survival_icd10_gbobstruction_date,
                survival_icd9_gallstone_date, survival_icd9_bileobstruction_date,
                survival_opcs4_removal_date, survival_opcs3_removal_date,
                survival_opcs3_gallstone_date, survival_date_of_death,
                survival_loss_to_follow_up, survival_icd10_cholecystit_date,
                survival_icd9_acute_date, survival_icd9_other_date,
                na.rm = TRUE),
    gbd_time = gbd_time/365.25
  )

# total follow-up time for GBD
sum(data$gbd_time) #1,238,698 years
# median follow-up time for GBD
summary(data$gbd_time) #10.48 (IQR: 10.42-10.92) years


# for gallstone
# defining time in study
data <- data %>%
  mutate(
    survival_icd10_gallstone_date = case_when(
      !is.na(icd10_gallstone_date) ~ as.numeric(difftime(icd10_gallstone_date, date_filled, units = "days")),
      TRUE ~ NA),
    survival_loss_to_follow_up = case_when(
      !is.na(loss_to_follow_up) ~ as.numeric(difftime(loss_to_follow_up, date_filled, units = "days")),
      TRUE ~ NA),
    survival_date_of_death = case_when(
      !is.na(date_of_death) ~ as.numeric(difftime(date_of_death, date_filled, units = "days")),
      TRUE ~ NA),
    survival_opcs3_gallstone_date = case_when(
      !is.na(opcs3_gallstone_date) ~ as.numeric(difftime(opcs3_gallstone_date, date_filled, units = "days")),
      TRUE ~ NA),
    survival_opcs3_removal_date = case_when(
      !is.na(opcs3_removal_date) ~ as.numeric(difftime(opcs3_removal_date, date_filled, units = "days")),
      TRUE ~ NA),
    survival_opcs4_gallstone_date = case_when(
      !is.na(opcs4_gallstone_date) ~ as.numeric(difftime(opcs4_gallstone_date, date_filled, units = "days")),
      TRUE ~ NA),
    survival_opcs4_removal_date = case_when(
      !is.na(opcs4_removal_date) ~ as.numeric(difftime(opcs4_removal_date, date_filled, units = "days")),
      TRUE ~ NA),
    survival_icd9_bileobstruction_date = case_when(
      !is.na(icd9_bileobstruction_date) ~ as.numeric(difftime(icd9_bileobstruction_date, date_filled, units = "days")),
      TRUE ~ NA),
    survival_icd9_gallstone_date = case_when(
      !is.na(icd9_gallstone_date) ~ as.numeric(difftime(icd9_gallstone_date, date_filled, units = "days")),
      TRUE ~ NA),
    survival_icd10_gbobstruction_date = case_when(
      !is.na(icd10_gbobstruction_date) ~ as.numeric(difftime(icd10_gbobstruction_date, date_filled, units = "days")),
      TRUE ~ NA),
    survival_icd10_bileobstruction_date = case_when(
      !is.na(icd10_bileobstruction_date) ~ as.numeric(difftime(icd10_bileobstruction_date, date_filled, units = "days")),
      TRUE ~ NA),
    survival_time_cenc = difftime(censoring, date_filled, units = "days"),
    gallstone_time = pmin(survival_icd10_gallstone_date, survival_time_cenc,
                survival_icd10_bileobstruction_date, survival_icd10_gbobstruction_date,
                survival_icd9_gallstone_date, survival_icd9_bileobstruction_date,
                survival_opcs4_removal_date, survival_opcs3_removal_date,
                survival_opcs3_gallstone_date, survival_date_of_death,
                survival_loss_to_follow_up, na.rm = TRUE),
    gallstone_time = gallstone_time/365.25
  )

# total follow-up time for gallstone
sum(data$gallstone_time) #1,239,148 years
# median follow-up time for gallstone
summary(data$gallstone_time) # 10.48 (IQR: 10.42-10.92) years
