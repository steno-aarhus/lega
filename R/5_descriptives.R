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
