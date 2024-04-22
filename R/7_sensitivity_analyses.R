#7. Sensitivity analyses

#Load packages
library(tidyverse)
library(Hmisc)
library(survival)
library(gtsummary)
library(ggsurvfit)
library(ggplot2)
library(dplyr)
library(tidyr)
library(here)
library(broom)


# Load data --------------------------------------------------------
targets::tar_make()
# Restart session
source(here::here("R/1_data_start.R"))


# Legumes including peas; substitution models ------------------------------
# Weekly substituting 80 g legumes including peas (NHS 1 portion beans = 80 g)
# https://www.nhs.uk/live-well/eat-well/5-a-day/5-a-day-what-counts/
# defining 80 g/week variable for each food
data <- data %>%
  mutate(legumepea80 = legume_pea_weekly/80,
         meats80 = meats_weekly/80,
         poultry80 = poultry_weekly/80,
         fish80 = fish_weekly/80)

## model 2 -----------------------------------------------------------------
# meats
meat_sensi2 <- coxph(Surv(survival_gbd, gbd == 1) ~
                       # removing meat
                       legumepea80 + poultry80 + fish80+
                       #other food components
                       cereal_refined_weekly + whole_grain_weekly + mixed_dish_weekly +
                       dairy_weekly + fats_weekly + fruit_weekly + nut_weekly +
                       veggie_weekly + potato_weekly + egg_weekly +
                       non_alc_beverage_weekly + alc_beverage_weekly + snack_weekly +
                       sauce_weekly + weight_weekly +
                       #other variables
                       age + region + sex +
                       alcohol_weekly + ethnicity + deprivation + education +
                       cohabitation + physical_activity + smoking +
                       related_disease + disease_family + yearly_income,
                     data = data, ties='breslow')

meat_sensi2 <- tidy(meat_sensi2, exponentiate = TRUE, conf.int = TRUE)

# poultry
poultry_sensi2 <- coxph(Surv(survival_gbd, gbd == 1) ~
                          # removing meat
                          legumepea80 + meats80 + fish80+
                          #other food components
                          cereal_refined_weekly + whole_grain_weekly + mixed_dish_weekly +
                          dairy_weekly + fats_weekly + fruit_weekly + nut_weekly +
                          veggie_weekly + potato_weekly + egg_weekly +
                          non_alc_beverage_weekly + alc_beverage_weekly + snack_weekly +
                          sauce_weekly + weight_weekly +
                          #other variables
                          age + region + sex +
                          alcohol_weekly + ethnicity + deprivation + education +
                          cohabitation + physical_activity + smoking +
                          related_disease + disease_family + yearly_income,
                        data = data, ties='breslow')

poultry_sensi2 <- tidy(poultry_sensi2, exponentiate = TRUE, conf.int = TRUE)



# fish
fish_sensi2 <- coxph(Surv(survival_gbd, gbd == 1) ~
                       # removing meat
                       legumepea80 + meats80 + poultry80+
                       #other food components
                       cereal_refined_weekly + whole_grain_weekly + mixed_dish_weekly +
                       dairy_weekly + fats_weekly + fruit_weekly + nut_weekly +
                       veggie_weekly + potato_weekly + egg_weekly +
                       non_alc_beverage_weekly + alc_beverage_weekly + snack_weekly +
                       sauce_weekly + weight_weekly +
                       #other variables
                       age + region + sex +
                       alcohol_weekly + ethnicity + deprivation + education +
                       cohabitation + physical_activity + smoking +
                       related_disease + disease_family + yearly_income,
                     data = data, ties='breslow')

fish_sensi2 <- tidy(fish_sensi2, exponentiate = TRUE, conf.int = TRUE)




# Varying 24h recalls -----------------------------------------------------
data <- data %>%
  mutate(legumes80 = legume_weekly/80,
         meats80 = meats_weekly/80,
         poultry80 = poultry_weekly/80,
         fish80 = fish_weekly/80)
data3 <- data %>%
  subset(p20077>=3)
data4 <- data %>%
  subset(p20077>=4)
data5 <- data %>%
  subset(p20077>=5)

## data3 = three 24h recalls -----------------------------------------------------------------

# meats
meat_data3 <- coxph(Surv(survival_gbd, gbd == 1) ~
                       # removing meat
                       legumes80 + poultry80 + fish80+
                      #other food components
                      cereal_refined_weekly + whole_grain_weekly + mixed_dish_weekly +
                      dairy_weekly + fats_weekly + fruit_weekly + nut_weekly +
                      veggie_weekly + potato_weekly + egg_weekly +
                      non_alc_beverage_weekly + alc_beverage_weekly + snack_weekly +
                      sauce_weekly + weight_weekly +
                      #other variables
                      age + region + sex +
                      alcohol_weekly + ethnicity + deprivation + education +
                      cohabitation + physical_activity + smoking +
                      related_disease + disease_family + yearly_income,
                     data = data3, ties='breslow')

meat_data3 <- tidy(meat_data3, exponentiate = TRUE, conf.int = TRUE)

# poultry
poultry_data3 <- coxph(Surv(survival_gbd, gbd == 1) ~
                          # removing meat
                          legumes80 + meats80 + fish80+
                         #other food components
                         cereal_refined_weekly + whole_grain_weekly + mixed_dish_weekly +
                         dairy_weekly + fats_weekly + fruit_weekly + nut_weekly +
                         veggie_weekly + potato_weekly + egg_weekly +
                         non_alc_beverage_weekly + alc_beverage_weekly + snack_weekly +
                         sauce_weekly + weight_weekly +
                         #other variables
                         age + region + sex +
                         alcohol_weekly + ethnicity + deprivation + education +
                         cohabitation + physical_activity + smoking +
                         related_disease + disease_family + yearly_income,
                        data = data3, ties='breslow')

poultry_data3 <- tidy(poultry_data3, exponentiate = TRUE, conf.int = TRUE)

# fish
fish_data3 <- coxph(Surv(survival_gbd, gbd == 1) ~
                       # removing meat
                       legumes80 + meats80 + poultry80+
                      #other food components
                      cereal_refined_weekly + whole_grain_weekly + mixed_dish_weekly +
                      dairy_weekly + fats_weekly + fruit_weekly + nut_weekly +
                      veggie_weekly + potato_weekly + egg_weekly +
                      non_alc_beverage_weekly + alc_beverage_weekly + snack_weekly +
                      sauce_weekly + weight_weekly +
                      #other variables
                      age + region + sex +
                      alcohol_weekly + ethnicity + deprivation + education +
                      cohabitation + physical_activity + smoking +
                      related_disease + disease_family + yearly_income,
                     data = data3, ties='breslow')

fish_data3 <- tidy(fish_data3, exponentiate = TRUE, conf.int = TRUE)


## data4 = four 24h recalls -----------------------------------------------------------------
# meats
meat_data4 <- coxph(Surv(survival_gbd, gbd == 1) ~
                      # removing meat
                      legumes80 + poultry80 + fish80+
                      #other food components
                      cereal_refined_weekly + whole_grain_weekly + mixed_dish_weekly +
                      dairy_weekly + fats_weekly + fruit_weekly + nut_weekly +
                      veggie_weekly + potato_weekly + egg_weekly +
                      non_alc_beverage_weekly + alc_beverage_weekly + snack_weekly +
                      sauce_weekly + weight_weekly +
                      #other variables
                      age + region + sex +
                      alcohol_weekly + ethnicity + deprivation + education +
                      cohabitation + physical_activity + smoking +
                      related_disease + disease_family + yearly_income,
                    data = data4, ties='breslow')

meat_data4 <- tidy(meat_data4, exponentiate = TRUE, conf.int = TRUE)

# poultry
poultry_data4 <- coxph(Surv(survival_gbd, gbd == 1) ~
                         # removing meat
                         legumes80 + meats80 + fish80+
                         #other food components
                         cereal_refined_weekly + whole_grain_weekly + mixed_dish_weekly +
                         dairy_weekly + fats_weekly + fruit_weekly + nut_weekly +
                         veggie_weekly + potato_weekly + egg_weekly +
                         non_alc_beverage_weekly + alc_beverage_weekly + snack_weekly +
                         sauce_weekly + weight_weekly +
                         #other variables
                         age + region + sex +
                         alcohol_weekly + ethnicity + deprivation + education +
                         cohabitation + physical_activity + smoking +
                         related_disease + disease_family + yearly_income,
                       data = data4, ties='breslow')

poultry_data4 <- tidy(poultry_data4, exponentiate = TRUE, conf.int = TRUE)

# fish
fish_data4 <- coxph(Surv(survival_gbd, gbd == 1) ~
                      # removing meat
                      legumes80 + meats80 + poultry80+
                      #other food components
                      cereal_refined_weekly + whole_grain_weekly + mixed_dish_weekly +
                      dairy_weekly + fats_weekly + fruit_weekly + nut_weekly +
                      veggie_weekly + potato_weekly + egg_weekly +
                      non_alc_beverage_weekly + alc_beverage_weekly + snack_weekly +
                      sauce_weekly + weight_weekly +
                      #other variables
                      age + region + sex +
                      alcohol_weekly + ethnicity + deprivation + education +
                      cohabitation + physical_activity + smoking +
                      related_disease + disease_family + yearly_income,
                    data = data4, ties='breslow')

fish_data4 <- tidy(fish_data4, exponentiate = TRUE, conf.int = TRUE)




# # Removing high ALT and AST from analysis-----------------------------------------------
# # removing high ALT and AST
# data <- data %>% mutate(
#   alt_level = case_when(
#     alt <= 45 & sex == "Female" | alt <= 70 & sex == "Male" ~ 0,
#     alt > 45 & sex == "Female" | alt > 70 & sex == "Male" ~ 1
#   ),
#   ast_level = case_when(
#     ast <= 35 & sex == "Female" | ast <= 45 & sex == "Male" ~ 0,
#     ast > 35 & sex == "Female" | ast > 45 & sex == "Male" ~ 1
#   )
# )
#
# # Subsetting data
# normal_liver <- data %>%
#   subset(alt_level !=1 & ast_level != 1)
#
#
# ## Running main analysis on subsample --------------------------------------
# # Weekly substituting 80 g legumes (NHS 1 portion beans = 80 g) https://www.nhs.uk/live-well/eat-well/5-a-day/5-a-day-what-counts/
# # defining 80 g/week variable for each food
# normal_liver <- normal_liver %>%
#   mutate(legumes80 = legume_weekly/80,
#          meats80 = meats_weekly/80,
#          poultry80 = poultry_weekly/80,
#          fish80 = fish_weekly/80)
#
# # meats
# meat_liver <- coxph(Surv(survival_time, nafld == 1) ~
#                        # removing meat
#                        legumes80 + poultry80 + fish80+
#                        #other food components
#                        cereal_refined_weekly + whole_grain_weekly + mixed_dish_weekly +
#                        dairy_weekly + fats_weekly + fruit_weekly + nut_weekly +
#                        veggie_weekly + potato_weekly + egg_weekly +
#                        non_alc_beverage_weekly + alc_beverage_weekly + snack_weekly +
#                        sauce_weekly + weight_weekly +
#                        #other variables
#                        age + region + sex +
#                        alcohol_weekly + ethnicity + deprivation + education +
#                        cohabitation + physical_activity + smoking +
#                        related_disease + disease_family + yearly_income,
#                      data = normal_liver, ties='breslow')
#
# meat_liver <- tidy(meat_liver, exponentiate = TRUE, conf.int = TRUE)
#
#
# # poultry
# poultry_liver <- coxph(Surv(survival_time, nafld == 1) ~
#                           # removing meat
#                           legumes80 + meats80 + fish80+
#                           #other food components
#                           cereal_refined_weekly + whole_grain_weekly + mixed_dish_weekly +
#                           dairy_weekly + fats_weekly + fruit_weekly + nut_weekly +
#                           veggie_weekly + potato_weekly + egg_weekly +
#                           non_alc_beverage_weekly + alc_beverage_weekly + snack_weekly +
#                           sauce_weekly + weight_weekly +
#                           #other variables
#                           age + region + sex +
#                           alcohol_weekly + ethnicity + deprivation + education +
#                           cohabitation + physical_activity + smoking +
#                           related_disease + disease_family + yearly_income,
#                         data = normal_liver, ties='breslow')
#
# poultry_liver <- tidy(poultry_liver, exponentiate = TRUE, conf.int = TRUE)
#
#
# # fish
# fish_liver <- coxph(Surv(survival_time, nafld == 1) ~
#                        # removing meat
#                        legumes80 + meats80 + poultry80+
#                        #other food components
#                        cereal_refined_weekly + whole_grain_weekly + mixed_dish_weekly +
#                        dairy_weekly + fats_weekly + fruit_weekly + nut_weekly +
#                        veggie_weekly + potato_weekly + egg_weekly +
#                        non_alc_beverage_weekly + alc_beverage_weekly + snack_weekly +
#                        sauce_weekly + weight_weekly +
#                        #other variables
#                        age + region + sex +
#                        alcohol_weekly + ethnicity + deprivation + education +
#                        cohabitation + physical_activity + smoking +
#                        related_disease + disease_family + yearly_income,
#                      data = normal_liver, ties='breslow')
#
# fish_liver <- tidy(fish_liver, exponentiate = TRUE, conf.int = TRUE) )
#


# Remove high alcohol intake --------------------------------------------------
percentile_90 <- quantile(data$alcohol_weekly, probs = 0.90, na.rm = TRUE)
lower_alc <- data %>%
  subset(alcohol_weekly < percentile_90)

## main analysis model 2 on subset -----------------------------------------------------------------
# meats
meat_alc <- coxph(Surv(survival_gbd, gbd == 1) ~
                       # removing meat
                       legumes80 + poultry80 + fish80+
                       #other food components
                       cereal_refined_weekly + whole_grain_weekly + mixed_dish_weekly +
                       dairy_weekly + fats_weekly + fruit_weekly + nut_weekly +
                       veggie_weekly + potato_weekly + egg_weekly +
                       non_alc_beverage_weekly + alc_beverage_weekly + snack_weekly +
                       sauce_weekly + weight_weekly +
                       #other variables
                       age + region + sex +
                       alcohol_weekly + ethnicity + deprivation + education +
                       cohabitation + physical_activity + smoking +
                       related_disease + disease_family + yearly_income,
                     data = lower_alc, ties='breslow')

meat_alc <- tidy(meat_alc, exponentiate = TRUE, conf.int = TRUE)


# poultry
poultry_alc <- coxph(Surv(survival_gbd, gbd == 1) ~
                          # removing meat
                          legumes80 + meats80 + fish80+
                          #other food components
                          cereal_refined_weekly + whole_grain_weekly + mixed_dish_weekly +
                          dairy_weekly + fats_weekly + fruit_weekly + nut_weekly +
                          veggie_weekly + potato_weekly + egg_weekly +
                          non_alc_beverage_weekly + alc_beverage_weekly + snack_weekly +
                          sauce_weekly + weight_weekly +
                          #other variables
                          age + region + sex +
                          alcohol_weekly + ethnicity + deprivation + education +
                          cohabitation + physical_activity + smoking +
                          related_disease + disease_family + yearly_income,
                        data = lower_alc, ties='breslow')

poultry_alc <- tidy(poultry_alc, exponentiate = TRUE, conf.int = TRUE)


# fish
fish_alc <- coxph(Surv(survival_gbd, gbd == 1) ~
                       # removing meat
                       legumes80 + meats80 + poultry80+
                       #other food components
                       cereal_refined_weekly + whole_grain_weekly + mixed_dish_weekly +
                       dairy_weekly + fats_weekly + fruit_weekly + nut_weekly +
                       veggie_weekly + potato_weekly + egg_weekly +
                       non_alc_beverage_weekly + alc_beverage_weekly + snack_weekly +
                       sauce_weekly + weight_weekly +
                       #other variables
                       age + region + sex +
                       alcohol_weekly + ethnicity + deprivation + education +
                       cohabitation + physical_activity + smoking +
                       related_disease + disease_family + yearly_income,
                     data = lower_alc, ties='breslow')

fish_alc <- tidy(fish_alc, exponentiate = TRUE, conf.int = TRUE)
