#8. Secondary analyses

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
library(eventglm)


# Pseudo observational method ---------------------------------------------
# using survival model from main analysis
# defining 80 g/week variable for each food
data <- data %>%
  mutate(legumes80 = legume_weekly/80,
         meats80 = meats_weekly/80,
         poultry80 = poultry_weekly/80,
         fish80 = fish_weekly/80)


# meats
fit_meat <- eventglm::cumincglm(Surv(time, gbd == 1) ~
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
                                time = 2, data = data)

fit_meat <- tidy(fit_meat, exponentiate = FALSE, conf.int = TRUE, digits = 2)


# poultry
fit_poultry <- eventglm::cumincglm(Surv(time, gbd == 1) ~
                                     # removing poultry
                                     legumes80 + meats80 + fish80+
                                     #other food components
                                     cereal_refined_weekly + whole_grain_weekly + mixed_dish_weekly +
                                     dairy_weekly + fats_weekly + fruit_weekly + nut_weekly +
                                     veggie_weekly + potato_weekly + egg_weekly  +
                                     non_alc_beverage_weekly + alc_beverage_weekly + snack_weekly +
                                     sauce_weekly + weight_weekly +
                                     #other variables
                                     age + region + sex +
                                     alcohol_weekly + ethnicity + deprivation + education +
                                     cohabitation + physical_activity + smoking +
                                     related_disease + disease_family + yearly_income,
                                   time = 2, data = data)

fit_poultry <- tidy(fit_poultry, exponentiate = FALSE, conf.int = TRUE, digits = 2)

# fish
fit_fish <- eventglm::cumincglm(Surv(time, gbd == 1) ~
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
                                time = 2, data = data)

fit_fish <- tidy(fit_fish, exponentiate = FALSE, conf.int = TRUE, digits = 2)






# Non specific substitutions ----------------------------------------------
# Leaving one portion of legumes (80g) out weekly
data <- data %>%
  mutate(legumes80 = legume_weekly/80)


## model 2 -----------------------------------------------------------------
#alcohol spline
nonspecific_model2 <- coxph(Surv(survival_gbd, gbd == 1) ~ legumes80 +
                              weight_weekly + age + region + sex +
                              alcohol_weekly + ethnicity + deprivation + education +
                              cohabitation + physical_activity + smoking +
                              related_disease + disease_family + yearly_income,
                            data = data, ties='breslow')

nonspecific_model2 <- tidy(nonspecific_model2, exponentiate = TRUE, conf.int = TRUE)


