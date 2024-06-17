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


# Non specific substitutions ----------------------------------------------
# Leaving one portion of legumes (80g) out weekly
data <- data %>%
  mutate(legumes80 = legumes_weekly/80)
data <- data %>% mutate(
    any_gbd = case_when(
        gbd == 1 | gbd == 2 ~ 1,
        TRUE ~ NA
    )
)

## any gbd -----------------------------------------------------------------
nonspecific_gbd <- coxph(Surv(survival_gbd, any_gbd == 1) ~ legumes80 +
                              food_weight_weekly +
                                strata(age_strata, region, sex) +
                                ethnicity + deprivation + education + yearly_income +
                                cohabitation + physical_activity + smoking +
                             estrogen_treatment +
                                pregnancies + related_conditions + family_diabetes,
                            data = data, ties='breslow')

nonspecific_gbd <- tidy(nonspecific_gbd, exponentiate = TRUE, conf.int = TRUE)


# gallstone ---------------------------------------------------------------

nonspecific_gallstone <- coxph(Surv(survival_gbd, gbd == 1) ~ legumes80 +
                             food_weight_weekly +
                             strata(age_strata, region, sex) +
                             ethnicity + deprivation + education + yearly_income +
                             cohabitation + physical_activity + smoking +
                                 estrogen_treatment +
                             pregnancies + related_conditions + family_diabetes,
                         data = data, ties='breslow')

nonspecific_gallstone <- tidy(nonspecific_gallstone, exponentiate = TRUE, conf.int = TRUE)


# Total legume  -----------------------------------------------------------

# consuming 80 g legumes more weekly independent of other foods consumed
# (non-substitution effect)

# any gallbladder disease
total_gbd <- coxph(Surv(survival_gbd, any_gbd == 1) ~ legumes80 +
                         # removing meat
                         poultry80 + fish80+
                         #other food components
                             meats_weekly + poultry_weekly + fish_weekly +
                         cereal_refined_weekly + whole_grain_weekly + mixed_dish_weekly +
                         dairy_weekly + fats_weekly + fruit_weekly + nut_weekly +
                         veggie_weekly + potato_weekly + egg_weekly +
                         non_alc_beverage_weekly + alc_beverage_weekly + snack_weekly +
                         sauce_weekly +
                         #other variables
                         strata(age_strata, region, sex) +
                         ethnicity + deprivation + education + yearly_income +
                         cohabitation + physical_activity + smoking +
                             estrogen_treatment +
                         pregnancies + related_conditions + family_diabetes,
                     data = data, ties='breslow')

total_gbd <- tidy(total_gbd, exponentiate = TRUE, conf.int = TRUE)

# gallstone
total_gallstone <- coxph(Surv(survival_gbd, gbd == 1) ~ legumes80 +
                   # removing meat
                   poultry80 + fish80+
                       #other food components
                       meats_weekly + poultry_weekly + fish_weekly +
                       cereal_refined_weekly + whole_grain_weekly + mixed_dish_weekly +
                       dairy_weekly + fats_weekly + fruit_weekly + nut_weekly +
                       veggie_weekly + potato_weekly + egg_weekly +
                       non_alc_beverage_weekly + alc_beverage_weekly + snack_weekly +
                       sauce_weekly +
                       #other variables
                       strata(age_strata, region, sex) +
                       ethnicity + deprivation + education + yearly_income +
                       cohabitation + physical_activity + smoking +
                       estrogen_treatment +
                       pregnancies + related_conditions + family_diabetes,
                   data = data, ties='breslow')

total_gallstone <- tidy(total_gallstone, exponentiate = TRUE, conf.int = TRUE)
