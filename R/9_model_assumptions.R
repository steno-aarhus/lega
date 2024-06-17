# model assumptions: proportional hazards
# Schoenfeld residuals

# Load packages
library(tidyverse)
library(Hmisc)
library(survival)
library(ggsurvfit)
library(dplyr)
library(here)


# Load data
data <- readr::read_csv("data/data_lega.csv")


# defining 80 g/week variable for each food
data <- data %>%
    mutate(legumes80 = legumes_weekly/80,
           meats80 = meats_weekly/80,
           poultry80 = poultry_weekly/80,
           fish80 = fish_weekly/80)

# define gbd outcome
data <- data %>% mutate(
    any_gbd = case_when(
        gbd == 1 | gbd == 2 ~ 1,
        TRUE ~ NA
    )
)

# meat substitution
meat_model2 <- coxph(Surv(survival_gbd, any_gbd == 1) ~
                         # removing meat
                         legumes80 + poultry80 + fish80+
                         #other food components
                         cereal_refined_weekly + whole_grain_weekly + mixed_dish_weekly +
                         dairy_weekly + fats_weekly + fruit_weekly + nut_weekly +
                         veggie_weekly + potato_weekly + egg_weekly +
                         non_alc_beverage_weekly + alc_beverage_weekly + snack_weekly +
                         sauce_weekly + food_weight_weekly +
                         #other variables
                         strata(age_strata, region, sex) +
                         ethnicity + deprivation + education + yearly_income +
                         cohabitation  + physical_activity + smoking +
                         estrogen_treatment +
                         pregnancies + related_conditions + family_diabetes,
                     data = data, ties='breslow')

ph_test1 <- cox.zph(meat_model2)
print(ph_test1)

# poultry substitution
poultry_model2 <- coxph(Surv(survival_gbd, any_gbd == 1) ~
                            # removing meat
                            legumes80 + meats80 + fish80+
                            #other food components
                            cereal_refined_weekly + whole_grain_weekly + mixed_dish_weekly +
                            dairy_weekly + fats_weekly + fruit_weekly + nut_weekly +
                            veggie_weekly + potato_weekly + egg_weekly +
                            non_alc_beverage_weekly + alc_beverage_weekly + snack_weekly +
                            sauce_weekly + food_weight_weekly +
                            #other variables
                            strata(age_strata, region, sex) +
                            ethnicity + deprivation + education + yearly_income +
                            cohabitation + physical_activity + smoking +
                            estrogen_treatment +
                            pregnancies + related_conditions + family_diabetes,
                        data = data, ties='breslow')

ph_test2 <- cox.zph(poultry_model2)
print(ph_test2)

# fish substitution
fish_model2 <- coxph(Surv(survival_gbd, any_gbd == 1) ~
                         # removing meat
                         legumes80 + meats80 + poultry80+
                         #other food components
                         cereal_refined_weekly + whole_grain_weekly + mixed_dish_weekly +
                         dairy_weekly + fats_weekly + fruit_weekly + nut_weekly +
                         veggie_weekly + potato_weekly + egg_weekly +
                         non_alc_beverage_weekly + alc_beverage_weekly + snack_weekly +
                         sauce_weekly + food_weight_weekly +
                         #other variables
                         strata(age_strata, region, sex) +
                         ethnicity + deprivation + education + yearly_income +
                         cohabitation + physical_activity + smoking +
                         estrogen_treatment +
                         pregnancies + related_conditions + family_diabetes,
                     data = data, ties='breslow')

ph_test3 <- cox.zph(fish_model2)
print(ph_test3)
