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


# Legumes including peas; substitution models ------------------------------
# Weekly substituting 80 g legumes including peas (NHS 1 portion beans = 80 g)
# https://www.nhs.uk/live-well/eat-well/5-a-day/5-a-day-what-counts/
# defining 80 g/week variable for each food
data <- data %>%
  mutate(legumepea80 = legume_pea_weekly/80,
         meats80 = meats_weekly/80,
         poultry80 = poultry_weekly/80,
         fish80 = fish_weekly/80)

# any gallbladder outcome
data <- data %>% mutate(
  any_gbd = case_when(
    gbd == 1 | gbd == 2 ~ 1,
    TRUE ~ NA
  )
)

## any gallbladder disease -----------------------------------------------------------------
# meats
meat_sensi2 <- coxph(Surv(survival_gbd, any_gbd == 1) ~
                       # removing meat
                       legumepea80 + poultry80 + fish80+
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
                          sauce_weekly + food_weight_weekly +
                          #other variables
                          strata(age_strata, region, sex) +
                          ethnicity + deprivation + education + yearly_income +
                          cohabitation + physical_activity + smoking +
                          estrogen_treatment +
                          pregnancies + related_conditions + family_diabetes,
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
                       sauce_weekly + food_weight_weekly +
                       #other variables
                       strata(age_strata, region, sex) +
                       ethnicity + deprivation + education + yearly_income +
                       cohabitation + physical_activity + smoking +
                       estrogen_treatment +
                       pregnancies + related_conditions + family_diabetes,
                     data = data, ties='breslow')

fish_sensi2 <- tidy(fish_sensi2, exponentiate = TRUE, conf.int = TRUE)



## gallstone ---------------------------------------------------------------
# meats
meat_sensi2 <- coxph(Surv(survival_gallstone, gbd == 1) ~
                       # removing meat
                       legumepea80 + poultry80 + fish80+
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

meat_sensi2 <- tidy(meat_sensi2, exponentiate = TRUE, conf.int = TRUE)

# poultry
poultry_sensi2 <- coxph(Surv(survival_gallstone, gbd == 1) ~
                          # removing meat
                          legumepea80 + meats80 + fish80+
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

poultry_sensi2 <- tidy(poultry_sensi2, exponentiate = TRUE, conf.int = TRUE)

# fish
fish_sensi2 <- coxph(Surv(survival_gallstone, gbd == 1) ~
                       # removing meat
                       legumepea80 + meats80 + poultry80+
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

fish_sensi2 <- tidy(fish_sensi2, exponentiate = TRUE, conf.int = TRUE)



# Varying 24h recalls -----------------------------------------------------
data <- data %>%
  mutate(legumes80 = legumes_weekly/80,
         meats80 = meats_weekly/80,
         poultry80 = poultry_weekly/80,
         fish80 = fish_weekly/80)
data3 <- data %>%
  subset(p20077>=3)
data4 <- data %>%
  subset(p20077>=4)


## data3 = three 24h recalls -----------------------------------------------------------------
# any gallbladder disease
# meats
meat_data3 <- coxph(Surv(survival_gbd, any_gbd == 1) ~
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
                      cohabitation + physical_activity + smoking +
                      estrogen_treatment +
                      pregnancies + related_conditions + family_diabetes,
                    data = data3, ties='breslow')

meat_data3 <- tidy(meat_data3, exponentiate = TRUE, conf.int = TRUE)

# poultry
poultry_data3 <- coxph(Surv(survival_gbd, any_gbd == 1) ~
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
                       data = data3, ties='breslow')

poultry_data3 <- tidy(poultry_data3, exponentiate = TRUE, conf.int = TRUE)

# fish
fish_data3 <- coxph(Surv(survival_gbd, any_gbd == 1) ~
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
                    data = data3, ties='breslow')

fish_data3 <- tidy(fish_data3, exponentiate = TRUE, conf.int = TRUE)


# gallstone
# meats
meat_data3 <- coxph(Surv(survival_gallstone, gbd == 1) ~
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
                      cohabitation + physical_activity + smoking +
                      estrogen_treatment +
                      pregnancies + related_conditions + family_diabetes,
                    data = data3, ties='breslow')

meat_data3 <- tidy(meat_data3, exponentiate = TRUE, conf.int = TRUE)

# poultry
poultry_data3 <- coxph(Surv(survival_gallstone, gbd == 1) ~
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
                       data = data3, ties='breslow')

poultry_data3 <- tidy(poultry_data3, exponentiate = TRUE, conf.int = TRUE)

# fish
fish_data3 <- coxph(Surv(survival_gallstone, gbd == 1) ~
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
                    data = data3, ties='breslow')

fish_data3 <- tidy(fish_data3, exponentiate = TRUE, conf.int = TRUE)


## data4 = four 24h recalls -----------------------------------------------------------------
# any gallbladder disease
# meats
meat_data4 <- coxph(Surv(survival_gbd, any_gbd == 1) ~
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
                      cohabitation + physical_activity + smoking +
                      estrogen_treatment +
                      pregnancies + related_conditions + family_diabetes,
                    data = data4, ties='breslow')

meat_data4 <- tidy(meat_data4, exponentiate = TRUE, conf.int = TRUE)

# poultry
poultry_data4 <- coxph(Surv(survival_gbd, any_gbd == 1) ~
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
                       data = data4, ties='breslow')

poultry_data4 <- tidy(poultry_data4, exponentiate = TRUE, conf.int = TRUE)

# fish
fish_data4 <- coxph(Surv(survival_gbd, any_gbd == 1) ~
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
                    data = data4, ties='breslow')

fish_data4 <- tidy(fish_data4, exponentiate = TRUE, conf.int = TRUE)


# gallstone
# meats
meat_data4 <- coxph(Surv(survival_gallstone, gbd == 1) ~
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
                      cohabitation + physical_activity + smoking +
                      estrogen_treatment +
                      pregnancies + related_conditions + family_diabetes,
                    data = data4, ties='breslow')

meat_data4 <- tidy(meat_data4, exponentiate = TRUE, conf.int = TRUE)

# poultry
poultry_data4 <- coxph(Surv(survival_gallstone, gbd == 1) ~
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
                       data = data4, ties='breslow')

poultry_data4 <- tidy(poultry_data4, exponentiate = TRUE, conf.int = TRUE)

# fish
fish_data4 <- coxph(Surv(survival_gallstone, gbd == 1) ~
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
                    data = data4, ties='breslow')

fish_data4 <- tidy(fish_data4, exponentiate = TRUE, conf.int = TRUE)



# # Removing high bilirubin  from analysis-----------------------------------------------
# removing top 10 % (90th percentile) of bilirubin
percentile_90 <- quantile(data$bilirubin, probs = 0.90, na.rm = TRUE)
lower_bili <- data %>%
  subset(bilirubin < percentile_90)

## main analysis model 2 on subset -----------------------------------------------------------------
# any gallbladder disease
# meats
meat_bili <- coxph(Surv(survival_gbd, any_gbd == 1) ~
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
                      cohabitation + physical_activity + smoking +
                      estrogen_treatment +
                      pregnancies + related_conditions + family_diabetes,
                    data = lower_bili, ties='breslow')

meat_bili <- tidy(meat_bili, exponentiate = TRUE, conf.int = TRUE)

# poultry
poultry_bili <- coxph(Surv(survival_gbd, any_gbd == 1) ~
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
                       data = lower_bili, ties='breslow')

poultry_bili <- tidy(poultry_bili, exponentiate = TRUE, conf.int = TRUE)

# fish
fish_bili <- coxph(Surv(survival_gbd, any_gbd == 1) ~
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
                    data = lower_bili, ties='breslow')

fish_bili <- tidy(fish_bili, exponentiate = TRUE, conf.int = TRUE)


# gallstone
# meats
meat_bili <- coxph(Surv(survival_gallstone, gbd == 1) ~
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
                      cohabitation + physical_activity + smoking +
                      estrogen_treatment +
                      pregnancies + related_conditions + family_diabetes,
                    data = lower_bili, ties='breslow')

meat_bili <- tidy(meat_bili, exponentiate = TRUE, conf.int = TRUE)

# poultry
poultry_bili <- coxph(Surv(survival_gallstone, gbd == 1) ~
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
                       data = lower_bili, ties='breslow')

poultry_bili <- tidy(poultry_bili, exponentiate = TRUE, conf.int = TRUE)

# fish
fish_bili <- coxph(Surv(survival_gallstone, gbd == 1) ~
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
                    data = lower_bili, ties='breslow')

fish_bili <- tidy(fish_bili, exponentiate = TRUE, conf.int = TRUE)
