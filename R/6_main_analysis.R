#6. Main analyses

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
library(splines)
library(kableExtra)
library(broom)

# Load data --------------------------------------------------------

# Weekly substituting 80 g legumes (NHS 1 portion beans = 80 g) https://www.nhs.uk/live-well/eat-well/5-a-day/5-a-day-what-counts/
# defining 80 g/week variable for each food
data <- data %>%
    mutate(legumes80 = legumes_weekly/80,
           meats80 = meats_weekly/80,
           poultry80 = poultry_weekly/80,
           fish80 = fish_weekly/80)


# Gallbladder disease ----------------------------------------------------
# estimate total follow-up time for GBD

# estimate median follow-up time for GBD

# define outcome
data <- data %>% mutate(
    any_gbd = case_when(
        gbd == 1 | gbd == 2 ~ 1,
        TRUE ~ NA
    )
)

## Model 1 ----------------------------------------------------------
# meats
meat_model1 <- coxph(Surv(survival_gbd, any_gbd == 1) ~
                       # removing meat
                       legumes80 + poultry80 + fish80+
                         #other food components
                         cereal_refined_weekly + whole_grain_weekly + mixed_dish_weekly +
                         dairy_weekly + fats_weekly + fruit_weekly + nut_weekly +
                         veggie_weekly + potato_weekly + egg_weekly +
                         non_alc_beverage_weekly + alc_beverage_weekly + snack_weekly +
                       sauce_weekly + food_weight_weekly + strata(age_strata, region, sex),
                     data = data, ties='breslow')

meat_model1 <- tidy(meat_model1, exponentiate = TRUE, conf.int = TRUE)

# poultry
poultry_model1 <- coxph(Surv(survival_gbd, any_gbd == 1) ~
                          # removing meat
                          legumes80 + meats80 + fish80+
                         #other food components
                         cereal_refined_weekly + whole_grain_weekly + mixed_dish_weekly +
                         dairy_weekly + fats_weekly + fruit_weekly + nut_weekly +
                         veggie_weekly + potato_weekly + egg_weekly +
                         non_alc_beverage_weekly + alc_beverage_weekly + snack_weekly +
                          sauce_weekly + food_weight_weekly + strata(age_strata, region, sex),
                        data = data, ties='breslow')

poultry_model1 <- tidy(poultry_model1, exponentiate = TRUE, conf.int = TRUE)

# fish
fish_model1 <- coxph(Surv(survival_gbd, any_gbd == 1) ~
                       # removing meat
                       legumes80 + meats80 + poultry80+
                         #other food components
                         cereal_refined_weekly + whole_grain_weekly + mixed_dish_weekly +
                         dairy_weekly + fats_weekly + fruit_weekly + nut_weekly +
                         veggie_weekly + potato_weekly + egg_weekly +
                         non_alc_beverage_weekly + alc_beverage_weekly + snack_weekly +
                       sauce_weekly + food_weight_weekly + strata(age_strata, region, sex),
                     data = data, ties='breslow')

fish_model1 <- tidy(fish_model1, exponentiate = TRUE, conf.int = TRUE)


# model 2 -----------------------------------------------------------------
# meats
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
                         cohabitation + physical_activity + smoking +
                         hrt + oral_contraceptive +
                         pregnancies + related_conditions + family_diabetes,
                     data = data, ties='breslow')

meat_model2 <- tidy(meat_model2, exponentiate = TRUE, conf.int = TRUE)

# poultry
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
                            hrt + oral_contraceptive +
                            pregnancies + related_conditions + family_diabetes,
                        data = data, ties='breslow')

poultry_model2 <- tidy(poultry_model2, exponentiate = TRUE, conf.int = TRUE)



# fish
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
                         hrt + oral_contraceptive +
                         pregnancies + related_conditions + family_diabetes,
                     data = data, ties='breslow')

fish_model2 <- tidy(fish_model2, exponentiate = TRUE, conf.int = TRUE)




# model 3 -----------------------------------------------------------------
# meats
meat_model3 <- coxph(Surv(survival_gbd, any_gbd == 1) ~
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
                         hrt + oral_contraceptive +
                         pregnancies + related_conditions + family_diabetes + bmi30,
                     data = data, ties='breslow')

meat_model3 <- tidy(meat_model3, exponentiate = TRUE, conf.int = TRUE)


# poultry
poultry_model3 <- coxph(Surv(survival_gbd, any_gbd == 1) ~
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
                            hrt + oral_contraceptive +
                            pregnancies + related_conditions + family_diabetes + bmi30,
                        data = data, ties='breslow'

poultry_model3 <- tidy(poultry_model3, exponentiate = TRUE, conf.int = TRUE)



# fish
fish_model3 <- coxph(Surv(survival_gbd, any_gbd == 1) ~
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
                         hrt + oral_contraceptive +
                         pregnancies + related_conditions + family_diabetes + bmi30,
                     data = data, ties='breslow'

fish_model3 <- tidy(fish_model3, exponentiate = TRUE, conf.int = TRUE)




# Gallstones only ---------------------------------------------------------

# estimate total follow-up time for gallstone

# estimate median follow-up time for gallstone


## Model 1 ----------------------------------------------------------
# meats
meat_model1 <- coxph(Surv(survival_gallstone, gbd == 1) ~
                         # removing meat
                         legumes80 + poultry80 + fish80+
                         #other food components
                         cereal_refined_weekly + whole_grain_weekly + mixed_dish_weekly +
                         dairy_weekly + fats_weekly + fruit_weekly + nut_weekly +
                         veggie_weekly + potato_weekly + egg_weekly +
                         non_alc_beverage_weekly + alc_beverage_weekly + snack_weekly +
                         sauce_weekly + food_weight_weekly + strata(age_strata, region, sex),
                     data = data, ties='breslow')

meat_model1 <- tidy(meat_model1, exponentiate = TRUE, conf.int = TRUE)

# poultry
poultry_model1 <- coxph(Surv(survival_gallstone, gbd == 1) ~
                            # removing meat
                            legumes80 + meats80 + fish80+
                            #other food components
                            cereal_refined_weekly + whole_grain_weekly + mixed_dish_weekly +
                            dairy_weekly + fats_weekly + fruit_weekly + nut_weekly +
                            veggie_weekly + potato_weekly + egg_weekly +
                            non_alc_beverage_weekly + alc_beverage_weekly + snack_weekly +
                            sauce_weekly + food_weight_weekly + strata(age_strata, region, sex),
                        data = data, ties='breslow')

poultry_model1 <- tidy(poultry_model1, exponentiate = TRUE, conf.int = TRUE)

# fish
fish_model1 <- coxph(Surv(survival_gallstone, gbd == 1) ~
                         # removing meat
                         legumes80 + meats80 + poultry80+
                         #other food components
                         cereal_refined_weekly + whole_grain_weekly + mixed_dish_weekly +
                         dairy_weekly + fats_weekly + fruit_weekly + nut_weekly +
                         veggie_weekly + potato_weekly + egg_weekly +
                         non_alc_beverage_weekly + alc_beverage_weekly + snack_weekly +
                         sauce_weekly + food_weight_weekly + strata(age_strata, region, sex),
                     data = data, ties='breslow')

fish_model1 <- tidy(fish_model1, exponentiate = TRUE, conf.int = TRUE)


# model 2 -----------------------------------------------------------------
# meats
meat_model2 <- coxph(Surv(survival_gallstone, gbd == 1) ~
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
                     data = data, ties='breslow')

meat_model2 <- tidy(meat_model2, exponentiate = TRUE, conf.int = TRUE)

# poultry
poultry_model2 <- coxph(Surv(survival_gallstone, gbd == 1) ~
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
                            hrt + oral_contraceptive +
                            pregnancies + related_conditions + family_diabetes,
                        data = data, ties='breslow')

poultry_model2 <- tidy(poultry_model2, exponentiate = TRUE, conf.int = TRUE)



# fish
fish_model2 <- coxph(Surv(survival_gallstone, gbd == 1) ~
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
                         hrt + oral_contraceptive +
                         pregnancies + related_conditions + family_diabetes,
                     data = data, ties='breslow')

fish_model2 <- tidy(fish_model2, exponentiate = TRUE, conf.int = TRUE)




# model 3 -----------------------------------------------------------------
# meats
meat_model3 <- coxph(Surv(survival_gallstone, gbd == 1) ~
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
                         hrt + oral_contraceptive +
                         pregnancies + related_conditions + family_diabetes + bmi30,
                     data = data, ties='breslow')

meat_model3 <- tidy(meat_model3, exponentiate = TRUE, conf.int = TRUE)


# poultry
poultry_model3 <- coxph(Surv(survival_gallstone, gbd == 1) ~
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
                            hrt + oral_contraceptive +
                            pregnancies + related_conditions + family_diabetes + bmi30,
                        data = data, ties='breslow')

poultry_model3 <- tidy(poultry_model3, exponentiate = TRUE, conf.int = TRUE)



# fish
fish_model3 <- coxph(Surv(survival_gallstone, gbd == 1) ~
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
                         hrt + oral_contraceptive +
                         pregnancies + related_conditions + family_diabetes + bmi30,
                     data = data, ties='breslow')

fish_model3 <- tidy(fish_model3, exponentiate = TRUE, conf.int = TRUE)
