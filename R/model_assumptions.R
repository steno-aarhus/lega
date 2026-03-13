# Model assumptions proportional hazards ------------------------------------------
# Schoenfeld residuals
model_assumption <- function(data) {
  data <- data %>%
    mutate(
      legumes80 = legume_weekly / 80,
      meats80 = meats_weekly / 80,
      poultry80 = poultry_weekly / 80,
      fish80 = fish_weekly / 80
    )
  meat_model2 <- coxph(
    Surv(survival_gbd, gbd == 1) ~
      # removing meat
      legumes80 + poultry80 + fish80 +
      # other food components
      cereal_refined_weekly + whole_grain_weekly + mixed_dish_weekly +
      dairy_weekly + fats_weekly + fruit_weekly + nut_weekly +
      veggie_weekly + potato_weekly + egg_weekly +
      non_alc_beverage_weekly + alc_beverage_weekly + snack_weekly +
      sauce_weekly + food_weight_weekly +
      # other variables
      ethnicity + deprivation + education +
      cohabitation + physical_activity + smoking +
      estrogen_treatment + bilirubin + weight_loss +
      pregnancies + related_conditions + family_diabetes +
      yearly_income + strata(region, age_strata, sex),
    data = data, ties = "breslow"
  )

  ph_test1 <- cox.zph(meat_model2)
  print(ph_test1)

  # poultry
  poultry_model2 <- coxph(
    Surv(survival_gbd, gbd == 1) ~
      # removing poultry
      legumes80 + meats80 + fish80 +
      # other food components
      cereal_refined_weekly + whole_grain_weekly + mixed_dish_weekly +
      dairy_weekly + fats_weekly + fruit_weekly + nut_weekly +
      veggie_weekly + potato_weekly + egg_weekly +
      non_alc_beverage_weekly + alc_beverage_weekly + snack_weekly +
      sauce_weekly + food_weight_weekly +
      # other variables
      ethnicity + deprivation + education +
      cohabitation + physical_activity + smoking +
      estrogen_treatment + bilirubin + weight_loss +
      pregnancies + related_conditions + family_diabetes +
      yearly_income + strata(region, age_strata, sex),
    data = data, ties = "breslow"
  )

  ph_test2 <- cox.zph(poultry_model2)
  print(ph_test2)

  # fish
  fish_model2 <- coxph(
    Surv(survival_gbd, gbd == 1) ~
      # removing fish
      legumes80 + meats80 + poultry80 +
      # other food components
      cereal_refined_weekly + whole_grain_weekly + mixed_dish_weekly +
      dairy_weekly + fats_weekly + fruit_weekly + nut_weekly +
      veggie_weekly + potato_weekly + egg_weekly +
      non_alc_beverage_weekly + alc_beverage_weekly + snack_weekly +
      sauce_weekly + food_weight_weekly +
      # other variables
      ethnicity + deprivation + education +
      cohabitation + physical_activity + smoking +
      estrogen_treatment + bilirubin + weight_loss +
      pregnancies + related_conditions + family_diabetes +
      yearly_income + strata(region, age_strata, sex),
    data = data, ties = "breslow"
  )

  ph_test3 <- cox.zph(fish_model2) %>% print()
  return(data)
}


# data <- data %>%
#    mutate(
#        legumes80 = legume_weekly / 80,
#        meats80 = meats_weekly / 80,
#        poultry80 = poultry_weekly / 80,
#        fish80 = fish_weekly / 80
#    )
# meat_model2 <- coxph(
#    Surv(survival_gbd, gbd == 1) ~
#        # removing meat
#        legumes80 + poultry80 + fish80 +
#       # other food components
#        cereal_refined_weekly + whole_grain_weekly + mixed_dish_weekly +
#        dairy_weekly + fats_weekly + fruit_weekly + nut_weekly +
#        veggie_weekly + potato_weekly + egg_weekly +
#        non_alc_beverage_weekly + alc_beverage_weekly + snack_weekly +
#        sauce_weekly + food_weight_weekly +
#        # other variables
#        ethnicity + deprivation + education +
#        physical_activity + smoking +
#        estrogen_treatment + bilirubin + weight_loss +
#        yearly_income +
#       pregnancies + related_conditions + family_diabetes,
#    data = data, ties = "breslow"
# )

# ph_test1 <- cox.zph(meat_model2)
# print(ph_test1)
# plot(ph_test1)

# # poultry
# poultry_model2 <- coxph(
#    Surv(survival_gbd, gbd == 1) ~
#        # removing meat
#        legumes80 + meats80 + fish80 +
#        # other food components
#        cereal_refined_weekly + whole_grain_weekly + mixed_dish_weekly +
#        dairy_weekly + fats_weekly + fruit_weekly + nut_weekly +
#        veggie_weekly + potato_weekly + egg_weekly +
#        non_alc_beverage_weekly + alc_beverage_weekly + snack_weekly +
#        sauce_weekly + food_weight_weekly +
#        # other variables
#        ethnicity + deprivation + education +
#        physical_activity + smoking +
#        estrogen_treatment + bilirubin + weight_loss +
#        yearly_income +
#        pregnancies + related_conditions + family_diabetes,
#    data = data, ties = "breslow"
# )

# ph_test2 <- cox.zph(poultry_model2)
# print(ph_test2)
# plot(ph_test2)

# # fish
# fish_model2 <- coxph(
#    Surv(survival_gbd, gbd == 1) ~
#        # removing meat
#        legumes80 + meats80 + poultry80 +
#        # other food components
#        cereal_refined_weekly + whole_grain_weekly + mixed_dish_weekly +
#        dairy_weekly + fats_weekly + fruit_weekly + nut_weekly +
#        veggie_weekly + potato_weekly + egg_weekly +
#        non_alc_beverage_weekly + alc_beverage_weekly + snack_weekly +
#        sauce_weekly + food_weight_weekly +
#        # other variables
#        ethnicity + deprivation + education +
#        physical_activity + smoking +
#        estrogen_treatment + bilirubin + weight_loss +
#        yearly_income +
#        pregnancies + related_conditions + family_diabetes,
#    data = data, ties = "breslow"
# )

# ph_test3 <- cox.zph(fish_model2)
# print(ph_test3)
# plot(ph_test3)




# Testing linearity assumption --------------------------------------------
# investigating the association between weekly intakes of legumes, red and processed meat,
# poultry, and fish and NAFLD for non-linearity using restricted cubic splines at the
# 10th, 50th and 90th percentiles as proposed in: https://pubmed.ncbi.nlm.nih.gov/37694448/
library(dplyr)
library(magrittr)
library(tidyverse)
library(survival)
library(splines)
library(broom)

data <- targets::tar_read(sorted_data)

# Calculate the percentiles for the knots
knots_legume <- quantile(data$legume_weekly, probs = c(0.1, 0.5, 0.9), na.rm = TRUE)
knots_meat <- quantile(data$meats_weekly, probs = c(0.1, 0.5, 0.9), na.rm = TRUE)
knots_poultry <- quantile(data$poultry_weekly, probs = c(0.1, 0.5, 0.9), na.rm = TRUE)
knots_fish <- quantile(data$fish_weekly, probs = c(0.1, 0.5, 0.9), na.rm = TRUE)

# Generate the spline basis with specified knots
data <- data %>% mutate(
    legume_spline = splines::bs(legume_weekly, knots = knots_legume, degree = 3),
    meat_spline = splines::bs(meats_weekly, knots = knots_meat, degree = 3),
    poultry_spline = splines::bs(poultry_weekly, knots = knots_poultry, degree = 3),
    fish_spline = splines::bs(fish_weekly, knots = knots_fish, degree = 3)
)


## legumes -----------------------------------------------------------------
legume <- coxph(Surv(survival_gbd, event = gbd) ~ legume_weekly + ethnicity
                + deprivation + education + cohabitation + physical_activity + smoking
                + estrogen_treatment + bilirubin + weight_loss + pregnancies
                + related_conditions + family_diabetes + yearly_income + strata(region, age_strata, sex),
                data = data,
                ties = 'breslow')


legume_splines <- coxph(Surv(survival_gbd, event = gbd) ~ legume_spline + ethnicity
                        + deprivation + education + cohabitation + physical_activity + smoking
                        + estrogen_treatment + bilirubin + weight_loss + pregnancies
                        + related_conditions + family_diabetes + yearly_income + strata(region, age_strata, sex),
                        data = data,
                        ties = 'breslow')

# Perform a likelihood ratio test
lrt_result <- anova(legume, legume_splines, test = "LRT") %>% print() # p = 0.39

## meat -----------------------------------------------------------------
meat <- coxph(Surv(survival_gbd, event = gbd) ~ meats_weekly + ethnicity
              + deprivation + education + cohabitation + physical_activity + smoking
              + estrogen_treatment + bilirubin + weight_loss + pregnancies
              + related_conditions + family_diabetes + yearly_income + strata(region, age_strata, sex),
              data = data,
              ties = 'breslow')

meat_splines <- coxph(Surv(survival_gbd, event = gbd) ~ meat_spline + ethnicity
                      + deprivation + education + cohabitation + physical_activity + smoking
                      + estrogen_treatment + bilirubin + weight_loss + pregnancies
                      + related_conditions + family_diabetes + yearly_income + strata(region, age_strata, sex),
                      data = data,
                      ties = 'breslow')

# Perform a likelihood ratio test
lrt_result <- anova(meat, meat_splines, test = "LRT") %>% print() # p = 0.97

## poultry -----------------------------------------------------------------
poultry <- coxph(Surv(survival_gbd, event = gbd) ~ poultry_weekly + ethnicity
                 + deprivation + education + cohabitation + physical_activity + smoking
                 + estrogen_treatment + bilirubin + weight_loss + pregnancies
                 + related_conditions + family_diabetes + yearly_income + strata(region, age_strata, sex),
                 data = data,
                 ties = 'breslow')

poultry_splines <- coxph(Surv(survival_gbd, event = gbd) ~ poultry_spline + ethnicity
                         + deprivation + education + cohabitation + physical_activity + smoking
                         + estrogen_treatment + bilirubin + weight_loss + pregnancies
                         + related_conditions + family_diabetes + yearly_income + strata(region, age_strata, sex),
                         data = data,
                         ties = 'breslow')

# Perform a likelihood ratio test
lrt_result <- anova(poultry, poultry_splines, test = "LRT") %>% print() # p = 0.61

## fish -----------------------------------------------------------------
fish <- coxph(Surv(survival_gbd, event = gbd) ~ fish_weekly + ethnicity
              + deprivation + education + cohabitation + physical_activity + smoking
              + estrogen_treatment + bilirubin + weight_loss + pregnancies
              + related_conditions + family_diabetes + yearly_income + strata(region, age_strata, sex),
              data = data,
              ties = 'breslow')

fish_splines <- coxph(Surv(survival_gbd, event = gbd) ~ fish_spline + ethnicity
                      + deprivation + education + cohabitation + physical_activity + smoking
                      + estrogen_treatment + bilirubin + weight_loss + pregnancies
                      + related_conditions + family_diabetes + yearly_income + strata(region, age_strata, sex),
                      data = data,
                      ties = 'breslow')

# Perform a likelihood ratio test
lrt_result <- anova(fish, fish_splines, test = "LRT") %>% print() # p = 0.59
