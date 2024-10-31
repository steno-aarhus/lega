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
    Surv(survival_time, gbd == 1) ~
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
    Surv(survival_time, gbd == 1) ~
      # removing meat
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
    Surv(survival_time, gbd == 1) ~
      # removing meat
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
