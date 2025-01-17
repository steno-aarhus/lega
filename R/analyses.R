# Analyses

create_formula <- function(xvars, covars) {
    outcome <- "Surv(survival_gbd, gbd == 1)"
    reformulate(c(xvars, covars), response = outcome)
}

# Main analyses -----------------------------------------------------------
main_model1 <- function(data) {
    covars1 <- c("cereal_refined_weekly", "whole_grain_weekly", "mixed_dish_weekly",
                 "dairy_weekly", "fats_weekly", "fruit_weekly", "nut_weekly",
                 "veggie_weekly", "potato_weekly", "egg_weekly",
                 "non_alc_beverage_weekly", "alc_beverage_weekly", "snack_weekly",
                 "sauce_weekly", "food_weight_weekly", "strata(region, age_strata, sex)")


    model1_formulas <- list(
        meat_model1 = create_formula(c("legumes80", "poultry80", "fish80"), covars1),
        poultry_model1 = create_formula(c("legumes80", "meats80", "fish80"), covars1),
        fish_model1 = create_formula(c("legumes80", "meats80", "poultry80"), covars1)
    )

    model1_results <- model1_formulas |>
        map(~ survival::coxph(.x, data = data, ties = "breslow")) |>
        map2(names(model1_formulas), ~ tidy(.x, exponentiate = TRUE, conf.int = TRUE) |>
                 mutate(across(where(is.numeric), ~ round(.x, 2))) |>
                 mutate(model = .y))

    return(model1_results)
}

main_model2<- function(data) {
    covars2 <- c("cereal_refined_weekly", "whole_grain_weekly", "mixed_dish_weekly",
                 "dairy_weekly", "fats_weekly", "fruit_weekly", "nut_weekly",
                 "veggie_weekly", "potato_weekly", "egg_weekly",
                 "non_alc_beverage_weekly", "alc_beverage_weekly", "snack_weekly",
                 "sauce_weekly", "food_weight_weekly", "ethnicity",
                 "deprivation", "education", "cohabitation", "physical_activity",
                 "smoking", "estrogen_treatment", "bilirubin", "weight_loss",
                 "pregnancies", "yearly_income",
                 "related_conditions", "family_diabetes",
                 "strata(region, age_strata, sex)")

    model2_formulas <- list(
        meat_model2 = create_formula(c("legumes80", "poultry80", "fish80"), covars2),
        poultry_model2 = create_formula(c("legumes80", "meats80", "fish80"), covars2),
        fish_model2 = create_formula(c("legumes80", "meats80", "poultry80"), covars2)
    )

    model2_results <- model2_formulas |>
        map(~ survival::coxph(.x, data = data, ties = "breslow")) |>
        map2(names(model2_formulas), ~ tidy(.x, exponentiate = TRUE, conf.int = TRUE) |>
                 mutate(across(where(is.numeric), ~ round(.x, 2))) |>
                 mutate(model = .y))

    return(model2_results)
}


main_model3<- function(data) {
    covars3 <- c("cereal_refined_weekly", "whole_grain_weekly", "mixed_dish_weekly",
                 "dairy_weekly", "fats_weekly", "fruit_weekly", "nut_weekly",
                 "veggie_weekly", "potato_weekly", "egg_weekly",
                 "non_alc_beverage_weekly", "alc_beverage_weekly", "snack_weekly",
                 "sauce_weekly", "food_weight_weekly", "ethnicity",
                 "deprivation", "education", "cohabitation", "physical_activity",
                 "smoking", "estrogen_treatment", "bilirubin", "weight_loss",
                 "pregnancies", "yearly_income",
                 "related_conditions", "family_diabetes", "bmi30",
                 "strata(region, age_strata, sex)")


    model3_formulas <- list(
        meat_model3 = create_formula(c("legumes80", "poultry80", "fish80"), covars3),
        poultry_model3 = create_formula(c("legumes80", "meats80", "fish80"), covars3),
        fish_model3 = create_formula(c("legumes80", "meats80", "poultry80"), covars3)
    )

    model3_results <- model3_formulas |>
        map(~ coxph(.x, data = data, ties = "breslow")) |>
        map2(names(model3_formulas), ~ tidy(.x, exponentiate = TRUE, conf.int = TRUE) |>
                 mutate(across(where(is.numeric), ~ round(.x, 2))) |>
                 mutate(model = .y))
    return(model3_results)
}

# secondary analyses
consumers_analyses<- function(data) {
    consumers <- data %>%
        subset(legume_weekly > 0)

    covars2 <- c("cereal_refined_weekly", "whole_grain_weekly", "mixed_dish_weekly",
                 "dairy_weekly", "fats_weekly", "fruit_weekly", "nut_weekly",
                 "veggie_weekly", "potato_weekly", "egg_weekly",
                 "non_alc_beverage_weekly", "alc_beverage_weekly", "snack_weekly",
                 "sauce_weekly", "food_weight_weekly", "ethnicity",
                 "deprivation", "education", "cohabitation", "physical_activity",
                 "smoking", "estrogen_treatment", "bilirubin", "weight_loss",
                 "pregnancies", "yearly_income",
                 "related_conditions", "family_diabetes",
                 "strata(region, age_strata, sex)")


    model2_formulas <- list(
        meat_model2 = create_formula(c("legumes80", "poultry80", "fish80"), covars2),
        poultry_model2 = create_formula(c("legumes80", "meats80", "fish80"), covars2),
        fish_model2 = create_formula(c("legumes80", "meats80", "poultry80"), covars2)
    )

    model2_results_consumers <- model2_formulas |>
        map(~ coxph(.x, data = consumers, ties = "breslow")) |>
        map2(names(model2_formulas), ~ tidy(.x, exponentiate = TRUE, conf.int = TRUE) |>
                 mutate(across(where(is.numeric), ~ round(.x, 2))) |>
                 mutate(model = .y))

    return(model2_results_consumers)
}



total_intake <- function(data) {
    consumers <- data %>%
        subset(legume_weekly > 0)

    total_model2 <- coxph(
        Surv(survival_gbd, gbd == 1) ~ legumes80 +
            # other food components
            cereal_refined_weekly + whole_grain_weekly + mixed_dish_weekly +
            dairy_weekly + fats_weekly + fruit_weekly + nut_weekly +
            veggie_weekly + potato_weekly + egg_weekly +
            non_alc_beverage_weekly + alc_beverage_weekly + snack_weekly +
            sauce_weekly + meats_weekly + poultry_weekly + fish_weekly +
            # other variables
            ethnicity + deprivation + education +
            cohabitation + physical_activity + smoking + bilirubin + weight_loss +
            related_conditions + family_diabetes + yearly_income + estrogen_treatment +
            pregnancies + strata(region, age_strata, sex),
        data = consumers, ties = "breslow"
    )
    total <- tidy(total_model2, exponentiate = TRUE, conf.int = TRUE) %>%
        mutate(across(where(is.numeric), ~ round(.x, 2)))
    return(total)
}

# sensitivity analyses ----------------------------------------------------
# peas included in legume component
legumes_and_peas<- function(data) {
    covariates_pea <- c("cereal_refined_weekly", "whole_grain_weekly", "mixed_dish_weekly",
                        "dairy_weekly", "fats_weekly", "fruit_weekly", "nut_weekly",
                        "veggie_pea_weekly", "potato_weekly", "egg_weekly",
                        "non_alc_beverage_weekly", "alc_beverage_weekly", "snack_weekly",
                        "sauce_weekly", "food_weight_weekly", "ethnicity",
                        "deprivation", "education", "cohabitation", "physical_activity",
                        "smoking", "estrogen_treatment", "bilirubin", "weight_loss",
                        "pregnancies", "yearly_income",
                        "related_conditions", "family_diabetes",
                        "strata(region, age_strata, sex)")

    model2_formulas_pea <- list(
        meat_model2 = create_formula(c("legumepea80", "poultry80", "fish80"), covariates_pea),
        poultry_model2 = create_formula(c("legumepea80", "meats80", "fish80"), covariates_pea),
        fish_model2 = create_formula(c("legumepea80", "meats80", "poultry80"), covariates_pea)
    )

    model2_results_pea <- model2_formulas_pea |>
        map(~ coxph(.x, data = data, ties = "breslow")) |>
        map2(names(model2_formulas_pea), ~ tidy(.x, exponentiate = TRUE, conf.int = TRUE) |>
                 mutate(across(where(is.numeric), ~ round(.x, 2))) |>
                 mutate(model = .y))

    return(model2_results_pea)
}

# soy excluded from legume component
legumes_without_soy<- function(data) {
    data <- data %>%
        mutate(
            legumenosoy80 = legume_no_soymilk / 80,
            meats80 = meats_weekly / 80,
            poultry80 = poultry_weekly / 80,
            fish80 = fish_weekly / 80
        )


    covariates_nosoy <- c("cereal_refined_weekly", "whole_grain_weekly", "mixed_dish_weekly",
                          "dairy_weekly", "fats_weekly", "fruit_weekly", "nut_weekly",
                          "veggie_weekly", "potato_weekly", "egg_weekly",
                          "non_alc_beverage_soymilk_weekly", "alc_beverage_weekly", "snack_weekly",
                          "sauce_weekly", "food_weight_weekly", "ethnicity",
                          "deprivation", "education", "cohabitation", "physical_activity",
                          "smoking", "estrogen_treatment", "bilirubin", "weight_loss",
                          "pregnancies", "yearly_income",
                          "related_conditions", "family_diabetes",
                          "strata(region, age_strata, sex)")

    model2_formulas_nosoy <- list(
        meat_model2 = create_formula(c("legumenosoy80", "poultry80", "fish80"), covariates_nosoy),
        poultry_model2 = create_formula(c("legumenosoy80", "meats80", "fish80"), covariates_nosoy),
        fish_model2 = create_formula(c("legumenosoy80", "meats80", "poultry80"), covariates_nosoy)
    )

    model2_results_nosoy <- model2_formulas_nosoy |>
        map(~ coxph(.x, data = data, ties = "breslow")) |>
        map2(names(model2_formulas_nosoy), ~ tidy(.x, exponentiate = TRUE, conf.int = TRUE) |>
                 mutate(across(where(is.numeric), ~ round(.x, 2))) |>
                 mutate(model = .y))

    return(model2_results_nosoy)
}



# Varying 24h recalls -----------------------------------------------------

three_recalls_analyses <- function(data) {
    data3 <- data %>%
        subset(p20077>=3)
    covars2 <- c("cereal_refined_weekly", "whole_grain_weekly", "mixed_dish_weekly",
                 "dairy_weekly", "fats_weekly", "fruit_weekly", "nut_weekly",
                 "veggie_weekly", "potato_weekly", "egg_weekly",
                 "non_alc_beverage_weekly", "alc_beverage_weekly", "snack_weekly",
                 "sauce_weekly", "food_weight_weekly", "ethnicity",
                 "deprivation", "education", "cohabitation", "physical_activity",
                 "smoking", "estrogen_treatment", "bilirubin", "weight_loss",
                 "pregnancies", "yearly_income",
                 "related_conditions", "family_diabetes",
                 "strata(region, age_strata, sex)")


    model2_formulas <- list(
        meat_model2 = create_formula(c("legumes80", "poultry80", "fish80"), covars2),
        poultry_model2 = create_formula(c("legumes80", "meats80", "fish80"), covars2),
        fish_model2 = create_formula(c("legumes80", "meats80", "poultry80"), covars2)
    )

    model2_results_3recalls <- model2_formulas |>
        map(~ coxph(.x, data = data3, ties = "breslow")) |>
        map2(names(model2_formulas), ~ tidy(.x, exponentiate = TRUE, conf.int = TRUE) |>
                 mutate(across(where(is.numeric), ~ round(.x, 2))) |>
                 mutate(model = .y))

    return(model2_results_3recalls)
}


# Excuding high bilirubin levels
# removing top 10 % (90th percentile) of bilirubin
#
bilirubin_analyses<- function(data) {
percentile_90 <- quantile(data$bilirubin, probs = 0.90, na.rm = TRUE)
lower_bili <- data %>%
    subset(bilirubin < percentile_90)

covars2 <- c("cereal_refined_weekly", "whole_grain_weekly", "mixed_dish_weekly",
             "dairy_weekly", "fats_weekly", "fruit_weekly", "nut_weekly",
             "veggie_weekly", "potato_weekly", "egg_weekly",
             "non_alc_beverage_weekly", "alc_beverage_weekly", "snack_weekly",
             "sauce_weekly", "food_weight_weekly", "ethnicity",
             "deprivation", "education", "cohabitation", "physical_activity",
             "smoking", "estrogen_treatment", "bilirubin", "weight_loss",
             "pregnancies", "yearly_income",
             "related_conditions", "family_diabetes",
             "strata(region, age_strata, sex)")

    model2_formulas <- list(
        meat_model2 = create_formula(c("legumes80", "poultry80", "fish80"), covars2),
        poultry_model2 = create_formula(c("legumes80", "meats80", "fish80"), covars2),
        fish_model2 = create_formula(c("legumes80", "meats80", "poultry80"), covars2)
    )

    model2_results_bili <- model2_formulas |>
        map(~ coxph(.x, data = lower_bili, ties = "breslow")) |>
        map2(names(model2_formulas), ~ tidy(.x, exponentiate = TRUE, conf.int = TRUE) |>
                 mutate(across(where(is.numeric), ~ round(.x, 2))) |>
                 mutate(model = .y))

    return(model2_results_bili)
}


# Red and processed meats analysed separately

meat_separate<- function(data) {
    covars2 <- c("cereal_refined_weekly", "whole_grain_weekly", "mixed_dish_weekly",
                 "dairy_weekly", "fats_weekly", "fruit_weekly", "nut_weekly",
                 "veggie_weekly", "potato_weekly", "egg_weekly",
                 "non_alc_beverage_weekly", "alc_beverage_weekly", "snack_weekly",
                 "sauce_weekly", "food_weight_weekly", "ethnicity",
                 "deprivation", "education", "cohabitation", "physical_activity",
                 "smoking", "estrogen_treatment", "bilirubin", "weight_loss",
                 "pregnancies", "yearly_income",
                 "related_conditions", "family_diabetes",
                 "strata(region, age_strata, sex)")

    model2_formulas <- list(
        red_model2 = create_formula(c("legumes80", "poultry80", "fish80", "processed_weekly"), covars2),
        processed_model2 = create_formula(c("legumes80", "poultry80", "fish80", "red_weekly"), covars2),
    )

    model2_results <- model2_formulas |>
        map(~ survival::coxph(.x, data = data, ties = "breslow")) |>
        map2(names(model2_formulas), ~ tidy(.x, exponentiate = TRUE, conf.int = TRUE) |>
                 mutate(across(where(is.numeric), ~ round(.x, 2))) |>
                 mutate(model = .y))

    return(model2_results)
}

