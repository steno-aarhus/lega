# Extra analyses ---------------------------------------------------------
## Gallstones only ---------------------------------------------------------
create_formula_gallstone <- function(xvars, covars) {
    outcome <- "Surv(survival_gallstone, gallstone == 1)"
    reformulate(c(xvars, covars), response = outcome)
}

gallstone_model2<- function(data) {
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

    model2_gallstone_formulas <- list(
        meat_model2 = create_formula_gallstone(c("legumes80", "poultry80", "fish80"), covars2),
        poultry_model2 = create_formula_gallstone(c("legumes80", "meats80", "fish80"), covars2),
        fish_model2 = create_formula_gallstone(c("legumes80", "meats80", "poultry80"), covars2)
    )

    model2_gallstone <- model2_gallstone_formulas |>
        map(~ coxph(.x, data = data, ties = "breslow")) |>
        map2(names(model2_gallstone_formulas), ~ tidy(.x, exponentiate = TRUE, conf.int = TRUE) |>
                 mutate(across(where(is.numeric), ~ round(.x, 2))) |>
                 mutate(model = .y))

    return(model2_gallstone)
}


## Cholecystit only --------------------------------------------------------
create_formula_cholecystit <- function(xvars, covars) {
    outcome <- "Surv(survival_gallstone, cholecystit == 1)"
    reformulate(c(xvars, covars), response = outcome)
}

cholecystit_model2<- function(data) {
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

    model2_cholecystit_formulas <- list(
        meat_model2 = create_formula_cholecystit(c("legumes80", "poultry80", "fish80"), covars2),
        poultry_model2 = create_formula_cholecystit(c("legumes80", "meats80", "fish80"), covars2),
        fish_model2 = create_formula_cholecystit(c("legumes80", "meats80", "poultry80"), covars2)
    )

    model2_cholecystit <- model2_cholecystit_formulas |>
        map(~ coxph(.x, data = data, ties = "breslow")) |>
        map2(names(model2_cholecystit_formulas), ~ tidy(.x, exponentiate = TRUE, conf.int = TRUE) |>
                 mutate(across(where(is.numeric), ~ round(.x, 2))) |>
                 mutate(model = .y))

    return(model2_cholecystit)
}

## Sex strata any GBD ---------------------------------------------------------
create_formula <- function(xvars, covars) {
    outcome <- "Surv(survival_gbd, gbd == 1)"
    reformulate(c(xvars, covars), response = outcome)
}

men_strata <- function(data) {
    men <- data %>%
        subset(sex == Male)
    covars2 <- c("cereal_refined_weekly", "whole_grain_weekly", "mixed_dish_weekly",
                 "dairy_weekly", "fats_weekly", "fruit_weekly", "nut_weekly",
                 "veggie_weekly", "potato_weekly", "egg_weekly",
                 "non_alc_beverage_weekly", "alc_beverage_weekly", "snack_weekly",
                 "sauce_weekly", "food_weight_weekly", "ethnicity",
                 "deprivation", "education", "cohabitation", "physical_activity",
                 "smoking", "estrogen_treatment", "bilirubin", "weight_loss",
                 "pregnancies", "yearly_income",
                 "related_conditions", "family_diabetes",
                 "strata(region, age_strata)")

    model2_formulas <- list(
        meat_model2 = create_formula(c("legumes80", "poultry80", "fish80"), covars2),
        poultry_model2 = create_formula(c("legumes80", "meats80", "fish80"), covars2),
        fish_model2 = create_formula(c("legumes80", "meats80", "poultry80"), covars2)
    )

    model2_men <- model2_formulas |>
        map(~ coxph(.x, data = men, ties = "breslow")) |>
        map2(names(model2_formulas), ~ tidy(.x, exponentiate = TRUE, conf.int = TRUE) |>
                 mutate(across(where(is.numeric), ~ round(.x, 2))) |>
                 mutate(model = .y))

    return(model2_men)
}

women_strata <- function(data) {
    women <- data %>%
        subset(sex == 0)
    covars2 <- c("cereal_refined_weekly", "whole_grain_weekly", "mixed_dish_weekly",
                 "dairy_weekly", "fats_weekly", "fruit_weekly", "nut_weekly",
                 "veggie_weekly", "potato_weekly", "egg_weekly",
                 "non_alc_beverage_weekly", "alc_beverage_weekly", "snack_weekly",
                 "sauce_weekly", "food_weight_weekly", "ethnicity",
                 "deprivation", "education", "cohabitation", "physical_activity",
                 "smoking", "estrogen_treatment", "bilirubin", "weight_loss",
                 "pregnancies", "yearly_income",
                 "related_conditions", "family_diabetes",
                 "strata(region, age_strata)")

    model2_formulas <- list(
        meat_model2 = create_formula(c("legumes80", "poultry80", "fish80"), covars2),
        poultry_model2 = create_formula(c("legumes80", "meats80", "fish80"), covars2),
        fish_model2 = create_formula(c("legumes80", "meats80", "poultry80"), covars2)
    )

    model2_results <- model2_formulas |>
        map(~ coxph(.x, data = women, ties = "breslow")) |>
        map2(names(model2_formulas), ~ tidy(.x, exponentiate = TRUE, conf.int = TRUE) |>
                 mutate(across(where(is.numeric), ~ round(.x, 2))) |>
                 mutate(model = .y))

    return(model2_results)
}

## Age strata any GBD ---------------------------------------------------------
age_strata <- function(data) {
    covars2 <- c("cereal_refined_weekly", "whole_grain_weekly", "mixed_dish_weekly",
                 "dairy_weekly", "fats_weekly", "fruit_weekly", "nut_weekly",
                 "veggie_weekly", "potato_weekly", "egg_weekly",
                 "non_alc_beverage_weekly", "alc_beverage_weekly", "snack_weekly",
                 "sauce_weekly", "food_weight_weekly", "ethnicity",
                 "deprivation", "education", "cohabitation", "physical_activity",
                 "smoking", "estrogen_treatment", "bilirubin", "weight_loss",
                 "pregnancies", "yearly_income",
                 "related_conditions", "family_diabetes",
                 "strata(region, sex)")

    model2_formulas <- list(
        meat_model2 = create_formula(c("legumes80", "poultry80", "fish80"), covars2),
        poultry_model2 = create_formula(c("legumes80", "meats80", "fish80"), covars2),
        fish_model2 = create_formula(c("legumes80", "meats80", "poultry80"), covars2)
    )

    # Function to run analysis for a given age group
    run_analysis <- function(age_strata) {
        subset_data <- data %>%
            subset(age == age_strata)

        results <- model2_formulas |>
            map(~ coxph(.x, data = subset_data, ties = "breslow")) |>
            map2(names(model2_formulas), ~ tidy(.x, exponentiate = TRUE, conf.int = TRUE) |>
                     mutate(across(where(is.numeric), ~ round(.x, 2))) |>
                     mutate(model = .y, sex = ifelse(sex_group == 1, "men", "women")))

        return(results)
    }

    # Apply the analysis for both sexes
    all_results <- map(0:5, run_analysis) |>
        bind_rows()

    return(all_results)
}



## BMI strata any GBD ---------------------------------------------------------
bmi_strata<- function(data) {

    data <- data %>%
        mutate(bmi_strata =
                   case_when(bmi < 25 ~ 0,
                             bmi >= 25 & bmi < 30 ~ 1,
                             bmi > 30 ~ 2))

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

    # Function to run analysis for a given bmi group
    run_analysis <- function(age_strata) {
        subset_data <- data %>%
            subset(bmi == age_strata)

        results <- model2_formulas |>
            map(~ coxph(.x, data = subset_data, ties = "breslow")) |>
            map2(names(model2_formulas), ~ tidy(.x, exponentiate = TRUE, conf.int = TRUE) |>
                     mutate(across(where(is.numeric), ~ round(.x, 2))) |>
                     mutate(model = .y, sex = ifelse(sex_group == 1, "men", "women")))

        return(results)
    }

    # Apply the analysis for both sexes
    all_results <- map(0:2, run_analysis) |>
        bind_rows()

    return(all_results)
}


