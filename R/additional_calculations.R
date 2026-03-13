# Additional calculations


# Unknown family disease history ------------------------------------------

# Confounder issues -------------------------------------------------------
library(magrittr)
library(dplyr)
library(survival)
library(broom)
library(purrr)

# How many unknown family disease history?
# data <- targets::tar_read(sorted_data)
table(data$family_diabetes)

# Excluding unknown family history
known_history <- data %>%
    filter(family_diabetes != "unknown")

create_formula <- function(xvars, covars) {
    outcome <- "Surv(survival_gbd, gbd == 1)"
    reformulate(c(xvars, covars), response = outcome)
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
        map(~ survival::coxph(.x, data = known_history, ties = "breslow")) |>
        map2(names(model2_formulas), ~ tidy(.x, exponentiate = TRUE, conf.int = TRUE) |>
                 mutate(across(where(is.numeric), ~ round(.x, 2))) |>
                 mutate(model = .y))

    return(model2_results)
}

known <- main_model2(known_history)
