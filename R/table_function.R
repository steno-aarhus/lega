# table for results
library(survival)
library(broom)
# analyses first:
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
                 "smoking", "estrogen_treatment", "pregnancies", "yearly_income",
                 "related_conditions", "family_diabetes",
                 "strata(region, age_strata, sex)")

    model2_formulas <- list(
        meat_model2 = create_formula(c("legumes80", "poultry80", "fish80"), covars2),
        poultry_model2 = create_formula(c("legumes80", "meats80", "fish80"), covars2),
        fish_model2 = create_formula(c("legumes80", "meats80", "poultry80"), covars2)
    )

    model2_results <- model2_formulas |>
        map(~ coxph(.x, data = data, ties = "breslow")) |>
        map2(names(model2_formulas), ~ tidy(.x, exponentiate = TRUE, conf.int = TRUE) |>
                 mutate(across(where(is.numeric), ~ round(.x, 2))) |>
                 mutate(model = .y))

    return(model2_results)
}



table_df <- data.frame(
      model = c("Model 1", "Model 2"),
      meat = c((glue::glue("{model1_results$meat_model1[1,2]} ({model1_results$meat_model1[1,6]}; {model1_results$meat_model1[1,7]})")),
                glue::glue("{model2_results$meat_model2[1,2]} ({model2_results$meat_model2[1,6]}; {model2_results$meat_model2[1,7]})")))

library(gt)
table_df %>% gt() %>%
    tab_caption("Consuming 80 g/week of legumes instead of 80 g/week of") %>%
    gtsave("main_table.html")

flextable::save_as_html(table1, path = here("doc", "table1.html"))
# From Luke:
# Instead of working with three (or more) dataframes, combine them together into
# one dataframe via bind_rows() or similar and then use filter() to take out what
# you want.

#
# table_df <- data.frame(
#   model = c("Model 1", "Model 2", "Model 3"),
#   meat = c(data$meat1),
#   poultry = c(meat1, meat2, meat3),
#   fish = c(meat1, meat2, meat3)
# )
# knitr::kable(table_df, col.names = c("Statistical model", "Red and processed meat"), row.names = TRUE)
