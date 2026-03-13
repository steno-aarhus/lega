# Additional calculations

# Time between completed diet questionnaires ------------------------------

library(dplyr)
library(magrittr)
library(tidyverse)
data <- targets::tar_read(outcome_data)

data <- data %>%
    mutate(
        ques_comp_t0 = p105010_i0,
        ques_comp_t1 = p105010_i1,
        ques_comp_t2 = p105010_i2,
        ques_comp_t3 = p105010_i3,
        ques_comp_t4 = p105010_i4,
        # Removing specific time stamp
        ques_comp_t0 = substr(ques_comp_t0, 1, 10),
        ques_comp_t1 = substr(ques_comp_t1, 1, 10),
        ques_comp_t2 = substr(ques_comp_t2, 1, 10),
        ques_comp_t3 = substr(ques_comp_t3, 1, 10),
        ques_comp_t4 = substr(ques_comp_t4, 1, 10)
    )

data <- data %>% mutate(
    ques_comp_t1 = as.Date(ques_comp_t1),
    ques_comp_t2 = as.Date(ques_comp_t2),
    ques_comp_t3 = as.Date(ques_comp_t3),
    ques_comp_t4 = as.Date(ques_comp_t4)
)


data <- data %>%
    rowwise() %>%
    mutate(
        # diff_t0_t1 = as.numeric(difftime(ques_comp_t1, ques_comp_t0, units = "days")),
        diff_t1_t2 = as.numeric(difftime(ques_comp_t2, ques_comp_t1, units = "days")),
        diff_t2_t3 = as.numeric(difftime(ques_comp_t3, ques_comp_t2, units = "days")),
        diff_t3_t4 = as.numeric(difftime(ques_comp_t4, ques_comp_t3, units = "days"))
    ) %>%
    ungroup()

# Combine all differences into a single column, ignoring NAs
all_differences <- data %>%
    select(diff_t1_t2, diff_t2_t3, diff_t3_t4) %>%
    pivot_longer(cols = everything(), values_to = "interval") %>%
    filter(!is.na(interval)) %>%
    pull(interval)

# Calculate mean and standard deviation
mean_interval <- mean(all_differences)
sd_interval <- sd(all_differences)

# Print results
cat("Mean interval:", mean_interval, "days\n")
cat("SD of interval:", sd_interval, "days\n")





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
