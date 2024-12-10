# Additional calculations

# Time between completed diet questionnaires ------------------------------

library(dplyr)
library(magrittr)
library(tidyverse)
data <- targets::tar_read(eligible_participants)

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




# Printing knots of alc_spline --------------------------------------------
library(dplyr)
library(magrittr)
library(tidyverse)

data <- targets::tar_read(id_data)

data <- data %>% mutate(
    alcohol_intake = rowSums(pick(matches("p26030")), na.rm = TRUE),
    alcohol_daily = alcohol_intake / p20077,
    alcohol_weekly = alcohol_daily * 7,
    alc_spline = splines::bs(alcohol_weekly, knots = 4, degree = 3)
)

# Calculate internal knots based on quantiles
nknots <- 4  # Number of knots (adjust as needed)
internal_knots <- quantile(data$alcohol_weekly, probs = seq(0.25, 0.75, length.out = nknots), na.rm = TRUE)

# Generate the spline basis using specified knots
spline_basis <- splines::bs(data$alcohol_weekly, knots = internal_knots, degree = 3)

# Extract and print the knot positions
knots <- attr(spline_basis, "knots")

cat("knots are placed at:", knots, "\n")



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
legume <- coxph(Surv(survival_gbd, event = gbd) ~ legume_weekly + alc_spline + ethnicity
                + deprivation + education + cohabitation + physical_activity + smoking
                + related_disease + disease_family + yearly_income + strata(region, age_strata, sex),
                data = data,
                ties = 'breslow')

legume_splines <- coxph(Surv(survival_gbd, event = gbd) ~ legume_spline + alc_spline + ethnicity
                        + deprivation + education + cohabitation + physical_activity + smoking
                        + related_disease + disease_family + yearly_income + strata(region, age_strata, sex),
                        data = data,
                        ties = 'breslow')

# Perform a likelihood ratio test
lrt_result <- anova(legume, legume_splines, test = "LRT") %>% print() # p = 0.9728

## meat -----------------------------------------------------------------
meat <- coxph(Surv(survival_gbd, event = gbd) ~ meats_weekly + alc_spline + ethnicity
              + deprivation + education + cohabitation + physical_activity + smoking
              + related_disease + disease_family + yearly_income + strata(region, age_strata, sex),
              data = data,
              ties = 'breslow')

meat_splines <- coxph(Surv(survival_gbd, event = gbd) ~ meat_spline + alc_spline + ethnicity
                      + deprivation + education + cohabitation + physical_activity + smoking
                      + related_disease + disease_family + yearly_income + strata(region, age_strata, sex),
                      data = data,
                      ties = 'breslow')

# Perform a likelihood ratio test
lrt_result <- anova(meat, meat_splines, test = "LRT") %>% print() # p = 0.6576

## poultry -----------------------------------------------------------------
poultry <- coxph(Surv(survival_gbd, event = gbd) ~ poultry_weekly + alc_spline + ethnicity
                 + deprivation + education + cohabitation + physical_activity + smoking
                 + related_disease + disease_family + yearly_income + strata(region, age_strata, sex),
                 data = data,
                 ties = 'breslow')

poultry_splines <- coxph(Surv(survival_gbd, event = gbd) ~ poultry_spline + alc_spline + ethnicity
                         + deprivation + education + cohabitation + physical_activity + smoking
                         + related_disease + disease_family + yearly_income + strata(region, age_strata, sex),
                         data = data,
                         ties = 'breslow')

# Perform a likelihood ratio test
lrt_result <- anova(poultry, poultry_splines, test = "LRT") %>% print() # p = 0.9392

## fish -----------------------------------------------------------------
fish <- coxph(Surv(survival_gbd, event = gbd) ~ fish_weekly + alc_spline + ethnicity
              + deprivation + education + cohabitation + physical_activity + smoking
              + related_disease + disease_family + yearly_income + strata(region, age_strata, sex),
              data = data,
              ties = 'breslow')

fish_splines <- coxph(Surv(survival_gbd, event = gbd) ~ fish_spline + alc_spline + ethnicity
                      + deprivation + education + cohabitation + physical_activity + smoking
                      + related_disease + disease_family + yearly_income + strata(region, age_strata, sex),
                      data = data,
                      ties = 'breslow')

# Perform a likelihood ratio test
lrt_result <- anova(fish, fish_splines, test = "LRT") %>% print() # p = 0.5011
