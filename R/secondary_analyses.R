#8. Secondary analyses

#Load packages
install.packages("Hmisc")
install.packages("survival")
install.packages("gtsummary")
install.packages("ggsurvfit")
install.packages("kableExtra")

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

# Pseudo observational method ---------------------------------------------
# using survival model from main analysis
# defining 80 g/week variable for each food
data <- data %>%
  mutate(legumes80 = legume_weekly/80,
         meats80 = meats_weekly/80,
         poultry80 = poultry_weekly/80,
         fish80 = fish_weekly/80)

data <- data %>%
  mutate(alcohol_spline = predict(bs(alcohol_weekly, df = 4, degree = 3, knots = NULL)))

# meats
meat_pseudo <- coxph(Surv(survival_time, nafld == 1) ~
                       # removing meat
                       legumes80 + poultry80 + fish80+
                         #other food components
                         cereal_refined_weekly + whole_grain_weekly + mixed_dish_weekly +
                         dairy_weekly + fats_weekly + fruit_weekly + nut_weekly +
                         veggie_weekly + potato_weekly + egg_weekly +
                         non_alc_beverage_weekly + alc_beverage_weekly + snack_weekly +
                       sauce_weekly + weight_weekly + age_strata + region + sex +
                       alcohol_spline + ethnicity + deprivation_quint + education +
                       cohabitation + physical_activity + smoking + diabetes + cancer +
                       non_cancer_illness + family_illness + yearly_income,
                     data = data, ties='breslow')

# Extract HR and 95% CI for the first coefficient
coef_summary <- summary(meat_pseudo)$coefficients
row_name <- rownames(coef_summary)
HR <- exp(coef_summary[1, "coef"])
CI <- confint(meat_pseudo)[1, ]
CI <- exp(CI)
# Round to two decimals
HR <- round(HR, 2)
CI <- round(CI, 2)
# Convert to dataframe
meat_pseudo <- data.frame(row_name = row_name, HR = HR, Lower_CI = CI[1], Upper_CI = CI[2])

# Calculate predicted survival probabilities
predicted_survival <- survfit(meat_pseudo)

# Create time points for pseudo-observations
time_points <- seq(0, max(predicted_survival$survival_time), by = 1)
# time_points <- seq(0, 1000000, by = 1)

# Initialize a matrix to store pseudo-observations
pseudo_observations <- matrix(0, nrow = length(time_points), ncol = nrow(predicted_survival$surv))

# For each time point, create pseudo-observations based on predicted survival probabilities
for (i in 1:length(time_points)) {
  pseudo_observations[i, ] <- as.numeric(predicted_survival$surv >= time_points[i])
}

# Calculate Kaplan-Meier estimator using pseudo-observations
pseudo_km <- survfit(Surv(time_points, colSums(pseudo_observations)) ~ 1)

# Plot the estimated survival function
plot(pseudo_km, xlab = "Time", ylab = "Survival Probability", main = "Estimated Survival Function using Pseudo-Observations")


# poultry
poultry_model2 <- coxph(Surv(survival_time, nafld == 1) ~
                          # removing meat
                          legumes80 + meats80 + fish80+
                         #other food components
                         cereal_refined_weekly + whole_grain_weekly + mixed_dish_weekly +
                         dairy_weekly + fats_weekly + fruit_weekly + nut_weekly +
                         veggie_weekly + potato_weekly + egg_weekly +
                         non_alc_beverage_weekly + alc_beverage_weekly + snack_weekly +
                          sauce_weekly + weight_weekly + age_strata + region + sex +
                          alcohol_spline + ethnicity + deprivation_quint + education +
                          cohabitation + physical_activity + smoking + diabetes + cancer +
                          non_cancer_illness + family_illness + yearly_income,
                        data = data, ties='breslow')

# Extract HR and 95% CI for the first coefficient
coef_summary <- summary(poultry_model2)$coefficients
row_name <- rownames(coef_summary)
HR <- exp(coef_summary[1, "coef"])
CI <- confint(poultry_model2)[1, ]
CI <- exp(CI)
# Round to two decimals
HR <- round(HR, 2)
CI <- round(CI, 2)
# Convert to dataframe
poultry_model2 <- data.frame(row_name = row_name, HR = HR, Lower_CI = CI[1], Upper_CI = CI[2])


# fish
fish_model2 <- coxph(Surv(survival_time, nafld == 1) ~
                       # removing meat
                       legumes80 + meats80 + poultry80+
                         #other food components
                         cereal_refined_weekly + whole_grain_weekly + mixed_dish_weekly +
                         dairy_weekly + fats_weekly + fruit_weekly + nut_weekly +
                         veggie_weekly + potato_weekly + egg_weekly +
                         non_alc_beverage_weekly + alc_beverage_weekly + snack_weekly +
                       sauce_weekly + weight_weekly + age_strata + region + sex +
                       alcohol_spline + ethnicity + deprivation_quint + education +
                       cohabitation + physical_activity + smoking + diabetes + cancer +
                       non_cancer_illness + family_illness + yearly_income,
                     data = data, ties='breslow')

# Extract HR and 95% CI for the first coefficient
coef_summary <- summary(fish_model2)$coefficients
row_name <- rownames(coef_summary)
HR <- exp(coef_summary[1, "coef"])
CI <- confint(fish_model2)[1, ]
CI <- exp(CI)
# Round to two decimals
HR <- round(HR, 2)
CI <- round(CI, 2)
# Convert to dataframe
fish_model2 <- data.frame(row_name = row_name, HR = HR, Lower_CI = CI[1], Upper_CI = CI[2])







# Non specific substitutions ----------------------------------------------
# Leaving one portion of legumes (80g) out weekly
data <- data %>%
  mutate(legumes80 = legume_weekly/80)


## model 2 -----------------------------------------------------------------
#alcohol spline
df <- 4
data <- data %>%
  mutate(alcohol_spline = predict(bs(alcohol_weekly, df = df, degree = 3, knots = NULL)))

nonspecific_model2 <- coxph(Surv(survival_time, nafld == 1) ~ legumes80 +
                              weight_weekly + age_strata + region + sex +
                              alcohol_spline + ethnicity + deprivation_quint + education +
                              cohabitation + physical_activity + smoking + diabetes + cancer +
                              non_cancer_illness + family_illness + yearly_income,
                            data = data, ties='breslow')

# Extract HR and 95% CI for the first coefficient
coef_summary <- summary(nonspecific_model2)$coefficients
row_name <- rownames(coef_summary)
HR <- exp(coef_summary[1, "coef"])
CI <- confint(nonspecific_model2)[1, ]
CI <- exp(CI)
# Round to two decimals
HR <- round(HR, 2)
CI <- round(CI, 2)
# Convert to dataframe
nonspecific_model2 <- data.frame(row_name = row_name, HR = HR, Lower_CI = CI[1], Upper_CI = CI[2])


### Extract results ---------------------------------------------------------
#create html table
nonspecific_model2 <- nonspecific_model2 %>%
  kable("html") %>%
  kable_styling()
flextable::save_as_html(nonspecific_model2, path = here("doc", "secondary_nonspecific_model2.html"))


