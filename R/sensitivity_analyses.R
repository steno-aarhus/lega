#7. Sensitivity analyses

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


# Load data --------------------------------------------------------
targets::tar_make()
# Restart session
source(here::here("R/1_data_start.R"))


# Legumes including peas; substitution models ------------------------------
# Weekly substituting 80 g legumes including peas (NHS 1 portion beans = 80 g)
# https://www.nhs.uk/live-well/eat-well/5-a-day/5-a-day-what-counts/
# defining 80 g/week variable for each food
data <- data %>%
  mutate(legumepea80 = legume_pea_weekly/80,
         meats80 = meats_weekly/80,
         poultry80 = poultry_weekly/80,
         fish80 = fish_weekly/80)

## model 2 -----------------------------------------------------------------
# Alcohol as spline with 4 knots for adjustment
df <- 4
data <- data %>%
  mutate(alcohol_spline = predict(bs(alcohol_weekly, df = df, degree = 3, knots = NULL)))

# meats
meat_model2 <- coxph(Surv(survival_time, nafld == 1) ~
                       # removing meat
                       legumepea80 + poultry80 + fish80+
                       #other food components
                       cereal_refined_weekly + whole_grain_weekly + mixed_dish_weekly +
                       dairy_weekly + fats_weekly + fruit_weekly + nut_weekly +
                       veggie_pea_weekly + potato_weekly + egg_weekly + meat_sub_weekly +
                       non_alc_beverage_weekly + alc_beverage_weekly + snack_weekly +
                       sauce_weekly + weight_weekly + age_strata + region + sex +
                       alcohol_spline + ethnicity + deprivation_quint + education +
                       cohabitation + physical_activity + smoking + diabetes + cancer +
                       non_cancer_illness + family_illness + yearly_income + alcohol_spline,
                     data = data, ties='breslow')

# Extract HR and 95% CI for the first coefficient
coef_summary <- summary(meat_model2)$coefficients
row_name <- rownames(coef_summary)
HR <- exp(coef_summary[1, "coef"])
CI <- confint(meat_model2)[1, ]
CI <- exp(CI)
# Round to two decimals
HR <- round(HR, 2)
CI <- round(CI, 2)
# Convert to dataframe
meat_model2 <- data.frame(row_name = row_name, HR = HR, Lower_CI = CI[1], Upper_CI = CI[2])

# poultry
poultry_model2 <- coxph(Surv(survival_time, nafld == 1) ~
                          # removing meat
                          legumepea80 + meats80 + fish80+
                          #other food components
                          cereal_refined_weekly + whole_grain_weekly + mixed_dish_weekly +
                          dairy_weekly + fats_weekly + fruit_weekly + nut_weekly +
                          veggie_pea_weekly + potato_weekly + egg_weekly + meat_sub_weekly +
                          non_alc_beverage_weekly + alc_beverage_weekly + snack_weekly +
                          sauce_weekly + weight_weekly + age_strata + region + sex +
                          alcohol_spline + ethnicity + deprivation_quint + education +
                          cohabitation + physical_activity + smoking + diabetes + cancer +
                          non_cancer_illness + family_illness + yearly_income + alcohol_spline,
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
                       legumepea80 + meats80 + poultry80+
                       #other food components
                       cereal_refined_weekly + whole_grain_weekly + mixed_dish_weekly +
                       dairy_weekly + fats_weekly + fruit_weekly + nut_weekly +
                       veggie_pea_weekly + potato_weekly + egg_weekly + meat_sub_weekly +
                       non_alc_beverage_weekly + alc_beverage_weekly + snack_weekly +
                       sauce_weekly + weight_weekly + age_strata + region + sex +
                       alcohol_spline + ethnicity + deprivation_quint + education +
                       cohabitation + physical_activity + smoking + diabetes + cancer +
                       non_cancer_illness + family_illness + yearly_income + alcohol_spline,
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


### Extract results ---------------------------------------------------------
# combine first row from each model in df
first_row_df1 <- meat_model2[1, ]
first_row_df2 <- poultry_model2[1, ]
first_row_df3 <- fish_model2[1, ]
first_rows_combined <- rbind(first_row_df1, first_row_df2, first_row_df3)
rownames(first_rows_combined) <- c("meat_model2", "poultry_model2", "fish_model2")

#create html table
legume_pea_analysis <- first_rows_combined %>%
  kable("html") %>%
  kable_styling()
flextable::save_as_html(legume_pea_analysis, path = here("doc", "sensitivity_legume_pea.html"))



# Varying 24h recalls -----------------------------------------------------
data3 <- data %>%
  subset(p20077>=3)
data4 <- data %>%
  subset(p20077>=4)
data5 <- data %>%
  subset(p20077>=5)

## data3 = three 24h recalls -----------------------------------------------------------------
# Alcohol as spline with 4 knots for adjustment
df <- 4
data3 <- data3 %>%
  mutate(alcohol_spline = predict(bs(alcohol_weekly, df = df, degree = 3, knots = NULL)))

# meats
meat_data3 <- coxph(Surv(survival_time, nafld == 1) ~
                       # removing meat
                       legumes80 + poultry80 + fish80+
                       #other food components
                       cereal_refined_weekly + whole_grain_weekly + mixed_dish_weekly +
                       dairy_weekly + fats_weekly + fruit_weekly + nut_weekly +
                       veggie_weekly + potato_weekly + egg_weekly + meat_sub_weekly +
                       non_alc_beverage_weekly + alc_beverage_weekly + snack_weekly +
                       sauce_weekly + weight_weekly + age_strata + region + sex +
                       alcohol_spline + ethnicity + deprivation_quint + education +
                       cohabitation + physical_activity + smoking + diabetes + cancer +
                       non_cancer_illness + family_illness + yearly_income,
                     data = data3, ties='breslow')

# Extract HR and 95% CI for the first coefficient
coef_summary <- summary(meat_data3)$coefficients
row_name <- rownames(coef_summary)
HR <- exp(coef_summary[1, "coef"])
CI <- confint(meat_data3)[1, ]
CI <- exp(CI)
# Round to two decimals
HR <- round(HR, 2)
CI <- round(CI, 2)
# Convert to dataframe
meat_data3 <- data.frame(row_name = row_name, HR = HR, Lower_CI = CI[1], Upper_CI = CI[2])

# poultry
poultry_data3 <- coxph(Surv(survival_time, nafld == 1) ~
                          # removing meat
                          legumes80 + meats80 + fish80+
                          #other food components
                          cereal_refined_weekly + whole_grain_weekly + mixed_dish_weekly +
                          dairy_weekly + fats_weekly + fruit_weekly + nut_weekly +
                          veggie_weekly + potato_weekly + egg_weekly + meat_sub_weekly +
                          non_alc_beverage_weekly + alc_beverage_weekly + snack_weekly +
                          sauce_weekly + weight_weekly + age_strata + region + sex +
                          alcohol_spline + ethnicity + deprivation_quint + education +
                          cohabitation + physical_activity + smoking + diabetes + cancer +
                          non_cancer_illness + family_illness + yearly_income,
                        data = data3, ties='breslow')

# Extract HR and 95% CI for the first coefficient
coef_summary <- summary(poultry_data3)$coefficients
row_name <- rownames(coef_summary)
HR <- exp(coef_summary[1, "coef"])
CI <- confint(poultry_data3)[1, ]
CI <- exp(CI)
# Round to two decimals
HR <- round(HR, 2)
CI <- round(CI, 2)
# Convert to dataframe
poultry_data3 <- data.frame(row_name = row_name, HR = HR, Lower_CI = CI[1], Upper_CI = CI[2])


# fish
fish_data3 <- coxph(Surv(survival_time, nafld == 1) ~
                       # removing meat
                       legumes80 + meats80 + poultry80+
                       #other food components
                       cereal_refined_weekly + whole_grain_weekly + mixed_dish_weekly +
                       dairy_weekly + fats_weekly + fruit_weekly + nut_weekly +
                       veggie_weekly + potato_weekly + egg_weekly + meat_sub_weekly +
                       non_alc_beverage_weekly + alc_beverage_weekly + snack_weekly +
                       sauce_weekly + weight_weekly + age_strata + region + sex +
                       alcohol_spline + ethnicity + deprivation_quint + education +
                       cohabitation + physical_activity + smoking + diabetes + cancer +
                       non_cancer_illness + family_illness + yearly_income,
                     data = data3, ties='breslow')

# Extract HR and 95% CI for the first coefficient
coef_summary <- summary(fish_data3)$coefficients
row_name <- rownames(coef_summary)
HR <- exp(coef_summary[1, "coef"])
CI <- confint(fish_data3)[1, ]
CI <- exp(CI)
# Round to two decimals
HR <- round(HR, 2)
CI <- round(CI, 2)
# Convert to dataframe
fish_data3 <- data.frame(row_name = row_name, HR = HR, Lower_CI = CI[1], Upper_CI = CI[2])


## data4 = four 24h recalls -----------------------------------------------------------------
# Alcohol as spline with 4 knots for adjustment
df <- 4
data4 <- data4 %>%
  mutate(alcohol_spline = predict(bs(alcohol_weekly, df = df, degree = 3, knots = NULL)))

# meats
meat_data4 <- coxph(Surv(survival_time, nafld == 1) ~
                      # removing meat
                      legumes80 + poultry80 + fish80+
                      #other food components
                      cereal_refined_weekly + whole_grain_weekly + mixed_dish_weekly +
                      dairy_weekly + fats_weekly + fruit_weekly + nut_weekly +
                      veggie_weekly + potato_weekly + egg_weekly + meat_sub_weekly +
                      non_alc_beverage_weekly + alc_beverage_weekly + snack_weekly +
                      sauce_weekly + weight_weekly + age_strata + region + sex +
                      alcohol_spline + ethnicity + deprivation_quint + education +
                      cohabitation + physical_activity + smoking + diabetes + cancer +
                      non_cancer_illness + family_illness + yearly_income,
                    data = data4, ties='breslow')

# Extract HR and 95% CI for the first coefficient
coef_summary <- summary(meat_data4)$coefficients
row_name <- rownames(coef_summary)
HR <- exp(coef_summary[1, "coef"])
CI <- confint(meat_data4)[1, ]
CI <- exp(CI)
# Round to two decimals
HR <- round(HR, 2)
CI <- round(CI, 2)
# Convert to dataframe
meat_data4 <- data.frame(row_name = row_name, HR = HR, Lower_CI = CI[1], Upper_CI = CI[2])

# poultry
poultry_data4 <- coxph(Surv(survival_time, nafld == 1) ~
                         # removing meat
                         legumes80 + meats80 + fish80+
                         #other food components
                         cereal_refined_weekly + whole_grain_weekly + mixed_dish_weekly +
                         dairy_weekly + fats_weekly + fruit_weekly + nut_weekly +
                         veggie_weekly + potato_weekly + egg_weekly + meat_sub_weekly +
                         non_alc_beverage_weekly + alc_beverage_weekly + snack_weekly +
                         sauce_weekly + weight_weekly + age_strata + region + sex +
                         alcohol_spline + ethnicity + deprivation_quint + education +
                         cohabitation + physical_activity + smoking + diabetes + cancer +
                         non_cancer_illness + family_illness + yearly_income,
                       data = data4, ties='breslow')

# Extract HR and 95% CI for the first coefficient
coef_summary <- summary(poultry_data4)$coefficients
row_name <- rownames(coef_summary)
HR <- exp(coef_summary[1, "coef"])
CI <- confint(poultry_data4)[1, ]
CI <- exp(CI)
# Round to two decimals
HR <- round(HR, 2)
CI <- round(CI, 2)
# Convert to dataframe
poultry_data4 <- data.frame(row_name = row_name, HR = HR, Lower_CI = CI[1], Upper_CI = CI[2])


# fish
fish_data4 <- coxph(Surv(survival_time, nafld == 1) ~
                      # removing meat
                      legumes80 + meats80 + poultry80+
                      #other food components
                      cereal_refined_weekly + whole_grain_weekly + mixed_dish_weekly +
                      dairy_weekly + fats_weekly + fruit_weekly + nut_weekly +
                      veggie_weekly + potato_weekly + egg_weekly + meat_sub_weekly +
                      non_alc_beverage_weekly + alc_beverage_weekly + snack_weekly +
                      sauce_weekly + weight_weekly + age_strata + region + sex +
                      alcohol_spline + ethnicity + deprivation_quint + education +
                      cohabitation + physical_activity + smoking + diabetes + cancer +
                      non_cancer_illness + family_illness + yearly_income,
                    data = data4, ties='breslow')

# Extract HR and 95% CI for the first coefficient
coef_summary <- summary(fish_data4)$coefficients
row_name <- rownames(coef_summary)
HR <- exp(coef_summary[1, "coef"])
CI <- confint(fish_data4)[1, ]
CI <- exp(CI)
# Round to two decimals
HR <- round(HR, 2)
CI <- round(CI, 2)
# Convert to dataframe
fish_data4 <- data.frame(row_name = row_name, HR = HR, Lower_CI = CI[1], Upper_CI = CI[2])

## data5 = five 24h recalls -----------------------------------------------------------------
# Alcohol as spline with 4 knots for adjustment
df <- 4
data5 <- data5 %>%
  mutate(alcohol_spline = predict(bs(alcohol_weekly, df = df, degree = 3, knots = NULL)))

# meats
meat_data5 <- coxph(Surv(survival_time, nafld == 1) ~
                      # removing meat
                      legumes80 + poultry80 + fish80+
                      #other food components
                      cereal_refined_weekly + whole_grain_weekly + mixed_dish_weekly +
                      dairy_weekly + fats_weekly + fruit_weekly + nut_weekly +
                      veggie_weekly + potato_weekly + egg_weekly + meat_sub_weekly +
                      non_alc_beverage_weekly + alc_beverage_weekly + snack_weekly +
                      sauce_weekly + weight_weekly + age_strata + region + sex +
                      alcohol_spline + ethnicity + deprivation_quint + education +
                      cohabitation + physical_activity + smoking + diabetes + cancer +
                      non_cancer_illness + family_illness + yearly_income,
                    data = data5, ties='breslow')

# Extract HR and 95% CI for the first coefficient
coef_summary <- summary(meat_data5)$coefficients
row_name <- rownames(coef_summary)
HR <- exp(coef_summary[1, "coef"])
CI <- confint(meat_data5)[1, ]
CI <- exp(CI)
# Round to two decimals
HR <- round(HR, 2)
CI <- round(CI, 2)
# Convert to dataframe
meat_data5 <- data.frame(row_name = row_name, HR = HR, Lower_CI = CI[1], Upper_CI = CI[2])

# poultry
poultry_data5 <- coxph(Surv(survival_time, nafld == 1) ~
                         # removing meat
                         legumes80 + meats80 + fish80+
                         #other food components
                         cereal_refined_weekly + whole_grain_weekly + mixed_dish_weekly +
                         dairy_weekly + fats_weekly + fruit_weekly + nut_weekly +
                         veggie_weekly + potato_weekly + egg_weekly + meat_sub_weekly +
                         non_alc_beverage_weekly + alc_beverage_weekly + snack_weekly +
                         sauce_weekly + weight_weekly + age_strata + region + sex +
                         alcohol_spline + ethnicity + deprivation_quint + education +
                         cohabitation + physical_activity + smoking + diabetes + cancer +
                         non_cancer_illness + family_illness + yearly_income,
                       data = data5, ties='breslow')

# Extract HR and 95% CI for the first coefficient
coef_summary <- summary(poultry_data5)$coefficients
row_name <- rownames(coef_summary)
HR <- exp(coef_summary[1, "coef"])
CI <- confint(poultry_data5)[1, ]
CI <- exp(CI)
# Round to two decimals
HR <- round(HR, 2)
CI <- round(CI, 2)
# Convert to dataframe
poultry_data5 <- data.frame(row_name = row_name, HR = HR, Lower_CI = CI[1], Upper_CI = CI[2])


# fish
fish_data5 <- coxph(Surv(survival_time, nafld == 1) ~
                      # removing meat
                      legumes80 + meats80 + poultry80+
                      #other food components
                      cereal_refined_weekly + whole_grain_weekly + mixed_dish_weekly +
                      dairy_weekly + fats_weekly + fruit_weekly + nut_weekly +
                      veggie_weekly + potato_weekly + egg_weekly + meat_sub_weekly +
                      non_alc_beverage_weekly + alc_beverage_weekly + snack_weekly +
                      sauce_weekly + weight_weekly + age_strata + region + sex +
                      alcohol_spline + ethnicity + deprivation_quint + education +
                      cohabitation + physical_activity + smoking + diabetes + cancer +
                      non_cancer_illness + family_illness + yearly_income,
                    data = data5, ties='breslow')

# Extract HR and 95% CI for the first coefficient
coef_summary <- summary(fish_data5)$coefficients
row_name <- rownames(coef_summary)
HR <- exp(coef_summary[1, "coef"])
CI <- confint(fish_data5)[1, ]
CI <- exp(CI)
# Round to two decimals
HR <- round(HR, 2)
CI <- round(CI, 2)
# Convert to dataframe
fish_data5 <- data.frame(row_name = row_name, HR = HR, Lower_CI = CI[1], Upper_CI = CI[2])


### Extract results ---------------------------------------------------------
# To combine different 24h recalls with main analysis results,
# run model 2 in "6_main_analysis.R" first
first_row_df1 <- meat_model2[1, ]
first_row_df2 <- meat_data3[1, ]
first_row_df3 <- meat_data4[1, ]
first_row_df4 <- meat_data5[1, ]
first_row_df5 <- poultry_model2[1, ]
first_row_df6 <- poultry_data3[1, ]
first_row_df7 <- poultry_data4[1, ]
first_row_df8 <- poultry_data5[1, ]
first_row_df9 <- fish_model2[1, ]
first_row_df10 <- fish_data3[1, ]
first_row_df11 <- fish_data4[1, ]
first_row_df12 <- fish_data5[1, ]
first_rows_combined <- rbind(first_row_df1, first_row_df2, first_row_df3, first_row_df4,
                             first_row_df5, first_row_df6, first_row_df7, first_row_df8,
                             first_row_df9, first_row_df10, first_row_df11, first_row_df12)
rownames(first_rows_combined) <- c("meat_data2", "meat_data3", "meat_data4", "meat_data5",
                                   "poultry_data2", "poultry_data3", "poultry_data4", "poultry_data5",
                                   "fish_data2", "fish_data3", "fish_data4", "fish_data5")
#create html table
diff_recalls <- first_rows_combined %>%
  kable("html") %>%
  kable_styling()
flextable::save_as_html(diff_recalls, path = here("doc", "sensitivity_diff_recalls.html"))



# Removing high ALT and AST from analysis-----------------------------------------------
# removing high ALT and AST
data <- data %>% mutate(
  alt_level = case_when(
    alt <= 45 & sex == "Female" | alt <= 70 & sex == "Male" ~ 0,
    alt > 45 & sex == "Female" | alt > 70 & sex == "Male" ~ 1
  ),
  ast_level = case_when(
    ast <= 35 & sex == "Female" | ast <= 45 & sex == "Male" ~ 0,
    ast > 35 & sex == "Female" | ast > 45 & sex == "Male" ~ 1
  )
)

# Subsetting data
normal_liver <- data %>%
  subset(alt_level !=1 & ast_level != 1)


## Running main analysis on subsample --------------------------------------
# Weekly substituting 80 g legumes (NHS 1 portion beans = 80 g) https://www.nhs.uk/live-well/eat-well/5-a-day/5-a-day-what-counts/
# defining 80 g/week variable for each food
normal_liver <- normal_liver %>%
  mutate(legumes80 = legume_weekly/80,
         meats80 = meats_weekly/80,
         poultry80 = poultry_weekly/80,
         fish80 = fish_weekly/80)

df <- 4
normal_liver <- normal_liver %>%
  mutate(alcohol_spline = predict(bs(alcohol_weekly, df = df, degree = 3, knots = NULL)))

# meats
meat_model2 <- coxph(Surv(survival_time, nafld == 1) ~
                       # removing meat
                       legumes80 + poultry80 + fish80+
                       #other food components
                       cereal_refined_weekly + whole_grain_weekly + mixed_dish_weekly +
                       dairy_weekly + fats_weekly + fruit_weekly + nut_weekly +
                       veggie_weekly + potato_weekly + egg_weekly + meat_sub_weekly +
                       non_alc_beverage_weekly + alc_beverage_weekly + snack_weekly +
                       sauce_weekly + weight_weekly + age_strata + region + sex +
                       alcohol_spline + ethnicity + deprivation_quint + education +
                       cohabitation + physical_activity + smoking + diabetes + cancer +
                       non_cancer_illness + family_illness + yearly_income,
                     data = normal_liver, ties='breslow')

# Extract HR and 95% CI for the first coefficient
coef_summary <- summary(meat_model2)$coefficients
row_name <- rownames(coef_summary)
HR <- exp(coef_summary[1, "coef"])
CI <- confint(meat_model2)[1, ]
CI <- exp(CI)
# Round to two decimals
HR <- round(HR, 2)
CI <- round(CI, 2)
# Convert to dataframe
meat_model2 <- data.frame(row_name = row_name, HR = HR, Lower_CI = CI[1], Upper_CI = CI[2])

# poultry
poultry_model2 <- coxph(Surv(survival_time, nafld == 1) ~
                          # removing meat
                          legumes80 + meats80 + fish80+
                          #other food components
                          cereal_refined_weekly + whole_grain_weekly + mixed_dish_weekly +
                          dairy_weekly + fats_weekly + fruit_weekly + nut_weekly +
                          veggie_weekly + potato_weekly + egg_weekly + meat_sub_weekly +
                          non_alc_beverage_weekly + alc_beverage_weekly + snack_weekly +
                          sauce_weekly + weight_weekly + age_strata + region + sex +
                          alcohol_spline + ethnicity + deprivation_quint + education +
                          cohabitation + physical_activity + smoking + diabetes + cancer +
                          non_cancer_illness + family_illness + yearly_income,
                        data = normal_liver, ties='breslow')

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
                       veggie_weekly + potato_weekly + egg_weekly + meat_sub_weekly +
                       non_alc_beverage_weekly + alc_beverage_weekly + snack_weekly +
                       sauce_weekly + weight_weekly + age_strata + region + sex +
                       alcohol_spline + ethnicity + deprivation_quint + education +
                       cohabitation + physical_activity + smoking + diabetes + cancer +
                       non_cancer_illness + family_illness + yearly_income,
                     data = normal_liver, ties='breslow')

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

### Extract results ---------------------------------------------------------
# combine first row from each model in df
first_row_df1 <- meat_model2[1, ]
first_row_df2 <- poultry_model2[1, ]
first_row_df3 <- fish_model2[1, ]
first_rows_combined <- rbind(first_row_df1, first_row_df2, first_row_df3)
rownames(first_rows_combined) <- c("meat_model2", "poultry_model2", "fish_model2")

#create html table
normal_liver <- first_rows_combined %>%
  kable("html") %>%
  kable_styling()
flextable::save_as_html(normal_liver, path = here("doc", "sensitivity_normal_liver.html"))



# Remove high alcohol intake --------------------------------------------------
percentile_90 <- quantile(data$alcohol_weekly, probs = 0.90, na.rm = TRUE)
lower_alc <- data %>%
  subset(alcohol_weekly < percentile_90)

## main analysis model 2 on subset -----------------------------------------------------------------
lower_alc <- lower_alc %>%
  mutate(alcohol_spline = predict(bs(alcohol_weekly, df = 4, degree = 3, knots = NULL)))

# meats
meat_model2 <- coxph(Surv(survival_time, nafld == 1) ~
                       # removing meat
                       legumes80 + poultry80 + fish80+
                       #other food components
                       cereal_refined_weekly + whole_grain_weekly + mixed_dish_weekly +
                       dairy_weekly + fats_weekly + fruit_weekly + nut_weekly +
                       veggie_weekly + potato_weekly + egg_weekly + meat_sub_weekly +
                       non_alc_beverage_weekly + alc_beverage_weekly + snack_weekly +
                       sauce_weekly + weight_weekly + age_strata + region + sex +
                       alcohol_spline + ethnicity + deprivation_quint + education +
                       cohabitation + physical_activity + smoking + diabetes + cancer +
                       non_cancer_illness + family_illness + yearly_income,
                     data = lower_alc, ties='breslow')

# Extract HR and 95% CI for the first coefficient
coef_summary <- summary(meat_model2)$coefficients
row_name <- rownames(coef_summary)
HR <- exp(coef_summary[1, "coef"])
CI <- confint(meat_model2)[1, ]
CI <- exp(CI)
# Round to two decimals
HR <- round(HR, 2)
CI <- round(CI, 2)
# Convert to dataframe
meat_model2 <- data.frame(row_name = row_name, HR = HR, Lower_CI = CI[1], Upper_CI = CI[2])

# poultry
poultry_model2 <- coxph(Surv(survival_time, nafld == 1) ~
                          # removing meat
                          legumes80 + meats80 + fish80+
                          #other food components
                          cereal_refined_weekly + whole_grain_weekly + mixed_dish_weekly +
                          dairy_weekly + fats_weekly + fruit_weekly + nut_weekly +
                          veggie_weekly + potato_weekly + egg_weekly + meat_sub_weekly +
                          non_alc_beverage_weekly + alc_beverage_weekly + snack_weekly +
                          sauce_weekly + weight_weekly + age_strata + region + sex +
                          alcohol_spline + ethnicity + deprivation_quint + education +
                          cohabitation + physical_activity + smoking + diabetes + cancer +
                          non_cancer_illness + family_illness + yearly_income,
                        data = lower_alc, ties='breslow')

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
                       veggie_weekly + potato_weekly + egg_weekly + meat_sub_weekly +
                       non_alc_beverage_weekly + alc_beverage_weekly + snack_weekly +
                       sauce_weekly + weight_weekly + age_strata + region + sex +
                       alcohol_spline + ethnicity + deprivation_quint + education +
                       cohabitation + physical_activity + smoking + diabetes + cancer +
                       non_cancer_illness + family_illness + yearly_income,
                     data = lower_alc, ties='breslow')

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

### Extract results ---------------------------------------------------------
first_row_df1 <- meat_model2[1, ]
first_row_df2 <- poultry_model2[1, ]
first_row_df3 <- fish_model2[1, ]
first_rows_combined <- rbind(first_row_df1, first_row_df2, first_row_df3)
rownames(first_rows_combined) <- c("meat_model2", "poultry_model2", "fish_model2")

#create html table
lower_alc <- first_rows_combined %>%
  kable("html") %>%
  kable_styling()
flextable::save_as_html(lower_alc, path = here("doc", "sensitivity_lower_alc.html"))

