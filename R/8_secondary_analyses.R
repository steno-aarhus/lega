#8. Secondary analyses

#Load packages
library(tidyverse)
library(Hmisc)
library(survival)
library(gtsummary)
library(ggsurvfit)
library(ggplot2)
library(dplyr)
library(tidyr)
library(here)
library(broom)
library(eventglm)


# Non specific substitutions ----------------------------------------------
# Leaving one portion of legumes (80g) out weekly
data <- data %>%
  mutate(legumes80 = legumes_weekly/80)


## model 2 -----------------------------------------------------------------
#alcohol spline
nonspecific_model2 <- coxph(Surv(survival_gbd, gbd == 1) ~ legumes80 +
                              weight_weekly + age + region + sex +
                              alcohol_weekly + ethnicity + deprivation + education +
                              cohabitation + physical_activity + smoking +
                              related_disease + disease_family + yearly_income,
                            data = data, ties='breslow')

nonspecific_model2 <- tidy(nonspecific_model2, exponentiate = TRUE, conf.int = TRUE)



# Total legume  -----------------------------------------------------------

# consuming 80 g legumes more weekly independent of other foods consumed
# (non-substitution effect)
