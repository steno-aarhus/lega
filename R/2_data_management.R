# Data management
library(dplyr)
library(magrittr)
library(tidyr)
library(stringr)

# Remove ineligible number of recalls ------------
data <- data %>%
  subset(p20077>=2)
data <- data %>%
  mutate(p20077 = as.numeric(p20077))

# Add ID ------------------------------------------------------------------
data <- data %>%
  mutate(id = 1:n(), .before = everything())

# Remove variables and columns --------------------------------------------
# Delete follow-up instances for confounder variables
variables_to_edit <- c("p738", "p2443", "p2453", "p3456", "p6150",
                       "p20002","p20107", "p20110", "p20111", "p20161", "p20162",
                       "p21000", "p22040", "p22506", "p23104", "p2443",
                       "p2453", "p6141", "p709")
data <- data %>%
  select(-matches(paste0(variables_to_edit, "_i[1-4]")))


# Recoding covariables (not foods) ------------------------------------------------------

data <- data %>% mutate(
  sex = p31,
  sex = as.factor(sex),
  age = p21022,
  age = as.numeric(age),
  age_strata = case_when(
    age < 45 ~ 0,
    age >= 45 & age <= 49 ~ 1,
    age >= 50 & age <= 54 ~ 2,
    age >= 55 & age <= 59 ~ 3,
    age >= 60 & age <= 64 ~ 4,
    age >= 65 ~ 5
  ),
  age_strata = as.factor(age_strata),
  ethnicity = case_when(
    p21000_i0 == "White" | p21000_i0 == "British" | p21000_i0 == "Irish" | p21000_i0 == "Any other white background" ~ "white",
    p21000_i0 == "Chinese" | p21000_i0 == "Asian or Asian British" | p21000_i0 =="Indian" | p21000_i0 == "Pakistani" | p21000_i0 == "Bangladeshi" | p21000_i0 == "Any other Asian background" ~ "asian",
    p21000_i0 == "Black or Black British" | p21000_i0 == "Caribbean" | p21000_i0 == "African" | p21000_i0 == "Any other Black background" ~ "black",
    p21000_i0 == "Mixed" | p21000_i0 == "White and Black Caribbean" |p21000_i0 == "White and Black African" | p21000_i0 == "White and Asian" | p21000_i0 == "Any other mixed background" |
      p21000_i0 == "Other ethnic group" | p21000_i0 == "Do not know" | p21000_i0 == "Prefer not to answer" | str_detect(p21000_i0, "NA") ~ "mixed or other"
  ),
  deprivation = p22189,
  deprivation_quint = ntile(deprivation, 5),
  deprivation_quint = as.factor(deprivation_quint),
  yearly_income = case_when(
    str_detect(p738_i0, "18,000 to") ~ "18,000-30,999",
    str_detect(p738_i0, "31,000") ~ "31,000-51,999",
    str_detect(p738_i0, "52,000") ~ "52,000-100,000",
    str_detect(p738_i0, "know") ~ "don't know",
    str_detect(p738_i0, "Greater") ~ ">100,000",
    str_detect(p738_i0, "Less") ~ "<18,000",
    str_detect(p738_i0, "answer") ~ "no answer",
    TRUE ~ "no answer"
  ),
  yearly_income = as.factor(yearly_income),
  education_short = as.character(str_sub(p6138_i0, start = 1, end = 28)),
  education = case_when(
    education_short == "College or University degree" ~ "High",
    education_short == "A levels/AS levels or equiva" ~ "Intermediate",
    education_short == "O levels/GCSEs or equivalent" ~ "Intermediate",
    education_short == "CSEs or equivalent|NVQ or HN" ~ "Low",
    education_short == "CSEs or equivalent|Other pro" ~ "Low",
    education_short == "CSEs or equivalent" ~ "Low",
    education_short == "NVQ or HND or HNC or equival" ~ "Low",
    education_short == "Other professional qualifica" ~ "Low",
    education_short == "None of the above" ~ "Low"
  ),
  education = as.factor(education),
  physical_activity = case_when(
    p22040_i0 >0 & p22040_i0 <=918 ~ "low",
    p22040_i0 >918 & p22040_i0 <=3706 ~ "moderate",
    p22040_i0 >3706 ~ "high",
    TRUE ~ "unknown"
  ),
  bmi = p23104_i0,
  bmi = as.numeric(bmi),
  bmi30 = ifelse(p23104_i0 >= 30, 1, 0),
  bmi30 = as.numeric(bmi30)
)

data <- data %>% mutate(
  p6150_i0 = ifelse(is.na(p6150_i0), "None", p6150_i0),
  p6150_i0 = as.character(p6150_i0),
  p20002_i0 = ifelse(is.na(p20002_i0), "None", p20002_i0),
  p20002_i0 = as.character(p20002_i0),
  related_disease = case_when(
    str_detect(p20002_i0, "hypert") | str_detect(p6150_i0, "High") | str_detect(p20002_i0, "myocardial") |
      str_detect(p6150_i0, "Heart") | str_detect(p20002_i0, "stroke") | str_detect(p20002_i0, "ischaemic") |
      str_detect(p20002_i0, "haemorrhage") | str_detect(p6150_i0, "Stroke") | str_detect(p20002_i0, "cholesterol") |
      str_detect(p20002_i0, "cholangitis") | str_detect(p20002_i0, "cholelithiasis") |
      str_detect(p20002_i0, "cholecyst") | str_detect(p20002_i0, "primary biliary cirrhosis") |
      str_detect(p20002_i0, "alcoholic cirrhosis") | str_detect(p6150_i0, "Angina") |  p2443_i0 == "Yes" ~ "yes",
    p2443_i0 == "No" | p6150_i0 == "None" | p20002_i0 == "None" ~ "none of the above",
    str_detect(p2443_i0, "know") | str_detect(p2443_i0, "answer") ~ "none of the above",
    TRUE ~ "none of the above"
  ))

data <- data %>% mutate(
  p20107_i0 = ifelse(is.na(p20107_i0), "None", p20107_i0),
  p20110_i0 = ifelse(is.na(p20110_i0), "None", p20110_i0),
  p20111_i0 = ifelse(is.na(p20111_i0), "None", p20111_i0),
  p20107_i0 = as.character(p20107_i0),
  p20110_i0 = as.character(p20110_i0),
  p20111_i0 = as.character(p20111_i0),
  disease_family = case_when(
    str_detect(p20107_i0, "Diabetes") | str_detect(p20110_i0, "Diabetes") | str_detect(p20111_i0, "Diabetes") |
      str_detect(p20107_i0, "High blood pressure") | str_detect(p20110_i0, "High blood pressure") |
      str_detect(p20111_i0, "High blood pressure") | str_detect(p20107_i0, "Stroke") | str_detect(p20110_i0, "Stroke") |
      str_detect(p20111_i0, "Stroke") | str_detect(p20107_i0, "Heart disease") | str_detect(p20110_i0, "Heart disease") |
      str_detect(p20111_i0, "Heart disease") ~ "yes",
    p20107_i0 == "None" | p20110_i0 == "None" | p20111_i0 == "None" ~ "none of the above",
    TRUE ~ "none of the above" # If none of the conditions match
  ),
  disease_family = as.factor(disease_family),
  cancer = case_when(
    str_detect(p2453_i0, "Do not know") ~ "don't know",
    str_detect(p2453_i0, "Yes") ~ "yes",
    p2453_i0 == "No" ~ "no",
    str_detect(p2453_i0, "answer") ~ "no answer",
    TRUE ~ "no answer"
  ),
  cancer = as.factor(cancer))



data <- data %>% mutate(
  cohabitation = case_when(
    p709_i0 == 1 ~ "alone",
    str_detect(p6141_i0, "Husband, wife or partner") ~ "with spouse/partner",
    p6141_i0 == "Son and/or daughter (include step-children)" |
      p6141_i0 == "Son and/or daughter (include step-children)|Brother and/or sister"|
      p6141_i0 == "Son and/or daughter (include step-children)|Mother and/or father" |
      p6141_i0 == "Son and/or daughter (include step-children)|Other related" |
      p6141_i0 == "Son and/or daughter (include step-children)|Grandchild" |
      p6141_i0 == "Son and/or daughter (include step-children)|Grandchild|Other related" |
      p6141_i0 == "Son and/or daughter (include step-children)|Grandparent"|
      p6141_i0 == "Son and/or daughter (include step-children)|Grandchild|Other related|Other unrelated" |
      p6141_i0 == "Son and/or daughter (include step-children)|Grandchild|Other unrelated" |
      p6141_i0 == "Son and/or daughter (include step-children)|Other related|Other unrelated" |
      p6141_i0 == "Son and/or daughter (include step-children)|Other unrelated" |
      p6141_i0 == "Son and/or daughter (include step-children)|Brother and/or sister|Other related" |
      p6141_i0 == "Son and/or daughter (include step-children)|Brother and/or sister|Other unrelated" |
      p6141_i0 == "Mother and/or father" |
      p6141_i0 == "Mother and/or father|Grandchild" |
      p6141_i0 == "Mother and/or father|Grandparent" |
      p6141_i0 == "Mother and/or father|Other related" |
      p6141_i0 == "Mother and/or father|Other unrelated" |
      p6141_i0 == "Son and/or daughter (include step-children)|Brother and/or sister|Mother and/or father" |
      p6141_i0 == "Son and/or daughter (include step-children)|Mother and/or father|Grandchild"|
      p6141_i0 == "Son and/or daughter (include step-children)|Mother and/or father|Other related" |
      p6141_i0 == "Son and/or daughter (include step-children)|Mother and/or father|Other unrelated" |
      p6141_i0 == "Son and/or daughter (include step-children)|Mother and/or father|Grandchild|Other unrelated" |
      p6141_i0 == "Son and/or daughter (include step-children)|Brother and/or sister|Mother and/or father|Other related" |
      p6141_i0 == "Other related" |
      p6141_i0 == "Other unrelated"
    ~ "other non-partner",
    p6141_i0 == "Prefer not to answer"
    ~ "no answer"
  ))

data <- data %>% mutate(
  smoking = case_when(
    str_detect(p20116_i0, "Never") ~ "never",
    str_detect(p20116_i0, "Previous") ~ "former",
    str_detect(p20116_i0, "Current") & as.numeric(p3456_i0) > 0 & as.numeric(p3456_i0) <= 15 ~ "current <15",
    str_detect(p20116_i0, "Current") & as.numeric(p3456_i0) > 15 ~ "current > 15",
    str_detect(p20116_i0, "answer") ~ "no answer",
    TRUE ~ "no answer"  # Handling cases not covered by the conditions
  ))

data <- data %>% mutate(
  p26030_i0 = ifelse(is.na(p26030_i0), 0, p26030_i0),
  p26030_i1 = ifelse(is.na(p26030_i1), 0, p26030_i1),
  p26030_i2 = ifelse(is.na(p26030_i2), 0, p26030_i2),
  p26030_i3 = ifelse(is.na(p26030_i3), 0, p26030_i3),
  p26030_i4 = ifelse(is.na(p26030_i4), 0, p26030_i4))

data <- data %>% mutate(
  alcohol_intake = rowSums(select(., starts_with("p26030"))))

data <- data %>% mutate(
  alcohol_daily = alcohol_intake/p20077)

data <- data %>% mutate(
  alcohol_weekly = alcohol_daily * 7)


data <- data %>% mutate(
  region = case_when(
    str_detect(p54_i0, "Barts") | str_detect(p54_i0, "Croydon") | str_detect(p54_i0, "Hounslow")  ~ "London",
    str_detect(p54_i0, "Cardiff") | str_detect(p54_i0, "Swansea") | str_detect(p54_i0, "Wrexham") ~ "Wales",
    str_detect(p54_i0, "Bury") | str_detect(p54_i0, "Stockport") | str_detect(p54_i0, "Liverpool") | str_detect(p54_i0, "Manchester") ~ "North_West",
    str_detect(p54_i0, "Newcastle") | str_detect(p54_i0, "Middlesborough") ~ "North_East",
    str_detect(p54_i0, "Leeds") | str_detect(p54_i0, "Sheffield") ~ "Yorkshire_Humber",
    str_detect(p54_i0, "Birmingham") | str_detect(p54_i0, "Cheadle")  ~ "West_Midlands",
    str_detect(p54_i0, "Nottingham") | str_detect(p54_i0, "Stoke") ~ "East_Midlands",
    str_detect(p54_i0, "Oxford") | str_detect(p54_i0, "Reading") ~ "South_East",
    str_detect(p54_i0, "Bristol") ~ "South_West",
    str_detect(p54_i0, "Edinburgh") | str_detect(p54_i0, "Glasgow") ~ "Scotland"
  ),
  region = as.factor(region),
  pea_servings = case_when(
    str_detect(p104280_i0, "1") | str_detect(p104280_i1, "1") | str_detect(p104280_i2, "1") |
      str_detect(p104280_i3, "1") | str_detect(p104280_i4, "1") ~ 1,
    str_detect(p104280_i0, "2") | str_detect(p104280_i1, "2") | str_detect(p104280_i2, "2") |
      str_detect(p104280_i3, "2") | str_detect(p104280_i4, "2") ~ 2,
    str_detect(p104280_i0, "3+") | str_detect(p104280_i1, "3+") | str_detect(p104280_i2, "3+") |
      str_detect(p104280_i3, "3+") | str_detect(p104280_i4, "3+") ~ 3,
    str_detect(p104280_i0, "half") | str_detect(p104280_i1, "half") | str_detect(p104280_i2, "half") |
      str_detect(p104280_i3, "half") | str_detect(p104280_i4, "half") ~ 0.5,
    str_detect(p104280_i0, "quarter") | str_detect(p104280_i1, "quarter") | str_detect(p104280_i2, "quarter") |
      str_detect(p104280_i3, "quarter") | str_detect(p104280_i4, "quarter") ~ 0.25),
  pea_servings = as.numeric(pea_servings),
  peas = pea_servings * 80) #assuming 1 serving 80g


# Removing individuals with missing information on covariates
filtered_data <- subset(data, !is.na(age))
filtered_data <- subset(filtered_data, !is.na(region))
filtered_data <- subset(filtered_data, !is.na(sex))
filtered_data <- subset(filtered_data, !is.na(ethnicity)) #126772
filtered_data <- subset(filtered_data, !is.na(deprivation)) #126623
filtered_data <- subset(filtered_data, !is.na(education)) #126263
filtered_data <- subset(filtered_data, !is.na(cohabitation)) # 125643
filtered_data <- subset(filtered_data, !is.na(physical_activity)) # no change
filtered_data <- subset(filtered_data, !is.na(smoking)) # no change
filtered_data <- subset(filtered_data, !is.na(related_disease)) # no change
filtered_data <- subset(filtered_data, !is.na(disease_family)) # no change
filtered_data <- subset(filtered_data, !is.na(yearly_income)) # no change
filtered_data <- subset(filtered_data, !is.na(bmi30)) #123822
data <- filtered_data


# Remove recoded variables from sorted data -------------------------------


variables_to_remove <- c("p20111", "p20110", "p20107", "p23104",
                         "p6150", "p20002", "p2453", "p2443", "p31",
                         "p20116", "p26030", "p3456", "p21022",
                         "p22040", "p6141", "p6138", "p22189",
                         "p21000", "p54", "p738", "p30650",
                         "p30620", "p20165", "p100002",
                         "p100001", "p709")

data <- data %>%
  select(-matches(variables_to_remove))


