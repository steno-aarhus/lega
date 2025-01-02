# Data wrangling functions

# Data management ---------------------------------------------------------
two_recalls <- function(data) {
    data <- data %>%
        subset(p20077 >= 2) %>%
        mutate(p20077 = as.numeric(p20077))
    return(data)
}

## Add id number -----------------------------------------------------------
data_id <- function(data) {
    data <- data %>%
        dplyr::mutate(id = 1:n(), .before = everything())
    return(data)
}

## Wrangle covariates (not food) -------------------------------------------
# sociodemographic factors
sociodemographics <- function(data) {
    data <- data %>% mutate(
        sex = as.factor(p31),
        age = as.numeric(p21022),
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
            p21000_i0 == 1|p21000_i0 == 1001|p21000_i0 == 1002|p21000_i0 == 1003 ~ "White",
            TRUE ~ "other"
        ),
        deprivation = p22189,
        yearly_income = case_when(
            p738_i0 == "1" ~ "<18,000",
            p738_i0 == "2" ~ "18,000-30,999",
            p738_i0 == "3" ~ "31,000-51,999",
            p738_i0 == "4" ~ "52,000-100,000",
            p738_i0 == "5" ~ ">100,000",
            TRUE ~ "unknown"
        ),
        yearly_income = as.factor(yearly_income),
        education = case_when(
            str_detect(p6138_i0, "1") ~ "High",
            str_detect(p6138_i0, "2") | str_detect(p6138_i0, "3")  ~ "Intermediate",
            str_detect(p6138_i0, "4") | str_detect(p6138_i0, "5") |
                str_detect(p6138_i0, "6") | str_detect(p6138_i0, "-7")  ~ "Low",
            str_detect(p6138_i0, "-3") ~ "Unknown"
        ),
        education = as.factor(education),
        physical_activity = case_when(
            p22040_i0 <= 918 ~ "low",
            p22040_i0 > 918 & p22040_i0 <= 3706 ~ "moderate",
            p22040_i0 > 3706 ~ "high",
            TRUE ~ "unknown"
        ),
        cohabitation = case_when(
            p709_i0 == 1 ~ "alone",
            str_detect(p6141_i0, "Husband, wife or partner") ~ "with spouse/partner",
            p6141_i0 == "Prefer not to answer" ~ "no answer",
            TRUE ~ "other non-partner"
        ),
        # 10 UK recruitment regions
        region = case_when(
            p54_i0 == 11012 | p54_i0 == 11020 | p54_i0 == 11018 ~ "London",
            p54_i0 == 11003 | p54_i0 == 11022 | p54_i0 == 11023 ~ "Wales",
            p54_i0 == 11008 | p54_i0 == 10003 | p54_i0 == 11016 | p54_i0 == 11001  ~ "North-West",
            p54_i0 == 11009 | p54_i0 == 11017 ~ "North-East",
            p54_i0 == 11010 | p54_i0 == 11014 ~ "Yorkshire-Humber",
            p54_i0 == 11021 | p54_i0 == 11024 ~ "West-Midlands",
            p54_i0 == 11013 | p54_i0 == 11006 ~ "East-Midlands",
            p54_i0 == 11002 | p54_i0 == 11007 ~ "South-East",
            p54_i0 == 11011 ~ "South-West",
            p54_i0 == 11005 | p54_i0 == 11004 ~ "Scotland"
        ),
        region = as.factor(region)
    )
    return(data)
}

#lifestyle variables
lifestyle <- function(data) {
    data <- data %>% mutate(
        p3456_i0 = case_when(
            p3456_i0 == -10| p3456_i0 == -1| p3456_i0 == -3 ~ 0,
            TRUE ~ p3456_i0
        ),
        smoking = case_when(
            str_detect(p20116_i0, "0") ~ "never",
            str_detect(p20116_i0, "1") ~ "former",
            str_detect(p20116_i0, "2") & as.numeric(p3456_i0) > 0 & as.numeric(p3456_i0) <= 15 ~ "current <15",
            str_detect(p20116_i0, "2") & as.numeric(p3456_i0) > 15 ~ "current > 15",
            TRUE ~ "no answer" # Handling cases not covered by the conditions
        ),
        # bmi
        bmi = p23104_i0,
        bmi = as.numeric(bmi),
        bmi30 = ifelse(p23104_i0 >= 30, 1, 0),
        bmi30 = as.numeric(bmi30)
    )
    return(data)
}

alcohol <- function(data) {
    data <- data %>% mutate(
        p26030_i0 = ifelse(is.na(p26030_i0), 0, p26030_i0),
        p26030_i1 = ifelse(is.na(p26030_i1), 0, p26030_i1),
        p26030_i2 = ifelse(is.na(p26030_i2), 0, p26030_i2),
        p26030_i3 = ifelse(is.na(p26030_i3), 0, p26030_i3),
        p26030_i4 = ifelse(is.na(p26030_i4), 0, p26030_i4)
    )
    return(data)
}

alcohol_intake <- function(data) {
    data <- data %>% mutate(
        alcohol_intake = rowSums(pick(matches("p26030")), na.rm = TRUE),
        alcohol_daily = alcohol_intake / p20077,
        alcohol_weekly = alcohol_daily * 7,
        alc_spline = splines::bs(alcohol_weekly, knots = 4, degree = 3)
    )
    return(data)
}

# related factors or family history of related diseases
illness <- function(data) {
    data <- data %>% mutate(
        related_conditions = case_when(
            str_detect(p20002_i0, "1473") | str_detect(p20002_i0, "1155") |
                str_detect(p20002_i0, "1158") | str_detect(p20002_i0, "1506") |
                str_detect(p20002_i0, "1604") | p2443_i0 == 1 ~ "yes",
            TRUE ~ "none of the above"),
        family_diabetes = case_when(
            str_detect(p20107_i0, "9") | str_detect(p20110_i0, "9") | str_detect(p20111_i0, "9") ~ "yes",
            TRUE ~ "no"),
        weight_loss = case_when(
            p2306_i0 == 3 ~ "yes",
            TRUE ~ "no")
    )
    return(data)
}

# female hormone related factors
hormones <- function(data) {
    data <- data %>% mutate(
        hrt = case_when(
            p2814_i0 == 1 ~ "yes",
            p2814_i0 == 0 ~ "no",
            TRUE ~ "unknown"),
        oral_contraceptive = case_when(
            p2784_i0 == 1 ~ "yes",
            p2784_i0 == 0 ~ "no",
            TRUE ~ "unknown"),
        estrogen_treatment = case_when(
            hrt == "yes" | oral_contraceptive == "yes" ~ "yes",
            TRUE ~ "no"),
        # preparing pregnancy outcomes for summed pregnancies
        # converting from character to numeric. All no answers coded as NA
        p3839_i0 = ifelse((p3839_i0 == -1), NA, p3839_i0),
        p3839_i0 = ifelse((p3839_i0 == -3), NA, p3839_i0),
        p3839_i0 = as.numeric(p3839_i0),
        p3849_i0 = ifelse((p3849_i0 == -1), NA, p3849_i0),
        p3849_i0 = ifelse((p3849_i0 == -3), NA, p3849_i0),
        p3849_i0 = as.numeric(p3849_i0),
        p2734_i0 = as.numeric(p2734_i0),
        p3829_i0 = ifelse((p3829_i0 == -1), NA, p3829_i0),
        p3829_i0 = ifelse((p3829_i0 == -3), NA, p3829_i0),
        p3829_i0 = as.numeric(p3829_i0)
    )
    return(data)
}

# pregnancies
pregnancies <- function(data) {
    data <- data %>%
        mutate(
            pregnancies = case_when(
                sex == 0 ~ rowSums(select(., p2734_i0, p3829_i0, p3839_i0, p3849_i0), na.rm = TRUE),
                sex == 1 ~ NA_real_
            )
        )
    return(data)
}

# bilirubin
bilirubin <- function(data) {
    data <- data %>% mutate(
        bilirubin = p30840_i0,
        bilirubin = as.numeric(bilirubin),
        bili_cat = case_when(
            p30840_i0 < 21 ~ "normal",
            p30840_i0 >= 21 ~ "elevated",
            TRUE ~ "unknown"
        ),
        bili_cat = as.factor(bili_cat)
    )
    return(data)
}

# Removing individuals with missing information on covariates
remove_missings <- function(data) {
    data <- data %>%
        filter(
            !is.na(age),
            !is.na(region),
            !is.na(sex),
            !is.na(ethnicity),
            !is.na(deprivation),
            !is.na(education),
            !is.na(cohabitation),
            !is.na(physical_activity),
            !is.na(smoking),
            !is.na(related_conditions),
            !is.na(family_diabetes),
            !is.na(yearly_income),
            !is.na(bmi30)
        )
    return(data)
}


# remove recoded variables before modelling diet variables
remove_p_vars <- function(data) {
    data <- data %>%
        select(-matches(c(
            "p20111", "p20110", "p20107", "p23104",
            "p6150", "p20002", "p2453", "p2443", "p31",
            "p20116", "p26030", "p3456", "p21022",
            "p22040", "p6141", "p6138", "p22189",
            "p21000", "p54", "p738", "p30650",
            "p30620", "p20165", "p100002",
            "p100001", "p709", "p3839", "p3829", "p3849",
            "p2784", "p2814", "p2306", "p30840", "p2734"
        )))
    return(data)
}


## Create diet variables ---------------------------------------------------

# estimate intake of peas based on pea servings
pea_servings <- function(data) {
    data <- data %>% mutate(
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
                str_detect(p104280_i3, "quarter") | str_detect(p104280_i4, "quarter") ~ 0.25
        ),
        pea_servings = as.numeric(pea_servings),
        peas = pea_servings * 80 # assuming 1 serving 80g
    )
    return(data)
}

# calculate weekly intake of food groups
calculate_weekly_diet <- function(variables, number_recalls) {
    rowSums(dplyr::pick(tidyselect::matches(variables)), na.rm = TRUE) / number_recalls * 7
}

# creating food groups from UKB Aurora Perez
food_groups <- function(data) {
    data <- data %>%
        mutate(
            legume_weekly = calculate_weekly_diet("p26086|p26101|p26136|p26137", p20077),
            meats_weekly = calculate_weekly_diet("p26066|p26100|p26104|p26117|p26122", p20077),
            poultry_weekly = calculate_weekly_diet("p26121|p26069", p20077),
            fish_weekly = calculate_weekly_diet("p26070|p26109|p26132|p26149", p20077),
            cereal_refined_weekly = calculate_weekly_diet("p26113|p26079|p26071|p26072|p26073|p26075|p26068|p26083", p20077),
            whole_grain_weekly = calculate_weekly_diet("p26074|p26076|p26077|p26078|p26105|p26114", p20077),
            mixed_dish_weekly = calculate_weekly_diet("p26128|p26097|p26116|p26135|p26139|p26145", p20077),
            dairy_weekly = calculate_weekly_diet("p26154|p26087|p26096|p26102|p26103|p26099|p26131|p26133|p26150", p20077),
            fats_weekly = calculate_weekly_diet("p26112|p26062|p26063|p26155|p26110|p26111", p20077),
            fruit_weekly = calculate_weekly_diet("p26089|p26090|p26091|p26092|p26093|p26094", p20077),
            nut_weekly = calculate_weekly_diet("p26107|p26108", p20077),
            veggie_weekly = calculate_weekly_diet("p26065|p26098|p26115|p26123|p26125|p26143|p26146|p26147|p26144", p20077),
            potato_weekly = calculate_weekly_diet("p26118|p26119|p26120", p20077),
            egg_weekly = calculate_weekly_diet("p26088", p20077),
            non_alc_beverage_weekly = calculate_weekly_diet("p26124|p26141|p26142|p26148|p26081|p26082|p26095|p26126|p26127", p20077),
            alc_beverage_weekly = calculate_weekly_diet("p26151|p26152|p26153|p26067|p26138", p20077),
            snack_weekly = calculate_weekly_diet("p26106|p26140|p26134|p26084|p26085|p26064|p26080", p20077),
            sauce_weekly = calculate_weekly_diet("p26129|p26130", p20077),
            legume_pea_weekly = calculate_weekly_diet("p26086|p26101|p26136|p26137|peas", p20077),
            veggie_pea_weekly = ((rowSums(pick(matches("p26065|p26098|p26147|p26123|p26125|p26143|p26146")), na.rm = TRUE) - peas) / p20077) * 7,
            legume_no_soymilk = calculate_weekly_diet("p26086|p26101|p26137", p20077), # removing soy milk from legumes
            non_alc_beverage_soymilk_weekly = calculate_weekly_diet("p26136|p26124|p26141|p26142|p26148|p26081|p26082|p26095|p26126|p26127", p20077),
            legume_soy_meat = calculate_weekly_diet("p26086|p26101|p26137", p20077), # removing soy milk and soy desert from legumes
            food_weight_weekly = legume_weekly + meats_weekly + poultry_weekly + fish_weekly + cereal_refined_weekly + whole_grain_weekly +
                mixed_dish_weekly + dairy_weekly + fats_weekly + fruit_weekly + nut_weekly + veggie_weekly + potato_weekly + egg_weekly +
                non_alc_beverage_weekly + alc_beverage_weekly + snack_weekly + sauce_weekly
        )
    return(data)
}

# remove recoded diet p-variables before modelling outcomes
remove_diet_p_vars <- function(data) {
    data <- data %>% select(-matches(c(
        "p26113", "p26079", "p26071", "p26072", "p26073", "p26075",
        "p26068", "p26083", "p26074", "p26076", "p26077", "p26078",
        "p26105", "p26114", "p26128", "p26097", "p26116", "p26135",
        "p26139", "p26154", "p26087", "p26096", "p26102", "p26103",
        "p26099", "p26131", "p26133", "p26150", "p26112", "p26062",
        "p26063", "p26155", "p26110", "p26111", "p26089", "p26090",
        "p26091", "p26092", "p26093", "p26094", "p26107", "p26108",
        "p26065", "p26098", "p26115", "p26123", "p26125", "p26143",
        "p26146", "p26147", "p26144", "p26118", "p26119", "p26120",
        "p26088", "p26145", "p26124", "p26141", "p26142", "p26148",
        "p26081", "p26082", "p26095", "p26126", "p26127", "p26151",
        "p26152", "p26153", "p26067", "p26138", "p26106", "p26140",
        "p26134", "p26084", "p26085", "p26064", "p26080", "p26129",
        "p26130", "p26086", "p26101", "p26136", "p26137", "p26066",
        "p26100", "p26104", "p26117", "p26122", "p26121", "p26069",
        "p26070", "p26109", "p26132", "p26149", "p26000", "p104280"
    )))
    return(data)
}


## Create outcome variables for time to event ------------------------------
# ICD10 diagnoses codes
icd10_diagnoses <- function(data) {
    icd10_subset <- data %>%
        select(starts_with("p41270"), starts_with("p41280"), "id") %>%
        # splitting diagnoses string-variable each time a | is in the string
        separate_wider_delim(p41270, delim = "|", names = paste0("p41270var_a", 0:258), too_few = "debug") %>%
        select(matches("p41270|p41280|id")) %>%
        pivot_longer(cols = matches("_a[0-9]*$"), names_to = c(".value", "a"), names_sep = "_") %>%

        # creating outcome variables with date info from p41280
        mutate(
            # gallstone (K80.X)
            icd10_gallstone_date = ifelse(str_detect(p41270var, "K80"), as.character(c_across(starts_with("p41280"))), NA),
            icd10_gallstone_date = as.Date(icd10_gallstone_date, format = "%Y-%m-%d"),
            # cholecystitis (K81.X)
            icd10_cholecystit_date = ifelse(str_detect(p41270var, "K81"), as.character(c_across(starts_with("p41280"))), NA),
            icd10_cholecystit_date = as.Date(icd10_cholecystit_date, format = "%Y-%m-%d"),
            # gallbladder obstruction (K82.0)
            icd10_gbobstruction_date = ifelse(str_detect(p41270var, "K820"), as.character(c_across(starts_with("p41280"))), NA),
            icd10_gbobstruction_date = as.Date(icd10_gbobstruction_date, format = "%Y-%m-%d"),
            # bile obstruction (K83.1)
            icd10_bileobstruction_date = ifelse(str_detect(p41270var, "K831"), as.character(c_across(starts_with("p41280"))), NA),
            icd10_bileobstruction_date = as.Date(icd10_bileobstruction_date, format = "%Y-%m-%d")
        ) %>%

        # retrieve first diagnosis date
        select(id, icd10_gallstone_date, icd10_cholecystit_date, icd10_gbobstruction_date, icd10_bileobstruction_date) %>%
        pivot_longer(cols = starts_with("icd10_"), names_to = "condition", values_to = "date") %>%
        filter(!is.na(date)) %>%
        group_by(id, condition) %>%
        slice(1) %>%
        pivot_wider(names_from = condition, values_from = date) %>%
        ungroup()

    # Join the dates back to the original data
    data <- data %>%
        left_join(icd10_subset, by = "id")

    return(data)
}

# ICD9 diagnoses codes
icd9_diagnoses <- function(data) {
    icd9_subset <- data %>%
        select(starts_with("p41271"), starts_with("p41281"), "id") %>%
        # splitting diagnoses string-variable each time a | is in the string
        separate_wider_delim(p41271, delim = "|", names = paste0("p41271var_a", 0:46), too_few = "debug") %>%
        select(matches("p41271|p41281|id")) %>%
        pivot_longer(cols = matches("_a[0-9]*$"), names_to = c(".value", "a"), names_sep = "_") %>%

        # creating outcome variables with date info from p41281
        mutate(
            # gallstone
            icd9_gallstone_date = ifelse(str_detect(p41271var, "574"), as.character(c_across(starts_with("p41281"))), NA),
            icd9_gallstone_date = as.Date(icd9_gallstone_date, format = "%Y-%m-%d"),
            # acute cholecystitis
            icd9_acute_date = ifelse(str_detect(p41271var, "5750"), as.character(c_across(starts_with("p41281"))), NA),
            icd9_acute_date = as.Date(icd9_acute_date, format = "%Y-%m-%d"),
            # other cholecystitis
            icd9_other_date = ifelse(str_detect(p41271var, "5751"), as.character(c_across(starts_with("p41281"))), NA),
            icd9_other_date = as.Date(icd9_other_date, format = "%Y-%m-%d"),
            # bile duct obstruction (5762)
            icd9_bileobstruction_date = ifelse(str_detect(p41271var, "5762"), as.character(c_across(starts_with("p41281"))), NA),
            icd9_bileobstruction_date = as.Date(icd9_bileobstruction_date, format = "%Y-%m-%d")
        ) %>%

    # Retrieve the first non-NA date for each outcome
        select(id, icd9_gallstone_date, icd9_acute_date, icd9_other_date, icd9_bileobstruction_date) %>%
        pivot_longer(cols = starts_with("icd9_"), names_to = "condition", values_to = "date") %>%
        filter(!is.na(date)) %>%
        group_by(id, condition) %>%
        slice(1) %>%
        pivot_wider(names_from = condition, values_from = date) %>%
        ungroup()

    # Join the dates back to the original data
    data <- data %>%
        left_join(icd9_subset, by = "id")

    return(data)
}

# opsc4 diagnoses codes
opcs4_diagnoses <- function(data) {
    opcs4 <- data %>%
        select(starts_with("p41272"), starts_with("p41282"), "id") %>%
        # splitting diagnoses string-variable each time a | is in the string
        separate_wider_delim(p41272, delim = "|", names = paste0("p41272var_a", 0:125), too_few = "debug") %>%
        select(matches("p41272|p41282|id")) %>%
        pivot_longer(cols = matches("_a[0-9]*$"), names_to = c(".value", "a"), names_sep = "_") %>%

        # creating outcome variables with date info from p41282
        mutate(
            # gallbladder removal
            opcs4_removal_date = case_when(
                str_detect(p41272var, "J181") ~ as.character(c_across(starts_with("p41282"))),
                str_detect(p41272var, "J182") ~ as.character(c_across(starts_with("p41282"))),
                str_detect(p41272var, "J183") ~ as.character(c_across(starts_with("p41282"))),
                str_detect(p41272var, "J188") ~ as.character(c_across(starts_with("p41282"))),
                str_detect(p41272var, "J189") ~ as.character(c_across(starts_with("p41282"))),
                TRUE ~ NA_character_),
            opcs4_removal_date = as.Date(opcs4_removal_date, format = "%Y-%m-%d"),
            opcs4_gallstone_date = case_when(
                str_detect(p41272var, "J331") ~ as.character(c_across(starts_with("p41282"))),
                str_detect(p41272var, "J332") ~ as.character(c_across(starts_with("p41282"))),
                str_detect(p41272var, "J521") ~ as.character(c_across(starts_with("p41282"))),
                str_detect(p41272var, "J211") ~ as.character(c_across(starts_with("p41282"))),
                str_detect(p41272var, "J411") ~ as.character(c_across(starts_with("p41282"))),
                str_detect(p41272var, "J413") ~ as.character(c_across(starts_with("p41282"))),
                str_detect(p41272var, "J491") ~ as.character(c_across(starts_with("p41282"))),
                str_detect(p41272var, "J492") ~ as.character(c_across(starts_with("p41282"))),
                str_detect(p41272var, "J242") ~ as.character(c_across(starts_with("p41282"))),
                str_detect(p41272var, "J243") ~ as.character(c_across(starts_with("p41282"))),
                str_detect(p41272var, "J261") ~ as.character(c_across(starts_with("p41282"))),
                TRUE ~ NA_character_),
            opcs4_gallstone_date = as.Date(opcs4_gallstone_date, format = "%Y-%m-%d")
        ) %>%

        # Retrieve the first non-NA date for each outcome
        select(id, opcs4_removal_date, opcs4_gallstone_date) %>%
        pivot_longer(cols = starts_with("opcs4_"), names_to = "condition", values_to = "date") %>%
        filter(!is.na(date)) %>%
        group_by(id, condition) %>%
        slice(1) %>%
        pivot_wider(names_from = condition, values_from = date) %>%
        ungroup()

    # Join the dates back to the original data
    data <- data %>%
        left_join(opcs4, by = "id")

    return(data)
}

# opcs3 diagnoses codes
opcs3_diagnoses <- function(data) {
    opcs3 <- data %>%
        select(starts_with("p41273"), starts_with("p41283"), "id") %>%
        # splitting diagnoses string-variable each time a | is in the string
        separate_wider_delim(p41273, delim = "|", names = paste0("p41272var_a", 0:15), too_few = "debug") %>%
        select(matches("p41273|p41283|id")) %>%
        pivot_longer(cols = matches("_a[0-9]*$"), names_to = c(".value", "a"), names_sep = "_") %>%

        # creating outcome variables with date info from p41283
        mutate(
            opcs3_removal_date = case_when(
                str_detect(p41273, "522") ~ as.character(c_across(starts_with("p41283"))),
                str_detect(p41273, "5222") ~ as.character(c_across(starts_with("p41283"))),
                TRUE ~ NA_character_),
            opcs3_removal_date = as.Date(opcs3_removal_date, format = "%Y-%m-%d"),
            opcs3_gallstone_date = ifelse(str_detect(p41273, "511"),
                                          as.character(c_across(starts_with("p41283"))),
                                          NA),
            opcs3_gallstone_date = as.Date(opcs3_gallstone_date, format = "%Y-%m-%d")
        ) %>%

        # Retrieve the first non-NA date for each outcome
        select(id, opcs3_removal_date, opcs3_gallstone_date) %>%
        pivot_longer(cols = starts_with("opcs3_"), names_to = "condition", values_to = "date") %>%
        filter(!is.na(date)) %>%
        group_by(id, condition) %>%
        slice(1) %>%
        pivot_wider(names_from = condition, values_from = date) %>%
        ungroup()


    # Join the dates back to the original data
    data <- data %>%
        left_join(opcs3, by = "id")

    return(data)
}


# Defining birth date as origin for survival analysis
date_birth <- function(data) {
    data <- data %>%
        mutate(
            date_birth = paste(p34, p52, "15", sep = "-"), # Combine year, month, and day into "YYYY-MM-DD"
            date_birth = as.Date(date_birth, format = "%Y-%m-%d") # Convert to Date format
        )
    return(data)
}

# Estimate last follow-up date for ICD10 codes (stagnation of diagnoses)
censoring_date <- function(data) {
    # Estimate last date of diagnoses
    dates <- data %>%
        subset(!is.na(icd10_gallstone_date))

    # Find the last date of diagnosis
    last_date <- max(dates$icd10_gallstone_date) %>% print()

    data <- data %>%
        mutate(censoring = as.Date(last_date))
    return(data)
}

# define variables for survival analyses
outcome_variables <- function(data) {
    data <- data %>%
        mutate(
            date_of_death = if_else(!is.na(p40000_i0), p40000_i0, p40000_i1),
            date_of_death = as.Date(date_of_death),
            loss_to_follow_up = p191,
            loss_to_follow_up = as.Date(loss_to_follow_up),
            # binary variable to indicate if outcome happened
            # gallstone
            gallstone = case_when(
                !is.na(icd10_gallstone_date) | !is.na(icd10_bileobstruction_date) |
                    !is.na(icd10_gbobstruction_date) | !is.na(icd9_gallstone_date) |
                    !is.na(icd9_bileobstruction_date) ~ 1,
                TRUE ~ 0),
            # cholecystectomy
            cholecystectomy = case_when(
                !is.na(opcs4_removal_date) | !is.na(opcs4_gallstone_date) |
                    !is.na(opcs3_removal_date) | !is.na(opcs3_gallstone_date) ~ 1,
                TRUE ~ 0),
            # cholecystitis
            cholecystit = case_when(
                !is.na(icd10_cholecystit_date) | !is.na(icd9_acute_date) |
                    !is.na(icd9_other_date) ~ 1,
                TRUE ~ 0),
            # any gallbladder disease
            gbd = case_when(
                gallstone == 1 | cholecystectomy == 1 | cholecystit == 1 ~ 1,
                TRUE ~ 0)
        )
    return(data)
}
# Eligibility criteria based on outcomes ----------------------------------


## Remove those with outcome before baseline (=before last webQ) --------
# time of last completed 24h recall as baseline date
last_completed_recall <- function(data) {
    data <- data %>%
        mutate(ques_comp_t0 = p105010_i0,
               ques_comp_t1 = p105010_i1,
               ques_comp_t2 = p105010_i2,
               ques_comp_t3 = p105010_i3,
               ques_comp_t4 = p105010_i4,
               # Removing specific time stamp
               ques_comp_t0 = substr(ques_comp_t0, 1, 10),
               ques_comp_t1 = substr(ques_comp_t1, 1, 10),
               ques_comp_t2 = substr(ques_comp_t2, 1, 10),
               ques_comp_t3 = substr(ques_comp_t3, 1, 10),
               ques_comp_t4 = substr(ques_comp_t4, 1, 10)) %>%

        #baseline start date as last completed questionnaire
        mutate(across(starts_with("ques_"), as.Date)) %>%
        # Gather all columns into key-value pairs
        pivot_longer(cols = starts_with("ques_"), names_to = "questionnaire", values_to = "date_filled") %>%
        # Group by participant ID and select the row with the latest date_filled for each participant
        group_by(id) %>%
        slice(which.max(date_filled)) %>%
        ungroup() %>%
        # Rename the remaining column to indicate the last filled questionnaire
        rename(last_filled_questionnaire = questionnaire) %>%
        mutate(date_filled = as.Date(date_filled))

    return(data)
}

# Removing those diagnosed before baseline
diagnosed_before <- function(data) {
    filtered_data <- data %>%
        filter(if_any(
            c(icd10_gallstone_date, icd10_bileobstruction_date, icd10_gbobstruction_date,
              icd9_gallstone_date, icd9_bileobstruction_date,
              opcs4_removal_date, opcs4_gallstone_date,
              opcs3_removal_date, opcs3_gallstone_date, icd10_cholecystit_date,
              icd9_acute_date, icd9_other_date),
            ~ . < date_filled
        ))

    data <- data %>%
        anti_join(filtered_data, by = "id")

    return(data)
}

# Removing those who were lost to follow-up or died before baseline
left_study <- function(data) {
    filtered_data <- data %>%
        filter(if_any(
            c(date_of_death, loss_to_follow_up),
            ~ . < date_filled
        ))

    data <- data %>%
        anti_join(filtered_data, by = "id")

    return(data)
}

# delete recoded outcome variables
remove_outcome_p_vars <- function(data) {
    data <- data %>% select(-matches(c(
        "p191", "p40000_i0", "p40000_i1","p105010_i0", "p105010_i1", "p105010_i2",
        "p105010_i3","p105010_i4", "p41280", "p41270", "p41281", "p41271", "p41282",
        "p41272","p41283", "p41273"
    )))
    return(data)
}


# Defining survival time as first occurrence of outcome ------------------
survival_time <- function(data) {
    data <- data %>%
        mutate(
            # gallstone or removal of gallbladder
            survival_time_gst = case_when(
                !is.na(icd10_gallstone_date) ~ as.numeric(difftime(icd10_gallstone_date, date_birth, units = "days")),
                !is.na(icd10_bileobstruction_date) ~ as.numeric(difftime(icd10_bileobstruction_date, date_birth, units = "days")),
                !is.na(icd10_gbobstruction_date) ~ as.numeric(difftime(icd10_gbobstruction_date, date_birth, units = "days")),
                !is.na(icd9_gallstone_date) ~ as.numeric(difftime(icd9_gallstone_date, date_birth, units = "days")),
                !is.na(icd9_bileobstruction_date) ~ as.numeric(difftime(icd9_bileobstruction_date, date_birth, units = "days")),
                !is.na(opcs4_gallstone_date) ~ as.numeric(difftime(opcs4_gallstone_date, date_birth, units = "days")),
                !is.na(opcs3_gallstone_date) ~ as.numeric(difftime(opcs3_gallstone_date, date_birth, units = "days")),
                !is.na(date_of_death) ~ as.numeric(difftime(date_of_death, date_birth, units = "days")),
                !is.na(loss_to_follow_up) ~ as.numeric(difftime(loss_to_follow_up, date_birth, units = "days")),
                TRUE ~ as.numeric(difftime(censoring, date_birth, units = "days"))
            ),
            # Using pmin to get the minimum survival time across columns
            survival_gallstone = pmin(survival_time_gst, na.rm = TRUE),
            survival_gallstone = survival_gallstone/365.25,
            # Remove temporary variable
            survival_time_gst = NULL,

            # removal of gallbladder
            survival_time_removal = case_when(
                !is.na(opcs4_removal_date) ~ as.numeric(difftime(opcs4_removal_date, date_birth, units = "days")),
                !is.na(opcs3_removal_date) ~ as.numeric(difftime(opcs3_removal_date, date_birth, units = "days")),
                !is.na(date_of_death) ~ as.numeric(difftime(date_of_death, date_birth, units = "days")),
                !is.na(loss_to_follow_up) ~ as.numeric(difftime(loss_to_follow_up, date_birth, units = "days")),
                TRUE ~ as.numeric(difftime(censoring, date_birth, units = "days"))
            ),
            # Using pmin to get the minimum survival time across columns
            survival_removal = pmin(survival_time_removal, na.rm = TRUE),
            survival_removal = survival_removal/365.25,
            # Remove temporary variable
            survival_time_removal = NULL,


            # cholecystitis
            survival_time_cholecystit = case_when(
                !is.na(icd10_cholecystit_date) ~ as.numeric(difftime(icd10_cholecystit_date, date_birth, units = "days")),
                !is.na(icd9_acute_date) ~ as.numeric(difftime(icd9_acute_date, date_birth, units = "days")),
                !is.na(icd9_other_date) ~ as.numeric(difftime(icd9_other_date, date_birth, units = "days")),
                !is.na(date_of_death) ~ as.numeric(difftime(date_of_death, date_birth, units = "days")),
                !is.na(loss_to_follow_up) ~ as.numeric(difftime(loss_to_follow_up, date_birth, units = "days")),
                TRUE ~ as.numeric(difftime(censoring, date_birth, units = "days"))
            ),
            # Use min to get the minimum survival time across columns
            survival_cholecystit = pmin(survival_time_cholecystit, na.rm = TRUE),
            survival_cholecystit = survival_cholecystit/365.25,
            # Remove temporary variable
            survival_time_cholecystit = NULL,

            # any gallbladder disease
            survival_time_gbd = case_when(
                !is.na(icd10_gallstone_date) ~ as.numeric(difftime(icd10_gallstone_date, date_birth, units = "days")),
                !is.na(icd10_bileobstruction_date) ~ as.numeric(difftime(icd10_bileobstruction_date, date_birth, units = "days")),
                !is.na(icd10_gbobstruction_date) ~ as.numeric(difftime(icd10_gbobstruction_date, date_birth, units = "days")),
                !is.na(icd9_gallstone_date) ~ as.numeric(difftime(icd9_gallstone_date, date_birth, units = "days")),
                !is.na(icd9_bileobstruction_date) ~ as.numeric(difftime(icd9_bileobstruction_date, date_birth, units = "days")),
                !is.na(opcs4_removal_date) ~ as.numeric(difftime(opcs4_removal_date, date_birth, units = "days")),
                !is.na(opcs4_gallstone_date) ~ as.numeric(difftime(opcs4_gallstone_date, date_birth, units = "days")),
                !is.na(opcs3_removal_date) ~ as.numeric(difftime(opcs3_removal_date, date_birth, units = "days")),
                !is.na(opcs3_gallstone_date) ~ as.numeric(difftime(opcs3_gallstone_date, date_birth, units = "days")),
                !is.na(icd10_cholecystit_date) ~ as.numeric(difftime(icd10_cholecystit_date, date_birth, units = "days")),
                !is.na(icd9_acute_date) ~ as.numeric(difftime(icd9_acute_date, date_birth, units = "days")),
                !is.na(icd9_other_date) ~ as.numeric(difftime(icd9_other_date, date_birth, units = "days")),
                !is.na(date_of_death) ~ as.numeric(difftime(date_of_death, date_birth, units = "days")),
                !is.na(loss_to_follow_up) ~ as.numeric(difftime(loss_to_follow_up, date_birth, units = "days")),
                TRUE ~ as.numeric(difftime(censoring, date_birth, units = "days"))
            ),
            # Use min to get the minimum survival time across columns
            survival_gbd = pmin(survival_time_gbd, na.rm = TRUE),
            survival_gbd = survival_gbd/365.25,
            # Remove temporary variable
            survival_time_gbd = NULL)

    return(data)
}


define_exposure_variables <- function(data) {
    data <- data %>%
        mutate(
            legumes80 = legume_weekly / 80,
            legumepea80 = legume_pea_weekly / 80,
            legumenosoy80 = legume_no_soymilk / 80,
            meats80 = meats_weekly / 80,
            poultry80 = poultry_weekly / 80,
            fish80 = fish_weekly / 80
        )
    return(data)
}

