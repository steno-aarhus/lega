# Diet data
# Load packages -----------------------------------------------------------
library(tidyverse)
library(magrittr)
library(dplyr)


# Average dietary intake of food groups -----------------------------------
# estimating average daily and weekly intakes of food groups in g
data <- data %>%
  # creating food groups from UKB Aurora Perez
  mutate(
    # refined cereals
    cereal_refined_total = rowSums(select(., starts_with("p26113") | starts_with("p26079") |
                                            starts_with("p26071") | starts_with("p26072") |
                                            starts_with("p26073") | starts_with("p26075") |
                                            starts_with("p26068") | starts_with("p26083")), na.rm = TRUE),
    cereal_refined_daily = cereal_refined_total/p20077,
    cereal_refined_weekly = cereal_refined_daily * 7,
    # whole-grain cereals
    whole_grain_total = rowSums(select(., starts_with("p26074") | starts_with("p26076") |
                                          starts_with("p26077") | starts_with("p26078") |
                                          starts_with("p26105") | starts_with("p26114")), na.rm = TRUE),
    whole_grain_daily = whole_grain_total/p20077,
    whole_grain_weekly = whole_grain_daily * 7,
    # mixed dishes
    mixed_dish_total = rowSums(select(., starts_with("p26128") | starts_with("p26097") |
                                        starts_with("p26116") | starts_with("p26135") |
                                        starts_with("p26139")), na.rm = TRUE),
    mixed_dish_daily = mixed_dish_total/p20077,
    mixed_dish_weekly = mixed_dish_daily * 7,
    # dairy
    dairy_total = rowSums(select(., starts_with("p26154") | starts_with("p26087") |
                                   starts_with("p26096") | starts_with("p26102") |
                                   starts_with("p26103") | starts_with("p26099") |
                                   starts_with("p26131") | starts_with("p26133") |
                                   starts_with("p26150")), na.rm = TRUE),
    dairy_daily = dairy_total/p20077,
    dairy_weekly = dairy_daily * 7,
    # fats and spread
    fats_total = rowSums(select(., starts_with("p26112") | starts_with("p26062") |
                                  starts_with("p26063") | starts_with("p26155") |
                                  starts_with("p26110") | starts_with("p26111")), na.rm = TRUE),
    fats_daily = fats_total/p20077,
    fats_weekly = fats_daily * 7,
    # fruit
    fruit_total = rowSums(select(., starts_with("p26089") | starts_with("p26090") |
                                   starts_with("p26091") | starts_with("p26092") |
                                   starts_with("p26093") | starts_with("p26094")), na.rm = TRUE),
    fruit_daily = fruit_total/p20077,
    fruit_weekly = fruit_daily * 7,
        # nuts and seeds
    nut_total = rowSums(select(., starts_with("p26107") | starts_with("p26108")), na.rm = TRUE),
    nut_daily = nut_total/p20077,
    nut_weekly = nut_daily*7,
    # vegetables
    veggie_total = rowSums(select(., starts_with("p26065") | starts_with("p26098") |
                                    starts_with("p26115") | starts_with("p26123") |
                                    starts_with("p26125") | starts_with("p26143") |
                                    starts_with("p26146") | starts_with("p26147") |
                                    starts_with("p26144")), na.rm = TRUE),
    veggie_daily = veggie_total/p20077,
    veggie_weekly = veggie_daily * 7,
    # potatoes
    potato_total = rowSums(select(., starts_with("p26118") | starts_with("p26119") |
                                    starts_with("p26120")), na.rm = TRUE),
    potato_daily = potato_total/p20077,
    potato_weekly = potato_daily * 7,
    # eggs
    egg_total = rowSums(select(., starts_with("p26088")), na.rm = TRUE),
    egg_daily = egg_total/p20077,
    egg_weekly = egg_daily * 7,
    # meat substitutes
    meat_sub_total = rowSums(select(., starts_with("p26145")), na.rm = TRUE),
    meat_sub_daily = meat_sub_total/p20077,
    meat_sub_weekly = meat_sub_daily * 7,
    # non-alcoholic beverages
    non_alc_beverage_total = rowSums(select(., starts_with("p26124") | starts_with("p26141") |
                                              starts_with("p26142") | starts_with("p26148") |
                                              starts_with("p26081") | starts_with("p26082") |
                                              starts_with("p26095") | starts_with("p26126") |
                                              starts_with("p26127")), na.rm = TRUE),
    non_alc_beverage_daily = non_alc_beverage_total/p20077,
    non_alc_beverage_weekly = non_alc_beverage_daily * 7,
    # alcoholic beverages
    alc_beverage_total = rowSums(select(., starts_with("p26151") | starts_with("p26152") |
                                          starts_with("p26153") | starts_with("p26067") |
                                          starts_with("p26138")), na.rm = TRUE),
    alc_beverage_daily = alc_beverage_total/p20077,
    alc_beverage_weekly = alc_beverage_daily * 7,
    # sugar, preserves, cakes & confectionery, snacks
    snack_total = rowSums(select(., starts_with("p26106") | starts_with("p26140") |
                                   starts_with("p26134") | starts_with("p26084") |
                                   starts_with("p26085") | starts_with("p26064") |
                                   starts_with("p26080")), na.rm = TRUE),
    snack_daily = snack_total/p20077,
    snack_weekly = snack_daily * 7,
    # Sauces & condiments
    sauce_total = rowSums(select(., starts_with("p26129") | starts_with("p26130")), na.rm = TRUE),
    sauce_daily = sauce_total/p20077,
    sauce_weekly = sauce_daily * 7,
    # legumes
    legume_total = rowSums(select(., starts_with("p26086") | starts_with("p26101") |
                                    starts_with("p26136") | starts_with("p26137")), na.rm = TRUE),
    legume_daily = legume_total/p20077,
    legume_weekly = legume_daily * 7,
    # red meats
    red_meat_total = rowSums(select(., starts_with("p26066") | starts_with("p26100") |
                                      starts_with("p26104") | starts_with("p26117")), na.rm = TRUE),
    red_meat_daily = red_meat_total/p20077,
    red_meat_weekly = red_meat_daily * 7,
    # processed meat
    proc_meat_total = rowSums(select(., starts_with("p26122")), na.rm = TRUE),
    proc_meat_daily = proc_meat_total/p20077,
    proc_meat_weekly = proc_meat_daily * 7,
    # red and processed
    meats_daily = proc_meat_daily + red_meat_daily,
    meats_weekly = proc_meat_weekly + red_meat_weekly,
    # poultry
    poultry_total = rowSums(select(., starts_with("p26121") | starts_with("p26069")), na.rm = TRUE),
    poultry_daily = poultry_total/p20077,
    poultry_weekly = poultry_daily * 7,
    # fish
    fish_total = rowSums(select(., starts_with("p26070") | starts_with("p26109") |
                                  starts_with("p26132") | starts_with("p26149")), na.rm = TRUE),
    fish_daily = fish_total/p20077,
    fish_weekly = fish_daily * 7,
    # total weights of foods
    weight_daily = cereal_refined_daily + whole_grain_daily + mixed_dish_daily +
        dairy_daily + fats_daily + fruit_daily + nut_daily + veggie_daily + potato_daily +
        egg_daily + meat_sub_daily + non_alc_beverage_daily + alc_beverage_daily +
        snack_daily + sauce_daily + legume_daily + meats_daily + poultry_daily + fish_daily,
    weight_weekly = cereal_refined_weekly + whole_grain_weekly + mixed_dish_weekly +
        dairy_weekly + fats_weekly + fruit_weekly + nut_weekly + veggie_weekly + potato_weekly +
        egg_weekly + meat_sub_weekly + non_alc_beverage_weekly + alc_beverage_weekly +
        snack_weekly + sauce_weekly + legume_weekly + meats_weekly + poultry_weekly + fish_weekly,
    #for secondary analysis
    # legumes
    legume_pea_total = rowSums(select(., starts_with("p26086") | starts_with("p26101") |
                                        starts_with("p26136") | starts_with("p26137") |
                                        "peas"), na.rm = TRUE), #1 serving = 80g
    legume_pea_daily = legume_pea_total/p20077,
    legume_pea_weekly = legume_pea_daily * 7,
    # vegetables
    veggie_pea_total = rowSums(select(., starts_with("p26065") | starts_with("p26098") |
                                starts_with("p26147") | starts_with("p26123") |
                                starts_with("p26125") | starts_with("p26143") |
                                starts_with("p26146")), na.rm = TRUE)-peas,
    veggie_pea_daily = veggie_pea_total/p20077,
    veggie_pea_weekly = veggie_pea_daily * 7
    )



# Drop p-variables for diet ------------------------------------------------------
remove_diet <- c("p26113", "p26079", "p26071","p26072", "p26073", "p26075",
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
                 "p26070", "p26109", "p26132", "p26149")

data <- data %>%
  select(-matches(remove_diet))


# # Save data ---------------------------------------------------------------
# arrow::write_parquet(data, here("data/data.parquet"))
# ukbAid::upload_data(here("data/data.parquet"), username = "FieLangmann")
