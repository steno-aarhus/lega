# Load packages required to define the pipeline:
library(targets)
library(magrittr)
library(ukbAid)
# library(tarchetypes) # Load other packages as needed.

# Tell targets your needed packages.
package_deps <- desc::desc_get_deps()$package |>
  stringr::str_subset("^R$", negate = TRUE)

# Set target options:
tar_option_set(
  packages = ukbAid::proj_get_dependencies(),
  format = "qs", # Optionally set the default storage format. qs is fast.
  #
  # This likely isn't necessary for most UK Biobank users at SDCA/AU.
  # For distributed computing in tar_make(), supply a {crew} controller
  # as discussed at https://books.ropensci.org/targets/crew.html.
  # Choose a controller that suits your needs. For example, the following
  # sets a controller with 2 workers which will run as local R processes:
  #
  #   controller = crew::crew_controller_local(workers = 2)
  #
)

# Run the R scripts in the R/ folder with your custom functions:
# tar_source()
# Or just some files:
# source(here::here("data-raw/download_data.R"))
source(here::here("R/data_wrangling.R"))
source(here::here("R/descriptives.R"))
source(here::here("R/model_assumptions.R"))
source(here::here("R/analyses.R"))
source(here::here("R/extra_analyses.R"))

# Things to run in order to work.
list(
    # download data
    # TODO: Uncomment this *after* finishing running `data-raw/create-data.R`
    tar_target(
        name = project_data,
        # TODO: This will eventually need to be changed to "parquet".
        command = rap_copy_from(
            rap_get_path_user_files(user = rap_get_user()) |>
                sort() |>
                head(1),
            here::here("data/data.csv")
        ),
        format = "file"
    ),
    # load data
    tar_target(
        name = unsorted_data,
        command = readr::read_csv(project_data)
    ),
    # remove those with less than 2 diet recalls
    tar_target(
        name = adequate_recalls,
        command = unsorted_data |>
            two_recalls()
    ),
    # add id
    tar_target(
        name = id_data,
        command = adequate_recalls |>
            data_id()
    ),
    # wrangle covariates (not food)
    tar_target(
        name = covariates,
        command = id_data |>
            sociodemographics() |>
            lifestyle() |>
            alcohol() |>
            alcohol_intake() |>
            illness() |>
            hormones() |>
            pregnancies() |>
            bilirubin() |>
            remove_missings() |>
            remove_p_vars()
    ),
    # wrangle diet data
    tar_target(
        name = diet_data,
        command = covariates |>
            pea_servings() |>
            food_groups() |>
            remove_diet_p_vars()
    ),
    # wrangle outcome variables
    tar_target(
        name = outcome_data,
        command = diet_data |>
            icd10_diagnoses() |>
            icd9_diagnoses() |>
            opcs4_diagnoses() |>
            opcs3_diagnoses() |>
            date_birth() |>
            censoring_date() |>
            outcome_variables()
    ),
    # eligibility criteria based on outcomes
    tar_target(
        name = eligible_participants,
        command = outcome_data |>
            last_completed_recall() |>
            diagnosed_before() |>
            left_study() |>
            remove_outcome_p_vars()
    ),
    # define survival time
    tar_target(
        name = sorted_data,
        command = eligible_participants |>
            survival_time() |>
            define_exposure_variables()
    ),
    # descriptive analyses
    tar_target(
        name = events,
        command = sorted_data |>
            number_events()
    ),
    tar_target(
        name = table1,
        command = sorted_data |>
            baseline_table()
    ),
    tar_target(
    name = suppl_base_table,
    command = sorted_data |>
        supplementary_baseline_table()
    ),
    tar_target(
    name = follow_up_year,
    command = sorted_data |>
        person_years_followup()
    ),
# main analyses -----------------------------------------------------------
tar_target(
    name = main_analyses1,
    command = sorted_data |>
        main_model1()
),
tar_target(
    name = main_analyses2,
    command = sorted_data |>
        main_model2()
),
tar_target(
    name = main_analyses3,
    command = sorted_data |>
        main_model3()
),
tar_target(
  name = model_control,
  command = sorted_data |>
  model_assumption()
),
# # secondary analyses ------------------------------------------------------
tar_target(
    name = consumers_main,
    command = sorted_data |>
        consumers_analyses()
),
tar_target(
    name = consumers_total,
    command = sorted_data |>
        total_intake()
),


# # sensitivity analyses ----------------------------------------------------
tar_target(
    name = legume_pea,
    command = sorted_data |>
        legumes_and_peas()
),
tar_target(
    name = no_soy,
    command = sorted_data |>
        legumes_without_soy()
),
tar_target(
    name = multiple_recalls,
    command = sorted_data |>
        three_recalls_analyses()
),
tar_target(
    name = normal_bilirubin,
    command = sorted_data |>
        bilirubin_analyses()
),


# Extra analyses ----------------------------------------------------------
# gallstone only
tar_target(
    name = gallstone_analyses,
    command = sorted_data |>
        gallstone_model2()
),
# cholecystectomy only
tar_target(
    name = cholecystectomy_analyses,
    command = sorted_data |>
        removal_model2()
),
# cholecystit only
tar_target(
    name = cholecystit_analyses,
    command = sorted_data |>
        cholecystit_model2()
),
tar_target(
    name = men_analyses,
    command = sorted_data |>
        men_strata()
),
tar_target(
    name = women_analyses,
    command = sorted_data |>
        women_strata()
),
tar_target(
    name = age_analyses,
    command = sorted_data |>
        age_strata()
),
tar_target(
    name = bmi_analyses,
    command = sorted_data |>
        bmi_strata()
),
tar_target(
    name = meat_type,
    command = sorted_data |>
        meat_separate()
))



