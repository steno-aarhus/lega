# Load packages required to define the pipeline:
library(targets)
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
source(here::here("R/model_control.R"))
source(here::here("R/analyses.R"))

# Things to run in order to work.
list(
    # TODO: Uncomment this *after* finishing running `data-raw/create-data.R`
    tar_target(
        name = download_project_data,
    # TODO: This will eventually need to be changed to "parquet".
        command = ukbAid::download_data(file_ext = "parquet"),
        format = "file"
    ),
    # load data
    tar_target(
        name = unsorted_data,
        command = readr::read_csv(download_data)
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


# descriptive analyses ----------------------------------------------------
    tar_target(
        name = events,
        command = sorted_data |>
            number_events()
    ),
)
