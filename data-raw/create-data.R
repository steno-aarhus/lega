# Keep only the necessary variables for RAP -------------------------------

library(magrittr)

# After the variables have been properly selected in the `data-raw/project-variables.csv`
# file, run this function so that only the selected variables are kept in the
# `data-raw/rap-variables.csv` file. This file has the exact variable names used
# by RAP that we need in order to create the project-specific dataset. After
# running this function, review the changes in Git and add and commit the changed
# files into the history.

# Uncomment if you messed up and need to start over.
# ukbAid::rap_variables %>%
#     readr::write_csv(here::here("data-raw/rap-variables.csv"))

# Create the project dataset and save inside RAP --------------------------

# Uncomment and run the below lines **ONLY AFTER** running the above function.
# After running this code and creating the csv file in the main RAP project
# folder, comment it out again so you don't accidentally run it anymore (unless
# you need to re-create the dataset).

dataset_filename <- ukbAid::proj_create_path_dataset()
# Create the dataset csv file.
readr::read_csv(here::here("data-raw/rap-variables.csv")) %>%
    dplyr::pull(id) %>%
    ukbAid::proj_create_dataset(
      output_path = dataset_filename
    )

# Move the created dataset over.
ukbAid::rap_move_file(
  dataset_filename,
  ukbAid::rap_get_path_users() |>
  stringr::str_subset(ukbAid::rap_get_user())
)


