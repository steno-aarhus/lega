# File-navn in RAP under "users" MUST be: "username-data-project_id-timestamp.csv",
# fx "FieLangmann-data-lega-2024-06-04T07-58-00.csv"
# When that is done, run this:

library(ukbAid)

download_data <- function(project_id = get_rap_project_id(),
                          file_prefix = "data",
                          file_ext = c("csv", "parquet"),
                          username = rap_get_user()) {
    lifecycle::deprecate_soft(
        when = "0.1.0",
        what = "download_data()",
        with = "rap_copy_from()"
    )
    file_ext <- rlang::arg_match(file_ext)
    rap_file <- rap_get_path_user_files(rap_get_user()) |>
        stringr::str_subset(file_ext) |>
        stringr::str_sort(decreasing = TRUE) |>
        head(1)
    rap_path <- glue::glue("/users/{username}/{rap_file}")

    cli::cli_alert_info("Downloading from RAP: {.val {rap_path}}.")
    output_path <- glue::glue("data/data.{file_ext}")
    cli::cli_alert_info("Downloading data to {.val {output_path}}.")
    rap_copy_from(rap_path, output_path)
    cli::cli_alert_success("Downloaded the data file!")
    return(output_path)
}

download_data()
