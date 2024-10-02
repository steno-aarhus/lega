# lega: Substituting meat, poultry, and fish with legumes and risk of gallbladder diseases in a large prospective cohort

-   Protocol:
    [![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.13882532.svg)](https://doi.org/10.5281/zenodo.13882532)

Western diets high in animal foods and saturated fats has been shown to
cause a multitude of non-communicable diseases while also having great
and negative impacts on the environment. Based on a combined
environmental and health related focus, legumes are increasingly being
recommended as a meat substitute. Previous research has however
indicated an increased risk of developing gallbladder related diseases
when consuming large amounts of legumes and this study therefore
investigates the association between substituting legumes for meats,
poultry, and fish and the risk of developing gallbladder diseases.

# Installing and setting up the project

If dependencies have been managed by using
`usethis::use_package("packagename")` through the `DESCRIPTION` file,
installing dependencies is as easy as opening the `.Rproj` file and
running this command in the console:

``` r
pak::pak()
```

# Steps to select and download the data

The `data-raw/` folder contains the scripts to select, process, and
prepare the data on the RAP to eventually be downloaded.

The steps to take to select the variables you want, create the CSV file
on the RAP, convert it to Parquet format (for faster loading), and
download to your project on RAP. The order is:

1.  Select the variables you want in `data-raw/project-variables.csv`.

2.  Follow the instructions in the `data-raw/create-data.R` script and
    run it to create the CSV file on the RAP server.

3.  Run `targets::tar_make()` to download the CSV file to `data/`.

# Processing and saving progress

It is very timely to rerun all code every time you have made changes in
the data, so you can save and reload your work along the way by
following the layout in `data-raw/processing.R` script. This is also
helpful to do, when you have completed all data management tasks and
want to save your changes to have a "ready to go" data frame to run your
analysis on.

Once you've created your dataset using `data-raw/create-data.R`, you can
uncomment the lines in the `_targets.R` file and afterwards run this
code whenever you enter the RAP project.

``` r
targets::tar_make()
```

# Brief description of folder and file contents

The following folders contain:

-   `data/`: Will contain the UK Biobank data (not saved to Git) as well
    as the intermediate results output files.

-   `data-raw/`: Contains the R script to download the data, as well as
    the CSV files that contain the project variables and the variable
    list as named in the RAP.

-   `doc/`: This file contains the R Markdown, Word, or other types of
    documents with written content, like the manuscript and protocol.

-   `R/`: Contains the R scripts and functions to create the figures,
    tables, and results for the project.

