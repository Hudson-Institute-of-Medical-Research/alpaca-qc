<!--
Document variables:

Assigned:
<author> = Vikesh Ajith
<github_username> = SpikyClip

Unassigned (replace all with desired value):
<alpaca-qc>
<screenshot_link>

-->

# `alpaca-qc`

[![GitHub Stars](https://img.shields.io/github/stars/Hudson-Institute-of-Medical-Research/alpaca-qc.svg)](https://github.com/Hudson-Institute-of-Medical-Research/alpaca-qc/stargazers) [![GitHub Issues](https://img.shields.io/github/issues/Hudson-Institute-of-Medical-Research/alpaca-qc.svg)](https://github.com/Hudson-Institute-of-Medical-Research/alpaca-qc/issues) [![Tag](https://img.shields.io/github/v/tag/Hudson-Institute-of-Medical-Research/alpaca-qc)](https://github.com/Hudson-Institute-of-Medical-Research/alpaca-qc) [![License](https://img.shields.io/github/license/Hudson-Institute-of-Medical-Research/alpaca-qc)](https://github.com/Hudson-Institute-of-Medical-Research/alpaca-qc/blob/master/LICENSE)

<!-- badges: start -->
[![R-CMD-check](https://github.com/Hudson-Institute-of-Medical-Research/alpaca-qc/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/Hudson-Institute-of-Medical-Research/alpaca-qc/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

A package for tidying plate reader data and adding drug annotation information
from ALPACA (Australian Library of Paediatric Anti-Cancer Agents) drug
libraries.

## Introduction

Plate readers such as the Clariostar produce a `.CSV` file for each plate which
contains well data and metadata. This data is not in tidy format, and needs to
be parsed and tidied. Well locations also need to be annotated with drug
information for downstream analysis.

`alpacaqc` is an R package designed to assist with the parsing of Clariostar
results into a tidied long format, as well as the annotation of drug
information to the results.

## Features

- Collects both experimental data and metadata from multiple Clariostar
  `.CSV` files, returning as a list of `tidy_data` and `tidy_metadata`
  tibbles in long-format.
- Imports a `drug_key.csv` provided by the user as a tibble which corresponds
  drug information to well location. The drug key is validated in several ways:
  - Checks that wells are unique, avoiding id collision
  - Check that plate row and column values fall within the range of a 384 well
  plate [`rows A - P`, `cols 1 - 24`]
  - Ensure that all fields are complete `[plate, plate_row, plate_col,
    drug_name]`
    Ensure that `cas_no` field is complete for all drugs (Except `PBS/PURO`)

- Annotates `tidy_data` with drug information from `drug_key` based on well
  position.

## Quick Start

1. Install `devtools` and `alpacaqc`:

    ```R
    install.packages("devtools")
    devtools::install_github("Hudson-Institute-of-Medical-Research/alpaca-qc")
    ```

2. Gather and tidy data:

    ```R
    # Get experimental data csv using glob pattern
    result_paths <- Sys.glob("data/sample/*.CSV")
    # Get and validate drug key
    drug_key_df <- import_drug_key("data/config/drug_well_key.csv")

    # Tidy data
    batch_data <- gather_plates(result_paths)
    ```

3. Annotate data with drug metadata and save:

    ```R
    # Annotate
    annot_data <- add_drug_annot(batch_data$tidy_data, drug_key_df)
    # Save
    readr::write_csv(annot_data, "tidy_data.csv")
    readr::write_csv(batch_data$tidy_metadata, "tidy_metadata.csv")
    ```

## Usage

### Drug Key Design

1. Design a drug_key `.csv` with the following field information:
    1. **`[plate]`:** An `integer` corresponding to a particular drug layout on a
       plate. This distinguishes different drugs that are in the same well
       position, but in different plate layouts.
    2. **`[plate_row]`:** A `character` letter `[A-P]` indicating a drug's row
       position on a 384 well plate.
    3. **`[plate_col]`:** A `integer` `[1-24]` indicating a drug's column position
       on a 384 well plate.
    4. **`[catalog_no]`** (*optional*): A `character` string indicating a drugs
       catalog number.
    5. **`[cas_no]`:** A `character` string of the form `XXXXXX-XX-X` indicating a
       drug's [CAS Registry Number](https://www.cas.org/cas-data/cas-registry).
       Mandatory as it is used downstream for database purposes.

    | plate | plate_row | plate_col | catalog_no | cas_no      | drug_name |
    | ----- | --------- | --------- | ---------- | ----------- | --------- |
    | 1     | A         | 1         | PURO       | NA          | PURO      |
    | 1     | A         | 2         | PBS        | NA          | PBS       |
    | 1     | A         | 3         | XXXXX      | XXXXXX-XX-1 | drug_a    |
    | 1     | A         | 4         | XXXXX      | XXXXXX-XX-2 | drug_b    |
    | 1     | A         | 5         | XXXXX      | XXXXXX-XX-3 | drug_c    |
    | ...   | ...       | ...       | ...        | ...         | ...       |

### ID Names

2. Read plates using the Clariostar. Export the data with the following default
   ID string format:

    ```
    <sample_name> <media> <concentration>nM P<plate>R<rep>
    ```

    This format is important as metadata is parsed from the ID string using the
    following default `regex` expression:

    ```R
    r"(ID1: (?<sample>\w+) (?<media>\w+) (?<conc>\d+)nM P(?<plate>\d)R(?<rep>\d+))"
    ```

    Alternatively, a custom
    [regex](https://cran.r-project.org/web/packages/stringr/vignettes/regular-expressions.html)
    pattern can be supplied using the `id_regex` argument to `gather_plates()`:

    ```R
    batch_data <- gather_plates(
        result_paths,
        id_regex = r"(ID1: (?<sample>\w+) (?<media>\w+) (?<conc>\d+)nM P(?<plate>\d)R(?<rep>\d+))"
    )
    ```
    Ensure that the custom regex pattern has capturing groups for `[sample,
    media, conc, plate, rep]`

### Tidy Data

3. Load `alpacaqc` in `R` and tidy the data:

    ```R
    library(alpacaqc)

    # Get experimental data csv using glob pattern
    result_paths <- Sys.glob("data/sample/*.CSV")
    # Get and validate drug key
    drug_key_df <- import_drug_key("data/config/drug_well_key.csv")

    # Tidy data
    batch_data <- gather_plates(result_paths)
    ```

    The `batch_data` variable is now a list with the contents
    `batch_data$tidy_data` and `batch_data$tidy_metadata`. The data is now in
    long-format:

    **`batch_data$tidy_data`**
    | test_run_no | datetime             | sample      | rep | plate | plate_row | plate_col | conc_nm | fluor |
    | ----------- | -------------------- | ----------- | --- | ----- | --------- | --------- | ------- | ----- |
    | 19344       | 2023-04-06T03:24:54Z | sample_name | 1   | 1     | A         | 1         | 10      | 5181  |
    | ...         | ...                  | ...         | ... | ...   | ...       | ...       | ...     | ...   |

    **`batch_data$tidy_metadata`**

    | filename     | test_run_no | datetime             | sample      | rep | plate | conc_nm | media | no_of_flashes_per_well | presetname  | excitation | dichroic_filter | emission | gain | wells_used_for_gain_adjustment | focal_height_mm |
    | ------------ | ----------- | -------------------- | ----------- | --- | ----- | ------- | ----- | ---------------------- | ----------- | ---------- | --------------- | -------- | ---- | ------------------------------ | --------------- |
    | filename.CSV | 19344       | 2023-04-06T03:24:54Z | sample_name | 1   | 1     | 10      | DMSO  | 5                      | Alamar Blue | 545-10     | auto 565        | 590-20   | 1130 | C3                             | 3.9             |
    | ...          | ...         | ...                  | ...         | ... | ...   | ...     | ...   | ...                    | ...         | ...        | ...             | ...      | ...  | ...                            | ...             |

### Annotate Data

4. Annotate `batch_data$tidy_data` with drug information from `drug_key_df`

    ```R
    annot_data <- add_drug_annot(batch_data$tidy_data, drug_key_df)
    ```

    `annot_data` now has the following structure:

    | test_run_no | datetime             | sample      | rep | plate | plate_row | plate_col | catalog_no | cas_no | drug_name | conc_nm | fluor |
    | ----------- | -------------------- | ----------- | --- | ----- | --------- | --------- | ---------- | ------ | --------- | ------- | ----- |
    | 19344       | 2023-04-06T03:24:54Z | sample_name | 1   | 1     | A         | 1         | XXXXX      | NA     | drug_a    | 10      | 5181  |
    | ...         | ...                  | ...         | ... | ...   | ...       | ...       | ...        | ...    | ...       | ...     | ...   |

5. Save data for downstream use.

    ```R
    readr::write_csv(annot_data, "tidy_data.csv")
    readr::write_csv(batch_data$tidy_metadata, "tidy_metadata.csv")
    ```

## Additional Information

### Versioning

`dev-0.0.1`

### License

This project is licensed under the terms of the [MIT license](https://github.com/Hudson-Institute-of-Medical-Research/alpaca-qc/blob/master/LICENSE).
