# Tidying Functions -------------------------------------------------------

# Function that uses unpivotr and regex to parse out run metadata in long format
tidy_metadata <- function(metadata_cells, settings_cells, filename) {
    # Clean up settings cells partition using unpivotr
    setting_df <- settings_cells %>%
        filter(
            # Remove partition header
            !str_detect(.data$value, "settings")
        ) %>%
        unpivotr::behead("left", "setting", "value") %>%
        select(.data$setting, .data$value) %>%
        # Pivot wider to switch setting row values to columns
        tidyr::pivot_wider(
            names_from = .data$setting,
            values_from = .data$value
        ) %>%
        # Clean up column names to lowercase, no spaces, with colons, brackets,
        # and dots removed
        rename_with(
            ~ tolower(.) %>%
                str_replace_all(" ", "_") %>%
                str_replace_all(r"([:\[\]\(\)\.])", "")
        )
    # Pull test_run_no with regex
    test_run_no <- metadata_cells %>%
        filter(str_detect(.data$value, "Test run no.:")) %>%
        pull(.data$value) %>%
        str_extract(r"(\d+)")
    # Pull date and time strings
    date_str <- metadata_cells %>%
        filter(str_detect(.data$value, "Date:")) %>%
        pull(.data$value)

    time_str <- metadata_cells %>%
        filter(str_detect(.data$value, "Time:")) %>%
        pull(.data$value)
    # Use lubridate to get datetime
    datetime <- lubridate::dmy_hms(
        paste(date_str, time_str),
        tz = "Australia/Melbourne"
    )

    metadata_str <- metadata_cells %>%
        filter(str_detect(.data$value, "ID1:")) %>%
        pull(.data$value)

    re_pattern <- r"(ID1: (?<sample>\w+) (?<media>\w+) (?<conc>\d+)nM P(?<plate>\d)R(?<rep>\d+))"
    # Extract run sample, media, concentration, plate number, and replicate
    # Using regex
    metadata_match <- metadata_str %>%
        str_match(re_pattern) %>%
        tibble::as_tibble(.name_repair = "minimal") %>%
        select(-1)
    # As storing run information in a filename or id name is brittle, try to
    # catch incorrect ID name patterns and throw error
    tryCatch(
        {
            # If any of the regex groups are NA, throw error
            test <- metadata_match %>% filter(if_any(everything(), is.na))
            stopifnot(nrow(test) == 0)
        },
        error = function(c) {
            msg <- glue::glue(
                "The following ID string does not match the regex pattern:

                    str: {metadata_str}
                    regex: {re_pattern}

                    Correct the ID string WITHIN the excel file, \\
                    NOT the filename, and rerun the script.
                    "
            )
            print(test)
            stop(msg)
        }
    )
    # Add datetime, run_no, and filename to growing metadata row
    metadata_df <- metadata_match %>%
        tibble::add_column(datetime, test_run_no, filename) %>%
        rename(conc_nm = .data$conc)
    # Bind both settings and metadata tables, converting string columns to
    # numeric
    metadata_df <- bind_cols(metadata_df, setting_df) %>%
        mutate(
            across(
                c(
                    .data$conc_nm,
                    .data$plate,
                    .data$rep,
                    .data$test_run_no,
                    .data$no_of_flashes_per_well,
                    .data$gain,
                    .data$focal_height_mm
                ),
                as.numeric
            )
        ) %>%
        relocate(
            .data$filename,
            .data$test_run_no,
            .data$datetime,
            .data$sample,
            .data$rep,
            .data$plate,
            .data$conc_nm
        ) %>%
        # Drop irrelevant information
        select(-c(.data$measurement_type, .data$microplate_name))

    return(metadata_df)
}

# Function that tidies experimental data into long format.
tidy_data <- function(data_cells, metadata_df) {
    # Use unpivotr to behead columns
    data_df <- data_cells %>%
        filter(!(.data$value %in% c("Well", "Content", "Raw Data (545-10/590-20)"))) %>%
        unpivotr::behead("left", "well", "value") %>%
        # [TODO] Possibly unnecessary to take content column as in contains no useful
        # information? It's later dropped
        unpivotr::behead("left", "content", "value") %>%
        select(.data$value, .data$well) %>%
        # Separate row and column information as it is more useful in separated
        # form
        tidyr::separate_wider_regex(
            cols = .data$well,
            patterns = c(
                plate_row = r"([A-Z]+)",
                plate_col = r"(\d+)"
            )
        ) %>%
        # Convert to numeric as some cols are numbered '01-09'
        mutate(plate_col = as.numeric(.data$plate_col)) %>%
        # Rename to indicate values are fluorescence
        rename(fluor = .data$value)
    # Bind some metadata to data table for usability, but not necessary in final
    # SQL database (just join with metadata)
    data_df <- bind_cols(
        metadata_df %>% select(.data$test_run_no:.data$conc_nm),
        data_df
    ) %>%
        # Keep location information together
        relocate(
            .data$plate_row,
            .data$plate_col,
            .after = .data$plate
        )

    return(data_df)
}
