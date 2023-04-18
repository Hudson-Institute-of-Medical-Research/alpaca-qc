# Tidying Functions -------------------------------------------------------

# Function that uses unpivotr and regex to parse out run metadata in long format
tidy_metadata <- function(metadata_cells, settings_cells, filename) {
    # Clean up settings cells partition using unpivotr
    setting_df <- settings_cells %>%
        filter(
            # Remove partition header
            !str_detect(value, "settings")
        ) %>%
        unpivotr::behead("left", setting, value) %>%
        select(setting, value) %>%
        # Pivot wider to switch setting row values to columns
        tidyr::pivot_wider(
            names_from = setting,
            values_from = value
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
        filter(str_detect(value, "Test run no.:")) %>%
        pull(value) %>%
        str_extract(r"(\d+)")
    # Pull date and time strings
    date_str <- metadata_cells %>%
        filter(str_detect(value, "Date:")) %>%
        pull(value)

    time_str <- metadata_cells %>%
        filter(str_detect(value, "Time:")) %>%
        pull(value)
    # Use lubridate to get datetime
    datetime <- lubridate::dmy_hms(
        paste(date_str, time_str),
        tz = "Australia/Melbourne"
    )

    metadata_str <- metadata_cells %>%
        filter(str_detect(value, "ID1:")) %>%
        pull(value)

    re_pattern <- r"(ID1: (?<sample>\w+) (?<media>\w+) (?<conc>\d+)nM P(?<plate>\d)R(?<rep>\d+))"
    # Extract run sample, media, concentration, plate number, and replicate
    # Using regex
    metadata_match <- metadata_str %>%
        str_match(re_pattern) %>%
        as_tibble(.name_repair = "minimal") %>%
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
            msg <- glue(
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
        rename(conc_nm = conc)
    # Bind both settings and metadata tables, converting string columns to
    # numeric
    metadata_df <- bind_cols(metadata_df, setting_df) %>%
        mutate(
            across(
                c(
                    conc_nm,
                    plate,
                    rep,
                    test_run_no,
                    no_of_flashes_per_well,
                    gain,
                    focal_height_mm
                ),
                as.numeric
            )
        ) %>%
        relocate(
            filename,
            test_run_no,
            datetime,
            sample,
            rep,
            plate,
            conc_nm
        ) %>%
        # Drop irrelevant information
        select(-c(measurement_type, microplate_name))

    return(metadata_df)
}

# Function that tidies experimental data into long format.
tidy_data <- function(data_cells, metadata_df) {
    # Use unpivotr to behead columns
    data_df <- data_cells %>%
        filter(!(value %in% c("Well", "Content", "Raw Data (545-10/590-20)"))) %>%
        unpivotr::behead("left", well, value) %>%
        # [TODO] Possibly unnecessary to take content column as in contains no useful
        # information? It's later dropped
        unpivotr::behead("left", content, value) %>%
        select(value, well) %>%
        # Separate row and column information as it is more useful in separated
        # form
        tidyr::separate_wider_regex(
            cols = well,
            patterns = c(
                plate_row = r"([A-Z]+)",
                plate_col = r"(\d+)"
            )
        ) %>%
        # Convert to numeric as some cols are numbered '01-09'
        mutate(plate_col = as.numeric(plate_col)) %>%
        # Rename to indicate values are fluorescence
        rename(fluor = value)
    # Bind some metadata to data table for usability, but not necessary in final
    # SQL database (just join with metadata)
    data_df <- bind_cols(
        metadata_df %>% select(test_run_no:conc_nm),
        data_df
    ) %>%
        # Keep location information together
        relocate(
            plate_row,
            plate_col,
            .after = plate
        )

    return(data_df)
}
