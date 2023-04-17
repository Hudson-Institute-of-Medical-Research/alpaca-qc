split_cells <- function(cells) {
    corners <- cells %>%
        filter(
            col == 1,
            str_detect(value, r"(User|Raw Data|Well$|Basic settings|General settings)")
        )

    partitions <- partition(cells, corners)

    metadata_cells <- partitions %>%
        filter(value == "User: USER") %>%
        pull(cells) %>%
        pluck(1)

    data_cells <- partitions %>%
        filter(value == "Well") %>%
        pull(cells) %>%
        pluck(1)

    settings_cells <- partitions %>%
        filter(value == "Basic settings") %>%
        pull(cells) %>%
        pluck(1)

    cell_list <- list(
        metadata = metadata_cells,
        data = data_cells,
        settings = settings_cells
    )
    return(cell_list)
}

tidy_data <- function(data_cells, metadata_df) {
    data_df <- data_cells %>%
        filter(!(value %in% c("Well", "Content", "Raw Data (545-10/590-20)"))) %>%
        behead("left", well, value) %>%
        behead("left", content, value) %>%
        select(value, well) %>%
        separate_wider_regex(
            cols = well,
            patterns = c(
                plate_row = r"([A-Z]+)",
                plate_col = r"(\d+)"
            )
        ) %>%
        mutate(plate_col = as.numeric(plate_col)) %>%
        rename(fluor = value)

    data_df <- bind_cols(
        metadata_df %>% select(test_run_no:conc_nm),
        data_df
    ) %>%
        relocate(
            plate_row,
            plate_col,
            .after = plate
        )

    return(data_df)
}

tidy_metadata <- function(metadata_cells, settings_cells, filename) {
    setting_df <- settings_cells %>%
        filter(
            !str_detect(value, "settings")
        ) %>%
        behead("left", setting, value) %>%
        select(setting, value) %>%
        pivot_wider(
            names_from = setting,
            values_from = value
        ) %>%
        rename_with(
            ~ tolower(.) %>%
                str_replace_all(" ", "_") %>%
                str_replace_all(r"([:\[\]\(\)\.])", "")
        )

    test_run_no <- metadata_cells %>%
        filter(str_detect(value, "Test run no.:")) %>%
        pull(value) %>%
        str_extract(r"(\d+)")

    date_str <- metadata_cells %>%
        filter(str_detect(value, "Date:")) %>%
        pull(value)

    time_str <- metadata_cells %>%
        filter(str_detect(value, "Time:")) %>%
        pull(value)

    datetime <- dmy_hms(
        paste(date_str, time_str),
        tz = "Australia/Melbourne"
    )

    metadata_str <- metadata_cells %>%
        filter(str_detect(value, "ID1:")) %>%
        pull(value)

    re_pattern <- r"(ID1: (?<sample>\w+) (?<media>\w+) (?<conc>\d+)nM P(?<plate>\d)R(?<rep>\d+))"

    metadata_match <- metadata_str %>%
        str_match(re_pattern) %>%
        as_tibble(.name_repair = "minimal") %>%
        select(-1)

    tryCatch(
        {
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

    metadata_df <- metadata_match %>%
        tibble::add_column(datetime, test_run_no, filename) %>%
        rename(conc_nm = conc)

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
        select(-c(measurement_type, microplate_name))

    return(metadata_df)
}

add_drug_info <- function(agg_data_df, drug_key_df) {
    complete_data_df <- agg_data_df %>%
        left_join(
            drug_key_df,
            by = c("plate", "plate_row", "plate_col")
        ) %>%
        relocate(catalog_no:drug_name, .after = plate_col)

    return(complete_data_df)
}

agg_data <- function(paths, drug_key_df) {
    metadata_list <- list()
    data_list <- list()
    file_no <- length(paths)

    for (i in 1:file_no) {
        path <- paths[[i]]

        filename <- basename(path)

        message(glue("Tidying '{filename}' [{i}/{file_no}]"))

        cells <- meltr::melt_csv(path) %>%
            filter(data_type != "missing")
        partitions <- split_cells(cells)

        metadata_df <- tidy_metadata(
            partitions$metadata,
            partitions$settings,
            filename
        )
        data_df <- tidy_data(partitions$data, metadata_df)

        metadata_list <- append(metadata_list, list(metadata_df))
        data_list <- append(data_list, list(data_df))
    }

    agg_metadata_df <- bind_rows(metadata_list) %>%
        arrange(test_run_no)

    agg_data_df <- bind_rows(data_list) %>%
        add_drug_info(drug_key_df) %>%
        arrange(
            test_run_no,
            sample,
            rep,
            plate,
            plate_row,
            plate_col
        )

    combined_data <- list(
        data = agg_data_df,
        metadata = agg_metadata_df
    )

    return(combined_data)
}
