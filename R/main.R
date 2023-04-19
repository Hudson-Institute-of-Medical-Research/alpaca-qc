# Main Functions ----------------------------------------------------------

#' Gathers plate reader csv data into tidied long-format.
#'
#' loops through csv paths, extracts experimental data, and extracts metadata
#' using regex.
#'
#' @param result_paths string vector of paths to csv files output by plate
#'   readers in a standard human-readable format.
#'
#' @return A list with $tidy_data and $tidy_metadata tibbles.
#' @export
#'
#' @examples
#' result_paths <- Sys.glob("data/raw_id_corrected/*.CSV")
#' test <- gather_plates(result_paths)
gather_plates <- function(result_paths) {
    metadata_list <- list()
    data_list <- list()
    file_no <- length(result_paths)

    # Loop through path indexes (needed for message)
    for (i in 1:file_no) {
        path <- result_paths[[i]]
        filename <- basename(path)

        message(glue("Tidying '{filename}'\t[{i}/{file_no}]"))
        # Get cells and partition them
        cells <- meltr::melt_csv(path) %>%
            filter(data_type != "missing")
        partitions <- split_cells(cells)
        # Get tidied metadata
        metadata_df <- tidy_metadata(
            partitions$metadata,
            partitions$settings,
            filename
        )
        # Get tidied experimental data
        data_df <- tidy_data(partitions$data, metadata_df)
        # Append data to list
        metadata_list <- append(metadata_list, list(metadata_df))
        data_list <- append(data_list, list(data_df))
    }
    # Bind data together
    tidy_metadata_df <- bind_rows(metadata_list) %>%
        arrange(test_run_no)

    tidy_data_df <- bind_rows(data_list) %>%
        arrange(
            test_run_no,
            sample,
            rep,
            plate,
            plate_row,
            plate_col
        )
    # Return as list
    batch_data <- list(
        tidy_data = tidy_data_df,
        tidy_metadata = tidy_metadata_df
    )

    return(batch_data)
}

#' Annotates tidy plate data to drug metadata.
#'
#' Tidy plate data produced by `gather_plates()` is joined to drug_key metadata
#' based on the combination of 'plate', `plate_row`, and `plate_col`.
#' @param tidy_data_df Tidied and gathered long-format data tibble produced by
#'   `gather_plates()`
#' @param drug_key_df A tibble key of corresponding (`catalog_no`, `cas_no`,
#'   `drug_name`) to (`plate`, `plate_row`, `plate_col`). All values are
#'   mandatory except for `catalog_no` as the rest of the information is needed
#'   for joining or used as downstream metadata.
#'
#' @return A tibble
#' @export
#'
#' @examples
#' # Get inputs
#' result_paths <- Sys.glob("data/raw_id_corrected/*.CSV")
#' drug_key_df <- readr::read_csv("data/config/drug_well_key_complete.csv")
#'
#' # Process data
#' batch_data <- gather_plates(result_paths)
#' annot_data <- add_drug_annot(batch_data$tidy_data, drug_key_df)
add_drug_annot <- function(tidy_data_df, drug_key_df) {
    annot_data_df <- tidy_data_df %>%
        left_join(
            drug_key_df,
            # A unique combination of plate, plate_row, and plate_col determines
            # Which drug is in which well.
            by = c("plate", "plate_row", "plate_col")
        ) %>%
        relocate(catalog_no:drug_name, .after = plate_col)

    return(annot_data_df)
}
