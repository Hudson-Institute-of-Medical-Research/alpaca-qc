# Main Functions ----------------------------------------------------------

# Function that loops through csv paths, tidies experimental data and metadata,
# adds drug information to data, and aggregates all data in a list of
# experimental data and metadata
agg_data <- function(paths, drug_key_df) {
    metadata_list <- list()
    data_list <- list()
    file_no <- length(paths)

    # Loop through path indexes (needed for message)
    for (i in 1:file_no) {
        path <- paths[[i]]
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
    # Return as list
    combined_data <- list(
        data = agg_data_df,
        metadata = agg_metadata_df
    )

    return(combined_data)
}
