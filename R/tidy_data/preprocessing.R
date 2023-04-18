# Pre-processing Functions ------------------------------------------------

# Splits spreadsheet cells into partitions based on corner sentinel values
# This is done rather than relying on line numbers as CRLF breaks double file
# length (possible bug?) and so may result in different values in linux/windows
# systems
split_cells <- function(cells) {
    # Get cells that match corners
    corners <- cells %>%
        filter(
            col == 1,
            # General settings here to separate it from basic settings
            # As general settings are not meaningful, hence only later using
            # the first 3 partitions.
            str_detect(value, r"(User|Raw Data|Well$|Basic settings|General settings)")
        )
    # Partition into list of matching cells where corner is top-left
    partitions <- unpivotr::partition(cells, corners)
    # Metadata cells top of the sheet, marked by "User"
    metadata_cells <- partitions %>%
        filter(value == "User: USER") %>%
        pull(cells) %>%
        purrr::pluck(1)
    # Long format data cells marked by 1st column name "Well"
    data_cells <- partitions %>%
        filter(value == "Well") %>%
        pull(cells) %>%
        purrr::pluck(1)
    # Settings cells marked by "Basic settings"
    settings_cells <- partitions %>%
        filter(value == "Basic settings") %>%
        pull(cells) %>%
        purrr::pluck(1)
    # Return a list of partitioned cells
    cell_list <- list(
        metadata = metadata_cells,
        data = data_cells,
        settings = settings_cells
    )

    return(cell_list)
}
