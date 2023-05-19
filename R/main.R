# Main Functions ----------------------------------------------------------

theme_set(theme_minimal())

# Tidying Functions -------------------------------------------------------

#' Gathers plate reader csv data into tidied long-format.
#'
#' loops through csv paths, extracts experimental data, and extracts metadata
#' using regex.
#'
#' @param result_paths string vector of paths to csv files output by plate
#'   readers in a standard human-readable format.
#'
#' @param id_regex regular expression that parses `sample`, `treatment`, `conc_nm`
#'
#' @return A list with $tidy_data and $tidy_metadata tibbles.
#' @export
#'
#' @examples
#' \dontrun{
#' result_paths <- Sys.glob("data/raw_id_corrected/*.CSV")
#' batch <- gather_plates(result_paths)
#' }
gather_plates <- function(result_paths,
                          id_regex = r"(ID1: (?<sample>\w+) (?<treatment>\w+) (?<conc_nm>\d+)nM P(?<plate>\d)R(?<rep>\d+))") {
    # Check if paths are valid csv's before processing data.
    validate_csv_paths(result_paths)

    metadata_list <- list()
    data_list <- list()
    file_no <- length(result_paths)

    # Loop through path indexes (needed for message)
    for (i in 1:file_no) {
        path <- result_paths[[i]]
        filename <- basename(path)

        message(glue::glue("Tidying '{filename}'\t[{i}/{file_no}]"))
        # Get cells and partition them
        cells <- meltr::melt_csv(path) %>%
            filter(.data$data_type != "missing")
        partitions <- split_cells(cells)
        # Get tidied metadata
        metadata_df <- tidy_metadata(
            partitions$metadata,
            partitions$settings,
            filename,
            id_regex
        )
        # Get tidied experimental data
        data_df <- tidy_data(partitions$data, metadata_df)
        # Append data to list
        metadata_list <- append(metadata_list, list(metadata_df))
        data_list <- append(data_list, list(data_df))
    }
    # Bind data together
    tidy_metadata_df <- bind_rows(metadata_list) %>%
        arrange(.data$test_run_no)

    tidy_data_df <- bind_rows(data_list) %>%
        arrange(
            .data$test_run_no,
            .data$sample,
            .data$rep,
            .data$plate,
            .data$plate_row,
            .data$plate_col
        )
    # Return as list
    batch_data <- list(
        tidy_data = tidy_data_df,
        tidy_metadata = tidy_metadata_df
    )

    return(batch_data)
}

#' Imports and validates a drug_key csv file
#'
#' @param drug_key_path A path to a csv correlating well locations (`[plate,
#'   plate_row, plate_col]`) to drug information (`[catalog_no, cas_no,
#'   drug_name]`).
#'
#'   Well information should be unique across `[plate, plate_row, plate_col]`
#'   and follow the specifications of a 384 well plate (i.e plate_row and
#'   plate_col should fall within ranges A-P and 1-24, respectively)
#'
#'   All fields should be complete (excepting the `cas_no` for `PURO/PBS`)
#'
#' @return A tibble() that can then be supplied to add_drug_annot()
#' @export
#'
#' @examples
#' \dontrun{
#' drug_key_df <- import_drug_key("data/config/drug_well_key_complete.csv")
#' }
import_drug_key <- function(drug_key_path) {
    drug_key_df <- readr::read_csv(
        drug_key_path,
        col_types = readr::cols(
            plate = readr::col_integer(),
            plate_col = readr::col_integer(),
            plate_row = readr::col_character(),
            catalog_no = readr::col_character(),
            cas_no = readr::col_character(),
            drug_name = readr::col_character()
        )
    ) %>%
        # Ensure plate_row is all uppercase downstream
        mutate(plate_row = str_to_upper(.data$plate_row))

    validate_drug_key(drug_key_df, drug_key_path)

    return(drug_key_df)
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
#' @return tibble
#' @export
#'
#' @examples
#' \dontrun{
#' # Get inputs
#' result_paths <- Sys.glob("data/raw_id_corrected/*.CSV")
#' drug_key_df <- readr::read_csv("data/config/drug_well_key_complete.csv")
#'
#' # Process data
#' batch_data <- gather_plates(result_paths)
#' annot_data <- add_drug_annot(batch_data$tidy_data, drug_key_df)
#' }
add_drug_annot <- function(tidy_data_df, drug_key_df) {
    annot_data_df <- tidy_data_df %>%
        left_join(
            drug_key_df,
            # A unique combination of plate, plate_row, and plate_col determines
            # Which drug is in which well.
            by = c("plate", "plate_row", "plate_col")
        ) %>%
        relocate(.data$catalog_no:.data$drug_name, .after = .data$plate_col)

    return(annot_data_df)
}

# Visualisation Functions -------------------------------------------------

#' Visualises plate data as a heatmap
#'
#' Visualises plate data as a heatmap faceted by `treatment`, `rep` `conc_nm`
#' and `plate`
#'
#' @param tidy_data A tidied tibble produced by `gather_plates()$tidy_data` or
#'   `add_drug_annot()`. Ideally filtered to a few specific plates of interest,
#'   otherwise the output plot will be very large due to faceting.
#' @param value A string corresponding to the heatmap measure of interest,
#'   default `"fluor"`
#'
#' @return `ggplot()` object
#' @export
#'
#' @examples
#' \dontrun{
#' annot_data %>%
#'     filter(conc_nm == 10) %>%
#'     plot_plate()
#' }
plate_heatmap <- function(tidy_data, value = "fluor") {
    tidy_data %>%
        mutate(
            shape = case_match(
                .data$drug_name,
                "PURO" ~ 4,
                "PBS" ~ 1,
                .default = NA
            )
        ) %>%
        ggplot(
            aes(
                as.factor(.data$plate_col),
                forcats::fct_rev(as.factor(.data$plate_row))
            )
        ) +
        geom_tile(aes(fill = !!sym(value))) +
        geom_point(aes(shape = .data$shape)) +
        facet_grid(
            rows = vars(.data$treatment, .data$rep, .data$conc_nm),
            cols = vars(.data$plate)
        ) +
        scale_shape_identity() +
        scale_fill_distiller(palette = "Spectral") +
        xlab("Plate Columns") +
        ylab("Plate Rows") +
        coord_fixed(ratio = 1)
}
