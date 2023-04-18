# Annotation Functions ----------------------------------------------------

# Annotation --------------------------------------------------------------

# Function that adds drug information to aggregated data given a drug:well key
# that specifies which drug is in which well
add_drug_info <- function(agg_data_df, drug_key_df) {
    complete_data_df <- agg_data_df %>%
        left_join(
            drug_key_df,
            # A unique combination of plate, plate_row, and plate_col determines
            # Which drug is in which well.
            by = c("plate", "plate_row", "plate_col")
        ) %>%
        relocate(catalog_no:drug_name, .after = plate_col)

    return(complete_data_df)
}
