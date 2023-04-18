drug_well_df <- read_csv("data/config/drug_well_key.csv")
drug_metadata_df <- readxl::read_excel(
    "data/config/L2000-Z601881-200uL_reorganisedwithNC_07112022.xlsx",
    sheet = "Final compound list"
)

sel_metadata <- drug_metadata_df %>%
    select(drug_name = Name, catalog_no = Cat, cas_no = `CAS Number`) %>%
    # ACSS2 inhibitor is repeated twice
    distinct(catalog_no, .keep_all = TRUE)

drug_well_df %>%
    left_join(
        sel_metadata,
        by = "drug_name"
    ) %>%
    mutate(catalog_no = coalesce(catalog_no.x, catalog_no.y)) %>%
    relocate(catalog_no, cas_no, .after = plate_col) %>%
    select(-c(catalog_no.x, catalog_no.y)) %>%
    write_csv("data/config/drug_well_key_joined.csv")
