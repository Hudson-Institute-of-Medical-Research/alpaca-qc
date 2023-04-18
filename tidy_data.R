library(readr)

paths <- Sys.glob("data/raw_id_corrected/*.CSV")
drug_key_df <- read_csv("data/config/drug_well_key_complete.csv")
#
test <- agg_data(paths, drug_key_df)
