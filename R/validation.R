# Input Validation Functions ----------------------------------------------

validate_dirs <- function(paths) {
    coll <- makeAssertCollection()
    for (path in paths) {
        path_directory <- dirname(path)
        assert_directory_exists(
            path_directory,
            access = "r",
            add = coll
        )
    }
    reportAssertions(coll)
}

validate_csv_paths <- function(paths) {
    validate_dirs(paths)

    coll <- makeAssertCollection()
    for (path in paths) {
        assert_file_exists(
            path,
            extension = "csv",
            access = "r",
            add = coll
        )
    }
    reportAssertions(coll)
}

validate_drug_key <- function(drug_key_df, path_name) {
    tryCatch(
        {
            rules <- validate::validator(.file = "tests/rules/drug_key_rules.yaml")

            out <- validate::confront(drug_key_df, rules)
            any_fails <- validate::summary(out)$fails %>% sum() > 0
            stopifnot(!any_fails)
        },
        error = function(c) {
            rules_violated <- validate::summary(out) %>%
                filter(.data$fails > 0) %>%
                left_join(validate::meta(rules), by = "name") %>%
                select(.data$name:.data$description)

            entry_violations <- validate::violating(drug_key_df, out)

            msg <- glue::glue(
                "
                Drug Key: {path_name}
                No. of Rules Failed\t: {nrow(rules_violated)}
                No. of Rows Failed\t: {nrow(entry_violations)}

                '{basename(path_name)}' fails to meet all rules for a valid \\
                drug key. Please correct the violating entries in the tables \\
                below.
                "
            )

            c$message <- msg
            print(rules_violated)
            print(entry_violations)

            stop(c)
        }
    )
}
