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
            rules <- validator(.file = "data/config/drug_key_rules.yaml")

            out <- confront(drug_key_df, rules)
            any_fails <- summary(out)$fails %>% sum() > 0
            stopifnot(!any_fails)
        },
        error = function(c) {
            rules_violated <- summary(out) %>%
                filter(fails > 0) %>%
                left_join(meta(rules), by = "name") %>%
                select(name:description)

            entry_violations <- violating(drug_key_df, out)

            msg <- glue(
                "
                Drug Key: {path_name}
                No. of Rules Failed\t: {nrow(rules_violated)}
                No. of Rows Failed\t: {nrow(entry_violations)}

                '{basename(path_name)}' fails to meet all rules for a valid \\
                drug key. Please collect the violating entries in the tables \\
                below.
                "
            )

            c$message <- msg

            plot(out)
            print(rules_violated)
            print(entry_violations)

            stop(c)
        }
    )
}
