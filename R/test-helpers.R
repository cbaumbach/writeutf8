expect_read_equal_write <- function(df) {
    filename <- tempfile()
    on.exit(file.remove(filename))
    writeutf8(df, filename)
    expect_equal_df(df, readutf8(filename))
}

expect_equal_df <- function(expected, actual) {
    if (!equal_df(expected, actual)) {
        fail(utils::capture.output(suppressWarnings({
            cat(sprintf("\nGot %s:\n", describe_df(actual)))
            print(actual)
            cat(sprintf("\nWant %s:\n", describe_df(expected)))
            print(expected)
        })))
        return(FALSE)
    }
    succeed()
    return(TRUE)
}

equal_df <- function(df1, df2) {
    if (has_double(df1) || has_double(df2)) {
        isTRUE(all.equal(df1, df2))
    } else {
        identical(df1, df2)
    }
}

has_double <- function(df) {
    "double" %in% sapply(df, typeof)
}

describe_df <- function(df) {
    sprintf("%dx%d table with %s", nrow(df), ncol(df),
            paste(sapply(df, typeof), collapse = ", "))
}
