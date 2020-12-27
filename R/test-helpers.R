expect_read_equal_write <- function(df) {
    filename <- tempfile()
    on.exit(file.remove(filename))
    writeutf8(df, filename)
    expect_equal_df(df, readutf8(filename, check.names = FALSE))
}

expect_equal_df <- function(expected, actual) {
    if (!equal_df(expected, actual)) {
        fail(utils::capture.output(suppressWarnings({
            cat(sprintf("\nGot %s\n\n", describe_df(actual)))
            print(actual)
            cat(sprintf("\nWant %s\n\n", describe_df(expected)))
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
    sprintf("%dx%d table with %s and names of length %s",
            nrow(df), ncol(df),
            paste(sapply(df, typeof), collapse = ", "),
            paste(nchar(names(df), type = "chars"), collapse = ", "))
}

test_random_vector <- function(random, type) {
    for (n in c(0, 3)) {
        expect_equal(typeof(random(n)), type)
        expect_equal(length(random(n)), n)
    }
}

random_logical <- function(n) {
    sample(c(TRUE, FALSE), size = n, replace = TRUE)
}

random_integer <- function(n) {
    sample(-10:10, size = n, replace = TRUE)
}

random_double <- function(n) {
    round(runif(n, min=-10, max=10), digits = 2)
}

random_char <- function() {
    sample(c(
        " ", "\t", "\n", "'", '"', "\\", "a", "ß",
        "\u0105",  # ą
        "\u0328",  # ogonek (little tail in ą)
        "\u0141",  # Ł
        "\u20AC",  # €
        "\u9B3C"   # 鬼
    ), 1)
}

random_string <- function(nchars) {
    paste(replicate(nchars, random_char()), collapse = "")
}

random_character <- function(n, nchars) {
    as.character(replicate(n, random_string(nchars)))
}

random_data_frame <- function() {
    random_df(ncols = sample.int(3, 1), nrows = sample.int(3, 1))
}

random_df <- function(ncols, nrows) {
    do.call(columns_to_df, random_columns(ncols, nrows))
}

columns_to_df <- function(...) {
    df <- data.frame(..., stringsAsFactors = FALSE)
    names(df) <- random_character(ncol(df), nchars = sample.int(3, 1))
    df
}

random_columns <- function(ncols, nrows) {
    FUNS <- list(random_logical, random_integer, random_double,
                 function(n) random_character(n, sample.int(4, 1)))
    lapply(sample(FUNS, ncols, replace = TRUE), function(f) f(nrows))
}