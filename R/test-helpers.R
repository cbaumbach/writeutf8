expect_read_equal_write <- function(
    df,
    writeutf8_extra_args = list(),
    readutf8_extra_args = list())
{
    write <- function(...) writeutf8(df, filename, ...)
    read <- function(...) readutf8(filename, ...)
    filename <- tempfile()
    on.exit(file.remove(filename))
    do.call(write, writeutf8_extra_args)
    expect_equal_df(df, do.call(read, readutf8_extra_args))
}

expect_equal_df <- function(expected, actual) {
    if (!equal_df(expected, actual)) {
        testthat::fail(utils::capture.output(suppressWarnings({
            cat(sprintf("\nGot %s\n\n", describe_df(actual)))
            print(actual)
            cat(sprintf("\nWant %s\n\n", describe_df(expected)))
            print(expected)
        })))
        return(FALSE)
    }
    testthat::succeed()
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
        testthat::expect_equal(typeof(random(n)), type)
        testthat::expect_equal(length(random(n)), n)
    }
}

random_logical <- function(n) {
    sample(c(TRUE, FALSE), size = n, replace = TRUE)
}

random_integer <- function(n) {
    sample(-10:10, size = n, replace = TRUE)
}

random_double <- function(n) {
    round(stats::runif(n, min=-10, max=10), digits = 2)
}

CHARS_AND_ENCODINGS <- c(
    " ",      "unknown",
    "\t",     "unknown",
    "\n",     "unknown",
    "'",      "unknown",
    '"',      "unknown",
    "\\",     "unknown",
    "a",      "unknown",
    "\xDF",   "latin1",  # ß
    "\xC6",   "latin1",  # Æ
    "\u00DF", "UTF-8",   # ß
    "\u00C6", "UTF-8",   # Æ
    "\u0105", "UTF-8",   # ą
    "\u0328", "UTF-8",   # ogonek (little tail in ą)
    "\u0141", "UTF-8",   # Ł
    "\u20AC", "UTF-8",   # €
    "\u9B3C", "UTF-8")   # Chinese character

encode <- function(chars_and_encodings) {
    chars <- chars_and_encodings[c(TRUE, FALSE)]
    encodings <- chars_and_encodings[c(FALSE, TRUE)]
    set_encoding <- function(char, enc) {
        Encoding(char) <- enc
        char
    }
    mapply(set_encoding, chars, encodings, USE.NAMES = FALSE)
}

CHARS <- encode(CHARS_AND_ENCODINGS)

random_char <- function() {
    sample(CHARS, 1)
}

random_string <- function(nchars) {
    paste(replicate(nchars, random_char()), collapse = "")
}

random_character <- function(n, nchars) {
    as.character(replicate(n, random_string(nchars)))
}

# Return a random data frame that can be uniquely reconstructed from
# its written representation as produced by writeutf8.
random_data_frame <- function() {
    df <-  random_df(ncols = sample.int(3, 1), nrows = sample.int(3, 1))
    if (is_ambiguous(df)) {
        random_data_frame()
    } else {
        df
    }
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
    prob <- 1 / (nrows + 1)
    lapply(sample(FUNS, ncols, replace = TRUE),
           function(f) add_random_missings(f(nrows), prob = prob))
}

add_random_missings <- function(x, prob) {
    x[stats::runif(length(x)) <= prob] <- NA
    x
}

# Return TRUE for data frames that cannot be uniquely reconstructed
# based on their written representation as produced by writeutf8.
is_ambiguous <- function(df) {
    (nrow(df) == 0
        || any(sapply(df, is_missing_only))
        || any(sapply(df, is_whitespace_only)))
}

is_missing_only <- function(x) {
    all(is.na(x))
}

is_whitespace_only <- function(x) {
    is.character(x) && all(is.na(x) | grepl("^[ \t\r\n]*$", x))
}
