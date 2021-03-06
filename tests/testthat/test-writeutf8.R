test_that("data frame with mix of ascii, latin1, and UTF-8 works", {
    with_mixed_encodings <- encode(c(
        NA,       "unknown",
        "",       "unknown",
        "abc",    "unknown",
        "\xd8",   "latin1",
        "\u9B3C", "UTF-8"))
    df <- data.frame(
        w = with_mixed_encodings,                 # character
        x = c(1L,    NA,    3L,    4L,     5L),   # integer
        y = c(1.5,   2.5,   NA,    4.5,    5.5),  # double
        z = c(TRUE,  FALSE, TRUE,  NA,     TRUE)) # logical
    expect_read_equal_write(df)
})

test_that("output file matches reference file byte for byte", {
    filename <- tempfile()
    on.exit(file.remove(filename))
    with_mixed_encodings <- encode(c(
        NA,       "unknown",
        "",       "unknown",
        "abc",    "unknown",
        "\xd8",   "latin1",
        "\u9B3C", "UTF-8"))
    df <- data.frame(
        w = with_mixed_encodings,                 # character
        x = c(1L,    NA,    3L,    4L,     5L),   # integer
        y = c(1.5,   2.5,   NA,    4.5,    5.5),  # double
        z = c(TRUE,  FALSE, TRUE,  NA,     TRUE), # logical
        t = as.POSIXct("2021-01-01 15:30:45"),    # POSIXct
        d = as.Date("2021-01-01"))                # Date
    writeutf8(df, filename)
    expect_identical(
        readChar(filename, 100, useBytes = TRUE),
        readChar("data/file.tsv", 100, useBytes = TRUE))
})

test_that("column names with UTF-8 encoding work", {
    expect_read_equal_write(
        structure(data.frame(1L), names = "\u9B3C"),
        readutf8_extra_args = list(check.names = FALSE))
})

test_that("text with embedded double quotes works", {
    expect_read_equal_write(data.frame(x = '"'))
})

test_that("we get an error if sep = \"\" in readutf8", {
    filename <- tempfile()
    on.exit(file.remove(filename))
    writeutf8(data.frame(x = 1), filename)
    expect_error(readutf8(filename, sep = ""))
})

test_that("column names with embedded quotes work", {
    expect_read_equal_write(
        data.frame(`"` = 1L, check.names = FALSE),
        readutf8_extra_args = list(check.names = FALSE))
})

test_that("character columns are not converted to factors by default", {
    df <- data.frame(x = "a", stringsAsFactors = FALSE)
    expect_read_equal_write(df)
})

test_that("data frames with 0 rows work", {
    # Note that we have to pass colClasses through to readutf8 since
    # read.table, the workhorse behind readutf8, cannot infer column
    # types from an empty table.  Without colClasses, read.table would
    # default columns in an empty table to logical.
    expect_read_equal_write(
        data.frame(x = integer(0), y = character(0)),
        readutf8_extra_args = list(
            colClasses = c("integer", "character")))
})

test_that("a whitespace-only column is read back as logical NA", {
    filename <- tempfile()
    on.exit(file.remove(filename))
    writeutf8(data.frame(x = " "), filename)
    expect_equal_df(data.frame(x = NA), readutf8(filename))
})

test_that("a missing-only column is read back as logical NA", {
    filename <- tempfile()
    on.exit(file.remove(filename))
    writeutf8(data.frame(x = NA_character_), filename)
    expect_equal_df(data.frame(x = NA), readutf8(filename))
})

test_that("we can choose how NAs are represented when written", {
    expect_read_equal_write(
        data.frame(x = c("NA", NA)),
        writeutf8_extra_args = list(na = "foo"),
        readutf8_extra_args = list(na.strings = "foo"))
})

test_that("end-of-line sequence in output defaults to \\r\\n", {
    filename <- tempfile()
    on.exit(file.remove(filename))
    writeutf8(data.frame(x = 1), filename)
    expect_match(readChar(filename, 20, useBytes = TRUE), "\\r\\n")
})

test_that("we can change the end-of-line sequence in output", {
    filename <- tempfile()
    on.exit(file.remove(filename))
    writeutf8(data.frame(x = 1), filename, eol = "\n")
    expect_match(readChar(filename, 20, useBytes = TRUE), "\\n")
    expect_false(grepl("\\r\\n", readChar(filename, 20, useBytes = TRUE)))
})

test_that("the column separator in output defaults to \\t", {
    filename <- tempfile()
    on.exit(file.remove(filename))
    writeutf8(data.frame(x = 1, y = 2), filename)
    expect_match(readChar(filename, 20, useBytes = TRUE), "\\t")
})

test_that("we can change the column separator in the output", {
    filename <- tempfile()
    on.exit(file.remove(filename))
    writeutf8(data.frame(x = 1, y = 2), filename, sep = ",")
    expect_match(readChar(filename, 20, useBytes = TRUE), ",")
})

test_that("Date objects can be written and read back", {
    expect_read_equal_write(
        data.frame(t = Sys.Date()),
        readutf8_extra_args = list(colClasses = "Date"))
})

test_that("POSIXct times lose subsecond precision", {
    filename <- tempfile()
    on.exit(file.remove(filename))
    df <- data.frame(t = Sys.time())
    writeutf8(df, filename)
    df2 <- readutf8(filename, colClasses = "POSIXct")
    expect_lte(df$t - df2$t, 1)
})

test_that("POSIXct subsecond precision can be preserved manually", {
    filename <- tempfile()
    on.exit(file.remove(filename))
    t <- Sys.time()
    df <- data.frame(seconds = as.double(t))
    writeutf8(df, filename)
    df2 <- readutf8(filename)
    df2$t <- as.POSIXct(df2$seconds, origin = "1970-01-01")
    expect_equal(df2$t, t)
})

test_that("POSIXct columns are quoted", {
    # POSIXct times are formatted as "YYYY-MM-DD hh:mm:ss TZ" in the
    # output.  With sep = " ", they would be interpreted as multiple
    # columns if unquoted.
    expect_read_equal_write(
        data.frame(t = trunc(Sys.time())),
        writeutf8_extra_args = list(sep = " "),
        readutf8_extra_args = list(sep = " ", colClasses = "POSIXct"))
})

test_that("double columns are quoted", {
    # Double values contain a decimal point.  With sep = ".", a column
    # of doubles would be interpreted as two columns if unquoted.
    expect_read_equal_write(
        data.frame(x = 1.5),
        writeutf8_extra_args = list(sep = "."),
        readutf8_extra_args = list(sep = "."))
})

# scan converts all end-of-line sequences to newlines.  Even when they
# are embedded in quoted strings!  The latter is not documented.  The
# closest I could find is the following excerpt from ?scan:
#
#   Whatever mode the connection is opened in, any of LF, CRLF or CR
#   will be accepted as the EOL marker for a line and so will match
#   sep = "\n".

test_that("scan converts embedded end-of-line sequences to \\n", {
    scan <- function(text) {
        base::scan(text = text, what = "", quote = '"', quiet = TRUE)
    }
    expect_equal(scan('"\n"'), "\n")
    expect_equal(scan('"\r"'), "\n")
    expect_equal(scan('"\r\n"'), "\n")
})

# It follows that read.table, which uses scan under the hood, cannot
# be used to read a string containing embedded \r or \r\n from a file
# into a data frame.  The string that ends up in the data frame will
# have its \r or \r\n replaced by \n.

test_that("embedded \\r or \\r\\n are replaced by \\n", {
    filename <- tempfile()
    on.exit(file.remove(filename))
    writeutf8(data.frame(x = c("\r", "\r\n")), filename)
    expect_equal_df(data.frame(x = c("\n", "\n")),
                    readutf8(filename, colClasses = "character"))
})

test_that("random data frames work", {
    for (i in 1:100) {
        ok <- expect_read_equal_write(
            random_data_frame(),
            readutf8_extra_args = list(check.names = FALSE))
        if (!ok) {
            break
        }
    }
})
