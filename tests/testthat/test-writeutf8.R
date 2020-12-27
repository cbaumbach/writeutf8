test_that("integer columns work", {
    expect_read_equal_write(data.frame(x = 1:2, y = 3:4))
})

test_that("column names with UTF-8 encoding work", {
    df <- structure(data.frame(1L), names = "\u9B3C")
    expect_equal(Encoding(names(df)), "UTF-8")
    expect_read_equal_write(df)
})

test_that("text with embedded double quotes works", {
    expect_read_equal_write(data.frame(x = '"'))
})

test_that("column names with embedded quotes work", {
    df <- data.frame(`"` = 1L, check.names = FALSE)
    expect_read_equal_write(df)
})

test_that("data frames with 0 rows work", {
    # Note that we have to pass colClasses through to readutf8 since
    # read.table, the workhorse behind readutf8, cannot infer column
    # types from an empty table.  Without colClasses, read.table would
    # default columns in an empty table to logical.
    expect_read_equal_write(
        data.frame(x = integer(0), y = character(0)),
        colClasses = c("integer", "character"))
})

test_that("a whitespace-only column is read back as logical NA", {
    filename <- tempfile()
    on.exit(file.remove(filename))
    writeutf8(data.frame(x = " "), filename)
    expect_equal_df(data.frame(x = NA), readutf8(filename))
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
        if (!expect_read_equal_write(random_data_frame())) {
            break
        }
    }
})
