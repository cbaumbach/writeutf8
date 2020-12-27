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

test_that("random data frames work", {
    set.seed(1234)
    for (i in 1:100) {
        if (!expect_read_equal_write(random_data_frame())) {
            break
        }
    }
})
