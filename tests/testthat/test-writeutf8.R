test_that("integer columns work", {
    expect_read_equal_write(data.frame(x = 1:2, y = 3:4))
})

test_that("column names with UTF-8 encoding work", {
    df <- structure(data.frame(1L), names = "\u9B3C")
    expect_equal(Encoding(names(df)), "UTF-8")
    expect_read_equal_write(df)
})

test_that("random data frames work", {
    set.seed(1234)
    for (i in 1:100) {
        if (!expect_read_equal_write(random_data_frame())) {
            break
        }
    }
})