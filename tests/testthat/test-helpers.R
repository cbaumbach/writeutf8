test_that("random_logical works", {
    test_random_vector(random_logical, "logical")
})

test_that("random_integer works", {
    test_random_vector(random_integer, "integer")
})

test_that("random_double works", {
    test_random_vector(random_double, "double")
})

test_that("random_char works", {
    for (i in 1:10) {
        expect_equal(nchar(random_char(), type = "chars"), 1)
    }
})

test_that("random_string works", {
    for (i in 1:10) {
        expect_equal(nchar(random_string(4), type = "chars"), 4)
    }
})

test_that("random_character works", {
    test_random_vector(function(n) random_character(n, 3), "character")
})

test_that("add_random_missings works", {
    expect_true(anyNA(add_random_missings(1:10, prob = 1)))
    expect_false(anyNA(add_random_missings(1:10, prob = 0)))
})

test_that("random_data_frame produces a data frame", {
    expect_s3_class(random_data_frame(), "data.frame")
})

test_that("data frame has at least one column", {
    for (i in 1:10) {
        expect_gte(ncol(random_data_frame()), 1)
    }
})

test_that("data frame has at least one row", {
    for (i in 1:10) {
        expect_gte(nrow(random_data_frame()), 1)
    }
})

test_that("is_ambiguous works", {
    expect_true(is_ambiguous(data.frame(x = " ")))
    expect_true(is_ambiguous(data.frame(x = NA)))
    expect_true(is_ambiguous(data.frame(x = integer(0))))
    expect_false(is_ambiguous(data.frame(x = 1)))
})

test_that("is_whitespace_only", {
    expect_true(is_whitespace_only(" "))
    expect_true(is_whitespace_only("\t"))
    expect_true(is_whitespace_only("\r"))
    expect_true(is_whitespace_only("\n"))
    expect_true(is_whitespace_only(NA_character_))
    expect_false(is_whitespace_only("a"))
    expect_false(is_whitespace_only(" a"))
    expect_false(is_whitespace_only(1))
    expect_false(is_whitespace_only(NA))
})

test_that("random data frame is never ambiguous", {
    for (i in 1:10) {
        expect_false(is_ambiguous(random_data_frame()))
    }
})

test_that("random data frame can contain NAs", {
    for (i in 1:10) {
        if (anyNA(random_data_frame())) {
            succeed()
            return()
        }
    }
    fail("found no NAs")
})
