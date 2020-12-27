writeutf8 <- function(df, filename) {
    con <- file(filename, open = "wb", encoding = "native.enc")
    on.exit(close(con))
    header <- paste(names(df), collapse = "\t")
    rows <- do.call(function(...) paste(..., sep = "\t"), unname(df[]))
    writeLines(enc2utf8(c(header, rows)), con, sep = "\n", useBytes = TRUE)
}

readutf8 <- function(filename, ...) {
    read.delim(filename, encoding = "UTF-8", ...)
}
