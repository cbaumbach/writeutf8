writeutf8 <- function(df, filename) {
    write_without_reencoding(enc2utf8(df_to_text(df)), filename)
}

df_to_text <- function(df) {
    c(collapse_header(df), collapse_columns(df))
}

collapse_header <- function(df) {
    paste(names(df), collapse = "\t")
}

collapse_columns <- function(df) {
    do.call(function(...) paste(..., sep = "\t"), quote_columns(df))
}

# Embedded double quotes are doubled (""), not escaped (\"), so that
# read.table with sep = "\t" recognizes them (see ?scan for details).
quote_columns <- function(df) {
    lapply(unname(df[]), function(x) {
        if (is.character(x) && length(x) > 0L) {
            paste0('"', gsub('"', '""', x), '"')
        } else {
            x
        }
    })
}

write_without_reencoding <- function(text, filename) {
    con <- file(filename, open = "wb", encoding = "native.enc")
    on.exit(close(con))
    writeLines(text, con, sep = "\n", useBytes = TRUE)
}

readutf8 <- function(filename, ...) {
    read.delim(filename, encoding = "UTF-8", ...)
}
