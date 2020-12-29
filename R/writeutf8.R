writeutf8 <- function(df, filename, na = "NA") {
    write_without_reencoding(enc2utf8(df_to_text(df, na)), filename)
}

df_to_text <- function(df, na) {
    c(collapse_header(df), collapse_columns(df, na))
}

collapse_header <- function(df) {
    paste(quote(names(df)), collapse = "\t")
}

collapse_columns <- function(df, na) {
    do.call(function(...) paste(..., sep = "\t"), quote_columns(df, na))
}

quote_columns <- function(df, na) {
    lapply(unname(df[]), function(x) {
        if (is.character(x) && length(x) > 0L) {
            ifelse(is.na(x), na, quote(x))
        } else {
            ifelse(is.na(x), na, x)
        }
    })
}

# Embedded double quotes are doubled (""), not escaped (\"), so that
# read.table with sep = "\t" recognizes them (see ?scan for details).
quote <- function(x) {
    paste0('"', gsub('"', '""', x), '"')
}

write_without_reencoding <- function(text, filename) {
    con <- file(filename, open = "wb", encoding = "native.enc")
    on.exit(close(con))
    writeLines(text, con, sep = "\n", useBytes = TRUE)
}

readutf8 <- function(filename, ...) {
    read.delim(filename, encoding = "UTF-8", ...)
}
