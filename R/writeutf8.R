writeutf8 <- function(df, filename, sep = "\t", na = "NA", eol = "\r\n") {
    write_without_reencoding(enc2utf8(df_to_text(df, sep, na)), filename, eol)
}

df_to_text <- function(df, sep, na) {
    c(collapse_header(df, sep), collapse_columns(df, sep, na))
}

collapse_header <- function(df, sep) {
    paste(quote(names(df)), collapse = sep)
}

collapse_columns <- function(df, sep, na) {
    do.call(function(...) paste(..., sep = sep),
            lapply(unname(df[]), quote, na = na))
}

# Embedded double quotes are doubled (""), not escaped (\"), so that
# read.table with sep = "\t" recognizes them (see ?scan for details).
quote <- function(x, na) {
    if (length(x) == 0) {
        return(character(0))
    }
    wrap <- function(x) paste0('"', x, '"')
    double_embedded_quotes <- function(x) gsub('"', '""', x)
    replace_NAs <- function(x, na) ifelse(is.na(x), na, x)
    wrap(double_embedded_quotes(replace_NAs(as.character(x), na)))
}

write_without_reencoding <- function(text, filename, eol) {
    con <- file(filename, open = "wb", encoding = "native.enc")
    on.exit(close(con))
    writeLines(text, con, sep = eol, useBytes = TRUE)
}

readutf8 <- function(filename, stringsAsFactors = FALSE, ...) {
    read.delim(filename, encoding = "UTF-8",
               stringsAsFactors = stringsAsFactors, ...)
}
