#' Save data frames as UTF-8 encoded plain text
#'
#' \code{writeutf8} saves a data frame as a UTF-8 encoded plain text
#' file and \code{readutf8} reads it back into R.
#'
#' @section Motivation:
#' Under Linux or macOS, \code{\link[utils]{write.table}} can be used
#' to save data frames as UTF-8 encoded plain text.  Under Microsoft
#' Windows, this approach no longer works because Windows does not
#' support UTF-8 locales.  \code{writeutf8} solves this problem.
#'
#' @section Output format:
#' If the only purpose of writing data to disk using \code{writeutf8}
#' is to read it back into R later, then the exact output format is
#' irrelevant; just use \code{readutf8} to read the data frame back
#' into R.  In this case, no customization of the output format is
#' necessary and the code for writing and reading will be very
#' concise: save with \code{writeutf(df, filename)} and read with
#' \code{readutf8(filename)}.
#'
#' The only time when the output format matters is when the written
#' out data has to be read by another software that expects a certain
#' input format.
#'
#' The data frame written by \code{writeutf8} contains a header row
#' with column names.  All columns, including the column names, are
#' quoted with double quotes.  This allows character columns and
#' column names to have arbitrary content.  Double quotes embedded in
#' character columns and column names are doubled.  Line endings
#' default to the Windows convention of \code{"\r\n"} because most
#' users of \code{writeutf8} will be working under Windows.  Row names
#' are not included in the output.
#'
#' @param df a data frame
#' @param filename the name of the output file
#' @param sep the field separator string
#' @param na the string to use for missing values in the data
#' @param eol the character(s) to print at the end of each row
#'
#' @aliases readutf8
#'
#' @seealso
#'   \code{\link[utils]{write.table}},
#'   \code{\link[base]{writeLines}},
#'   \code{\link[utils]{read.table}},
#'   \code{\link[base]{scan}},
#'   \code{\link[base]{Encoding}}
#'
#' @examples
#' \dontrun{
#' df <- data.frame(    # ascii  latin1  UTF-8
#'     w = c(NA,   "",    "abc", "\xd8", "\u9B3C"), # character
#'     x = c(1L,   NA,    3L,    4L,     5L),       # integer
#'     y = c(1.5,  2.5,   NA,    4.5,    5.5),      # double
#'     z = c(TRUE, FALSE, TRUE,  NA,     TRUE),     # logical
#'     t = as.POSIXct("2021-01-01 15:30:45"),       # POSIXct
#'     d = as.Date("2021-01-01"))                   # Date
#'
#' writeutf8(df, "data.tsv")
#'
#' # Then later in another R script:
#' df <- readutf8("data.tsv")
#' }
#'
#' @export
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

#' @rdname writeutf8
#'
#' @param stringsAsFactors Convert character vectors to factors?
#' @param ... further arguments passed to \code{\link[utils]{read.table}}
#'
#' @export
readutf8 <- function(filename, stringsAsFactors = FALSE, ...) {
    # scan, which is used internally by read.table, expects embedded
    # double quotes to be escaped when sep = "" and otherwise doubled.
    # writeutf8 always doubles embedded double quotes.  Therefore,
    # readutf8 must never be called with sep = "" on a file with a
    # data frame containing embedded double quotes.
    if (methods::hasArg("sep") && list(...)$sep == "") {
        stop("Can't use sep = \"\". Specify separator explicitly.")
    }
    utils::read.delim(filename, encoding = "UTF-8",
                      stringsAsFactors = stringsAsFactors, ...)
}
