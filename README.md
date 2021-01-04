# Save data frames as UTF-8 encoded plain text

## Motivation

Under Linux or macOS, R's `write.table` can be used to save data
frames as UTF-8 encoded plain text.  Under Microsoft Windows, this
approach no longer works because Windows does not support UTF-8
locales.  `writeutf8` solves this problem.  `writeutf8` saves a data
frame as a UTF-8 encoded plain text file and `readutf8` reads it back
into R.


## Installation

    install.packages("remotes")
    remotes::install_github("cbaumbach/writeutf8")


## Output format

If the only purpose of writing data to disk using `writeutf8` is to
read it back into R later, then the exact output format is irrelevant;
just use `readutf8` to read the data frame back into R.  In this case,
no customization of the output format is necessary and the code for
writing and reading will be very concise: save with `writeutf(df,
filename)` and read with `readutf8(filename)`.

The only time when the output format matters is when the written out
data has to be read by another software that expects a certain input
format.

The data frame written by `writeutf8` contains a header row with
column names.  All columns, including the column names, are quoted
with double quotes.  This allows character columns and column names to
have arbitrary content.  Double quotes embedded in character columns
and column names are doubled.  Line endings default to the Windows
convention of `"\r\n"` because most users of `writeutf8` will be
working under Windows.  Row names are not included in the output.


## Example

    df <- data.frame(    # ascii  latin1  UTF-8
        w = c(NA,   "",    "abc", "\xd8", "\u9B3C"), # character
        x = c(1L,   NA,    3L,    4L,     5L),       # integer
        y = c(1.5,  2.5,   NA,    4.5,    5.5),      # double
        z = c(TRUE, FALSE, TRUE,  NA,     TRUE),     # logical
        t = as.POSIXct("2021-01-01 15:30:45"),       # POSIXct
        d = as.Date("2021-01-01"))                   # Date

    Encoding(df$w) <- c(rep("unknown", 3), "latin1", "UTF-8")

    writeutf8(df, "data.tsv")

    # Then later in another R script:
    df <- readutf8("data.tsv")
