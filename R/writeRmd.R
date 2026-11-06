#' Write Rmarkdown file
#'
#' Write Rmarkdown-formatted strings to file.
#'
#' @param contents A character vector or a (nested) list of such vectors.
#' @param file String containing a file path or a connection object to an opened file.
#' @param append Logical scalar indicating whether to append \code{contents} to \code{file}.
#' @param clean.empty Boolean indicating whether repeated empty lines should be collapsed into a single empty line.
#' @param trailing.empty Boolean indicating whether an empty line should be added to the end of a non-empty \code{contents}, if one is not already present.
#'
#' @return \code{contents} is written to \code{file} and \code{NULL} is invisibly returned.
#'
#' @details
#' Each element of each character vector in \code{contents} is placed on a new line in the output.
#'
#' Empty strings are respected and manifest as empty lines.
#' If \code{clean.empty=TRUE}, multiple empty lines are replaced by a single empty line to avoid excessive empty space in the file.
#'
#' Setting \code{trailing.empty=TRUE} is recommended if further content is to be added to \code{file}.
#' This avoids unexpected formatting of appended text, e.g., Markdown section headers are not respected if they are preceded by a non-empty line.
#'
#' @author Aaron Lun
#'
#' @examples
#' contents <- list(
#'     c("# TITLE", ""),
#'     list(
#'         c("```{r}", "a <- 1", "```"),
#'         c("", "super foo bar")
#'     ),
#'     c("", "## section")
#' )
#' tmp <- tempfile(fileext=".Rmd")
#' writeRmd(contents, tmp)
#' cat(readLines(tmp), sep="\n") # having a look
#'
#' @export
writeRmd <- function(contents, file, append = FALSE, clean.empty = TRUE, trailing.empty = FALSE) {
    contents <- unlist(contents, use.names=FALSE)

    if (clean.empty) {
        runs <- rle(contents)
        runs$lengths[runs$values==""] <- 1L
        contents <- inverse.rle(runs)
    }

    if (trailing.empty && length(contents)) {
        last <- contents[length(contents)]
        if (last != "" && substr(last, nchar(last), nchar(last)) != "\n") {
            contents <- c(contents, "")
        }
    }

    write(contents, file=file, append=append, ncolumns=1)
    invisible(NULL)
}
