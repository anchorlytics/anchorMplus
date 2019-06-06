# Assemble input syntax for Mplus

#' Word wrap to accommodate Mplus line length limitations.
#'
#' Mplus input syntax is limited to 90 characters on a line.
#' This function takes a character vector, concatenates it with spaces,
#' and word-wraps the result, returning a single string.
#'
#' @param .list a character vector
#' @param width passed to [base::strwrap()]
#' @param exdent passed to [base::strwrap()]
#' @param ... passed to [base::strwrap()]
#' @return a single string, potentially with newlines
#'
#' @export
#' @family Mplus helpers
#' @author Sean Ho <anchor@seanho.com>
#'
#' @examples
#' cat(wordwrap(rownames(mtcars)))
wordwrap <- function(.list, width = 80, exdent = 4, ...) {
  paste(
    strwrap(paste(.list, collapse = " "), width = width, exdent = exdent, ...),
    collapse = "\n")
}
