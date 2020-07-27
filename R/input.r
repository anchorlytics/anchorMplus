# Assemble input syntax for Mplus

#' Word wrap to accommodate Mplus line length limitations.
#'
#' Mplus input syntax is limited to 90 characters on a line.
#' This function is a wrapper around [base::strwrap()]
#' with reasonable defaults.
#'
#' @param x a character vector
#' @param width passed to [base::strwrap()]
#' @param exdent passed to [base::strwrap()]
#' @param ... passed to [base::strwrap()]
#' @return a single string, potentially with newlines
#'
#' @export
#' @family Input syntax
#'
#' @examples
#' cat(wordwrap(rownames(mtcars)))
#'
wordwrap <- function(x, width = 80, exdent = 4, ...) {
  paste(strwrap(
    paste(x, collapse = " "),
    width = width,
    exdent = exdent,
    ...
  ), collapse = "\n")
}
