# Assemble input syntax for Mplus

#' Word wrap to accommodate Mplus line length limitations.
#'
#' Mplus input syntax is limited to 90 characters on a line.
#' This function is a wrapper around \code{\link[base]{strwrap}}
#' with reasonable defaults.
#'
#' @param x a character vector
#' @param width passed to \code{\link[base]{strwrap}}
#' @param exdent passed to \code{\link[base]{strwrap}}
#' @param ... passed to \code{\link[base]{strwrap}}
#' @return a single string, potentially with newlines
#'
#' @export
#' @family Input syntax
#' @author Sean Ho <anchor@seanho.com>
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
