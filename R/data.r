# Prepare data for Mplus

#' Specify categorical variables to Mplus
#'
#' This creates an Mplus input syntax line indicating which
#' variables are categorical (ordinal or binary/dichotomous).
#' A variable is deemed dichotomous if it has at most two unique non-NA values,
#' regardless of its type.
#'
#' The return value is a single string that can be passed to
#' \code{\link[MplusAutomation]{mplusObject}} in the \code{VARIABLE} argument
#'
#' @param .data data frame with proper column types
#' @param ... additional options passed to \code{\link{wordwrap}}
#' @return a single string starting with \code{CATEGORICAL =}
#'
#' @export
#' @importFrom stats na.omit
#' @family Data preparation
#' @author Sean Ho <anchor@seanho.com>
#'
#' @examples
#' cat(mp_cat(data.frame(
#'   Number_of_Cylinders = ordered(mtcars$cyl),
#'   Miles_per_Gallon = mtcars$mpg,
#'   Engine_V_shaped_or_Straight = ordered(mtcars$vs),
#'   Transmission_Automatic_or_Manual = ordered(mtcars$am))))
mp_cat <- function(.data, ...) {
  wordwrap(c(
    "CATEGORICAL =",
    names(which(sapply(.data, function(col) {
      is.ordered(col) | length(na.omit(unique(col))) <= 2
    }))),
    ";"), ...)
}
