# Prepare data for Mplus

#' Specify categorical variables to Mplus
#'
#' This creates an Mplus input syntax line indicating which
#' variables are categorical (ordinal or binary/dichotomous).
#' A variable is deemed dichotomous if it has at most two unique non-NA values,
#' regardless of its type.
#'
#' @param .data data frame with proper column types
#' @param ... additional options passed to [mp_wordwrap()]
#' @return a single string that can be passed to
#'   [MplusAutomation::mplusObject()] in the `VARIABLE` argument
#'
#' @export
#' @family Mplus helpers
#' @author Sean Ho <anchor@seanho.com>
#'
#' @examples
#' cat(mp_cat(data.frame(
#'   Number_of_Cylinders = ordered(mtcars$cyl),
#'   Miles_per_Gallon = mtcars$mpg,
#'   Engine_V_shaped_or_Straight = ordered(mtcars$vs),
#'   Transmission_Automatic_or_Manual = ordered(mtcars$am))))
mp_cat <- function(.data, ...) {
  mp_wordwrap(c(
    "CATEGORICAL =",
    names(which(sapply(.data, function(col) {
      is.ordered(col) | length(na.omit(unique(col))) <= 2
    }))),
    ";"), ...)
}
