# Prepare data for Mplus

#' Specify categorical variables to Mplus
#'
#' This creates an Mplus input syntax line indicating which
#' variables are categorical (ordinal or binary/dichotomous).
#' A variable is deemed dichotomous if it has at most two unique non-NA values,
#' regardless of its type.
#'
#' The return value is a single string that can be passed to
#' [MplusAutomation::mplusObject()] in the `VARIABLE` argument
#'
#' @param .data data frame with proper column types
#' @param ... additional options passed to [base::wordwrap()]
#' @return a single string starting with `CATEGORICAL =`
#'
#' @importFrom stats na.omit
#'
#' @export
#' @family Data preparation
#'
#' @examples
#' cat(mp_cat(data.frame(
#'   Number_of_Cylinders = ordered(mtcars$cyl),
#'   Miles_per_Gallon = mtcars$mpg,
#'   Engine_V_shaped_or_Straight = ordered(mtcars$vs),
#'   Transmission_Automatic_or_Manual = ordered(mtcars$am))))
#'
mp_cat <- function(.data, ...) {
  wordwrap(c(
    "CATEGORICAL =",
    names(which(sapply(.data, function(col) {
      is.ordered(col) | length(na.omit(unique(col))) <= 2
    }))),
    ";"), ...)
}

#' Convert column types for Mplus
#'
#' Mplus requires numeric input data.  This function converts each column
#' of a data frame according to its type:
#' + **Character**: Dropped completely. If you want to preserve these,
#'   convert them beforehand to factor.
#' + **Ordinal**: Levels are converted to integer.
#' + **All others**: Converted to integer.
#'
#' @param .data data frame with columns in standard R types
#' @return data frame with only numeric/integer columns
#'
#' @importFrom dplyr select mutate across everything `%>%`
#' @importFrom rlang is_character
#'
#' @export
#' @family Data preparation
#'
#' @examples
#'
mp_numeric <- function(.data) {
  .data %>%
    select(!tidyselect:::where(is_character)) %>%
    mutate(across(tidyselect:::where(is.ordered),  ~as.integer(levels(.))[.])) %>%
    mutate(across(everything(), as.integer))
}
