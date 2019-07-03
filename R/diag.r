# Model diagnostics

#' Check if fitted mplusObject has converged
#'
#' This macro uses the existence of estimated standard errors as a proxy
#' for determining if the model was identified.
#'
#' @param .obj mplusObject fitted by \code{\link[MplusAutomation]{mplusModeler}}
#' @param param character name of field to look for in parameter list.
#'   By default, look for existence of standard errors.
#' @return logical
#'
#' @export
#' @family Model diagnostics
#' @author Sean Ho <anchor@seanho.com>
#'
is_identified <- function(.obj, param = "se") {
  # first set in $parameters is usually unstandardized
  param %in% names(.obj$results$parameters[[1]])
}

#' Format Mplus warning/error messages
#'
#' Pull all messages from the Mplus output object,
#' concatenate multi-line messages,
#' and convert case so messages are not all-caps.
#'
#' @param .obj mplusObject fitted by MplusAutomation::mplusModeler()
#' @param what character vector of fields to pull from .obj
#' @return character vector of messages
#'
#' @export
#' @importFrom stringi stri_trans_totitle
#' @family Model diagnostics
#' @author Sean Ho <anchor@seanho.com>
#'
get_messages <- function(.obj, what = c("errors", "warnings")) {
  lapply(
    lapply(
      unlist(.obj$results[what], recursive = FALSE),
      paste, collapse = " "
    ),
    stringi::stri_trans_totitle, type = "sentence"
  )
}

#' Abbreviate common phrases in Mplus messages
#'
#' @param x character vector, e.g., of Mplus messages
#' @return character vector, each message abbreviated
#'
#' @export
#' @importFrom stringr str_replace_all
#' @family Model diagnostics
#' @author Sean Ho <anchor@seanho.com>
#'
#' @examples
#' abbr_msg("cov matrix (psi) is not positive definite. Variable x.")
#'
abbr_msg <- function(x) {
  stringr::str_replace_all(
    x,
    c(
      "^.* may not be identified\\. .* parameter: (.*)$" = "Not identified: \\1",
      "^The condition number is .*$" = "",
      "^.* matrix (.*) is not positive definite\\..*$" = "Not positive definite: \\1",
      "^.* (best loglikelihood value was not replicated).*$" = "\\1"
    )
  )
}
