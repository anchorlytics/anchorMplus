# Parse Mplus SAVEDATA output

#' Parse Mplus TECH4 output
#'
#' Mplus' \code{TECH4} output contains first- and second-order estimated
#' \strong{moments} for the latent factors.
#' Unfortunately, the \code{MplusAutomation} library is still buggy
#' in parsing this section; hence this function.
#'
#' For \strong{single-group} models, the \code{TECH4} section has one subsection,
#' containing the following moments estimated for the latent factors:
#'
#' \enumerate{
#'   \item \strong{Means}
#'   \item \strong{Covariance} matrix
#'   \item \strong{Correlation} matrix
#' }
#'
#' If \strong{standard errors} are available, a second subsection is added with
#' the following for \strong{each} of the means, covariance, and correlation:
#'
#' \enumerate{
#'   \item \strong{Standard error}
#'   \item \strong{Ratio} of SE/estimate
#'   \item \strong{P-value} on estimate differing from zero
#' }
#'
#' Hence, there are 9 items within this second subsection.
#'
#' For \strong{multi-group} models, each group has its own subsection.
#' If standard errors are available, there are \strong{twice} as many subsections
#' as there are groups: first all the moments for each of the groups, then all
#' the standard errors for each of the groups.
#'
#' TODO: if no groups, returns a single group named "X"
#' TODO: extract vector of means
#'
#' @param .text contents of an Mplus output file, as read by
#'   \code{\link[base]{readLines}}
#' @return a list with one entry for each group.
#'   Each entry has members 'cov' and 'cor',
#'   each of which has members 'est', 'se', 'ratio', and 'p',
#'   each of which is a matrix.
#'
#' @export
#' @import MplusAutomation
#' @family Output parsing
#' @author Sean Ho <anchor@seanho.com>
#'
parse_tech4 <- function(.text) {
  output <- MplusAutomation:::parse_into_sections(.text)
  tech4 <- MplusAutomation:::getSection("^TECHNICAL 4 OUTPUT$", output)
  t4sec <- MplusAutomation:::getMultilineSection(
    ".* DERIVED .*", tech4, allowMultiple = TRUE)

  if (any(is.na(t4sec))) { return(NULL)}

  headers <- tech4[attr(t4sec, "matchlines")]
  headers <- trimws(headers)
  groups <- sapply(headers, function(h) {
    sub("^ESTIMATES DERIVED FROM THE MODEL( FOR )?", "", h)
  })
  groups <- make.names(groups)

  st <- list(
    est = "",
    se = "s.e. for",
    ratio = "est./s.e. for",
    p = "two-tailed p-value for"
  )
  moments <- list(
    cov = "covariance matrix",
    cor = "correlation matrix"
  )
  suffix <- "for the latent variables"

  # initialise result
  result <- list()
  for (gp in unique(groups)) {
    for (m in names(moments)) {
      result[[gp]][[m]] <- st
    }
  }

  # given a subsection within TECH4, extract all known matrices
  store_matrices <- function(sec, gp) {
    # fix est/SE ratio on diagonal of cov
    sec <- gsub("***********", "0.000", sec, fixed = TRUE)
    # iterate over possible entries
    for (m in names(moments)) {
      for (s in names(st)) {
        tag <- paste("^\\s*", st[[s]], "estimated", moments[[m]], suffix)
        # check if entry exists in current subsection
        if (any(grepl(tag, sec, ignore.case = TRUE))) {
          mat <- MplusAutomation:::matrixExtract(sec, tag, ignore.case = TRUE)
          mat <- ifelse(is.na(mat), t(mat), mat)
          result[[gp]][[m]][[s]] <<- mat
        }
      }
    }
  }

  # iterate over subsections
  mapply(store_matrices, t4sec, groups)
  result
}
