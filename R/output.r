# Parse Mplus SAVEDATA output

#' Parse Mplus TECH4 output
#'
#' Mplus' `TECH4` output contains first- and second-order estimated **moments**
#' for the latent factors.
#' Unfortunately, the `MplusAutomation` library is still buggy
#' in parsing this section; hence this function.
#'
#' For **single-group** models, the TECH4 section has one subsection,
#' containing the following moments estimated for the latent factors:
#'
#' 1. **Means**
#' 2. **Covariance** matrix
#' 3. **Correlation** matrix
#'
#' If **standard errors** are available, a second subsection is added with
#' the following for **each** of the means, covariance, and correlation:
#'
#' 1. **Standard error**
#' 2. **Ratio** of SE/estimate
#' 3. **P-value** on estimate differing from zero
#'
#' Hence, there are 9 items within this second subsection.
#'
#' For **multi-group** models, each group has its own subsection.
#' If standard errors are available, there are **twice** as many subsections as
#' there are groups: first all the moments for each of the groups, then all the
#' standard errors for each of the groups.
#'
#' @param .text contents of an Mplus output file, as read by readLines()
#' @return a list with one entry for each group.
#'   Each entry has members 'cov' and 'cor',
#'   each of which has members 'est', 'se', 'ratio', and 'p',
#'   each of which is a matrix.
#'
#' @export
#' @family Mplus helpers
#' @author Sean Ho <anchor@seanho.com>
parse_tech4 <- function(.text) {
  output <- MplusAutomation:::parse_into_sections(.text)
  tech4 <- MplusAutomation:::getSection("^TECHNICAL 4 OUTPUT$", output)
  t4sec <- MplusAutomation:::getMultilineSection(
    ".* DERIVED .*", tech4, allowMultiple = TRUE)

  if (any(is.na(t4sec))) { return(NULL)}

  # TODO: if no groups, this paragraph produces one group named "X"
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
    #    mean = "means", # TODO: vector, not matrixExtract()
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
