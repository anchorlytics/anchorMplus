# Invoke Mplus and save output

#' Run Mplus and save full output
#'
#' MplusAutomation::mplusModeler() runs Mplus and augments the
#' mplusOjbect with a $results element.
#' This wrapper saves the full text output from Mplus in an element named
#' $stdout within that $results element.
#'
#' This function has side-effects, just like mplusModeler(), in that
#' it leaves Mplus input, output, and savedata files in the current directory.
#'
#' @param .obj mplusObject as created by MplusAutomation::mplusObject()
#' @param name basename for generated Mplus files (no extension)
#' @param run set to 0 for dry-run (passed to MplusAutomation::mplusModeler())
#' @param ... additional options passed to MplusAutomation::mplusModeler()
#' @return an mplusObject with a $results section
#'
#' @export
#' @family Mplus helpers
#' @author Sean Ho <anchor@seanho.com>
#'
mp_run <- function(.obj, name = "mp", run = 1, ...) {
  out <- utils::capture.output({
    suppressMessages({
      res <- MplusAutomation::mplusModeler(
        .obj, dataout = paste0(name, ".dat"), run = run, ...)
    })
  })
  res$results$stdout <- out
  res$results$tech4 <- parse_tech4(readLines(paste0(name, ".out")))
  res
}
