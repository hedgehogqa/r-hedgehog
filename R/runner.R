
#' Hedgehog property runner
#'
#' Run all hedgehod property tests within a directory
#'
#' @param path directory to search for test files
#' @param pattern regex pattern to use when searching
#'   for test files.
#'
#' @export
run.tests <- function(path, pattern = "\\.[rR]$") {

  # Evil global variable to track how many success
  # and failure cases we have
  hedgehog.internal <<-
    list( successes = 0, failures = 0 )

  # Grab all our test files and run them
  files <- normalizePath(sort(dir(path, pattern, full.names = TRUE)))
  lapply(files, source)

  successes <- hedgehog.internal$successes
  failures  <- hedgehog.internal$failures

  rm( hedgehog.internal, envir = globalenv() )

  if (failures > 0 || successes == 0) {
    stop(paste("Hedgehog tests failed with"
              , failures
              , "failures and"
              , successes
              , "successes"))
  } else {
    paste("Hedgehog tests passed with"
         , successes
         , "successes")
  }
}

#' Label a property
#'
#' Give a description to a property
#'
#' @param A short textual description of the property
#' @param A property (specified with forall).
#'
#' @export
property <- function(desc, prop) {
  cat(paste("---",desc, "---\n"))
  prop
  cat("\n")
}
