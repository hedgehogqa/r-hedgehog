#' Constructor for a report.
#'
#' @param g A function from size to tree
report <- function ( tests, shrinks, message, counterexample ) {
  structure (
    list (
      tests = tests
    , shrinks = shrinks
    , message = message
    , counterexample = counterexample
    ), class = "report"
  )
}

#' Print information about a generator.
#'
#' @param g A generator
print.report <- function ( report ) {
    # Print a nice message for the user.
    # This could be moved into a fully fledged
    # report type.
    cat ( paste("\nFalsifiable after", report$tests, "tests, and",  report$shrinks, "shrinks\n") )

    # Print the message which comes with the counterexample.
    print ( report$message )

    # Show the counterexamples
    cat ( "Counterexample:\n")
    print ( report$counterexample )
}
