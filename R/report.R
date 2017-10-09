# Constructor for a report.
#
# @param tests
#   the test on which the failure occurred
# @param shrinks
#   how many shrinks were performed
# @param message
#   the error message to print
# @param counterexample
#   the smallest counterexample to print
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

#' @export
print.report <- function ( x, ... ) {
    # Print a nice message for the user.
    # This could be moved into a fully fledged
    # report type.
    cat ( paste("Falsifiable after", x$tests, "tests, and",  x$shrinks, "shrinks\n") )

    # Print the message which comes with the counterexample.
    print ( x$message )

    # Show the counterexamples
    cat ( "Counterexample:\n")
    print ( x$counterexample )
}
