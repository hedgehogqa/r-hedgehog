# Constructor for a report.
#
# @param tests
#   the test on which the failure occurred
# @param shrinks
#   how many shrinks were performed
# @param messages
#   a list of the error messages to print
# @param counterexample
#   the smallest counterexample to print
report <- function ( tests, shrinks, messages, counterexample ) {
  structure (
    list (
      tests = tests
    , shrinks = shrinks
    , messages = messages
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

    # Print the messages which come with the counterexample.
    for (message in x$messages) {
      print ( message )
    }

    # Show the counterexamples
    cat ( "Counterexample:\n")
    print ( x$counterexample )
}
