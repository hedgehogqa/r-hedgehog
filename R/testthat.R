
##################################
# Testthat Compatibility helpers #
##################################

# We have to break into testthat a bit, as within the
# forall we can't fail on a single expectation, as we
# wouldn't then be able to shrink the result and find
# the smallest test case which generates it.
#
# This module contains the savage and duplicated code
# we require.


# Run a property (with test case error handling), and turn it
# into an expectation.
#
# Error handling code from testthat more or less.
#
# @param property the property to test
# @param arguments the generated arguments to the property.
# @param curry whether to pass only one argument
#   to the property, or use do.call to use the list
#   generated as individual arguments.
# @param ret.error to return the error or a success indicator
#
# @return the error and a bool indicating success.
run.prop <- function ( property, arguments, curry ) {
  arguments  <- if ( curry ) arguments else list ( arguments )
  test_error <- NULL
  handled    <- F
  discard    <- F
  ok         <- T

  register_expectation <- function(e) {
    e           <- as.expectation(e)
    test_error <<- e
    ok         <<- ok && expectation_ok(e)
  }

  handle_error <- function(e) {
    handled    <<- TRUE
    register_expectation(e)
    e$handled   <- TRUE
    test_error <<- e
  }

  handle_fatal <- function(e) {
    handled <<- TRUE
    # Quote: Hadley
    # > Error caught in handle_error() has precedence
    if (!is.null(test_error)) {
      if (isTRUE(test_error$handled)) {
        return()
      }
    }
    register_expectation(e)
  }

  handle_expectation <- function(e) {
    handled <<- TRUE
    register_expectation(e)
    tryInvokeRestart("muffle_expectation") # testthat >= 3.3.0
    tryInvokeRestart("continue_test")      # testthat < 3.3.0
  }

  handle_warning <- function(e) {
    handled <<- TRUE
    register_expectation(e)
    invokeRestart("muffleWarning")
  }

  handle_message <- function(e) {
    handled <<- TRUE
    invokeRestart("muffleMessage")
  }

  handle_discard <- function(e) {
    handled <<- TRUE
    discard <<- TRUE
  }

  tryCatch(
    withCallingHandlers({
          do.call( property, arguments )
          if (!handled)
            testthat::fail("No expectations in property")
        }
      , expectation = handle_expectation
      , warning     = handle_warning
      , message     = handle_message
      , discard     = handle_discard
      , error       = handle_error
    )
  , error = handle_fatal
  )
  
  list ( discard = discard, ok = ok, test_error = test_error)
}

# From testthat.
expectation_type <- function(exp) {
  stopifnot(testthat::is.expectation(exp))
  gsub("^expectation_", "", class(exp)[[1]])
}

# From testthat.
expectation_ok <- function(exp) {
  expectation_type(exp) %in% c("success", "warning")
}

# From testthat more or less.
as.expectation <- function(x, ...) UseMethod("as.expectation", x)
as.expectation.default <- function(x, ..., srcref = NULL) {
  stop("Don't know how to convert '", paste(class(x), collapse = "', '"),
       "' to expectation.", call. = FALSE)
}
as.expectation.expectation <- function(x, ..., srcref = NULL) {
  if (is.null(x$srcref)) {
    x$srcref <- srcref
  }
  x
}
as.expectation.logical <- function(x, message, ..., srcref = NULL, info = NULL) {
  type <- if (x) "success" else "failure"
  testthat::new_expectation(type, paste(message, info, sep = "\n"), srcref = srcref)
}
as.expectation.error <- function(x, ..., srcref = NULL) {
  error <- x$message
  msg <- gsub("Error.*?: ", "", as.character(error))
  msg <- gsub("\n$", "", msg)
  testthat::new_expectation("error", msg, srcref = srcref)
}
as.expectation.warning <- function(x, ..., srcref = NULL) {
  msg <- x$message
  testthat::new_expectation("warning", msg, srcref = srcref)
}
as.expectation.skip <- function(x, ..., srcref = NULL) {
  error <- x$message
  msg <- gsub("Error.*?: ", "", as.character(error))
  testthat::new_expectation("skip", msg, srcref = srcref)
}
