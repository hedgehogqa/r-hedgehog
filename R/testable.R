#' Constructor function for a testable value.
#'
#' These are pretty much just a bool with
#' a few values for exceptions and warnings.
#'
#' Partially derived from testthat (MIT licenced).
testable <- function(type, message = NULL, srcref = NULL) {
  type <- match.arg(type, c("success", "failure", "error", "warning"))

  structure(
    list(
      message = message,
      srcref = srcref
    ),
    class = c(
      paste0("testable_", type),
      "testable"
    )
  )
}

#' R ad-hoc testable class
as.testable <- function(x, ...) UseMethod("as.testable", x)

as.testable.default <- function(x, ..., srcref = NULL) {
  stop("Don't know how to convert '", paste(class(x), collapse = "', '"),
       "' to testable.", call. = FALSE)
}

as.testable.testable <- function(x, ..., srcref = NULL) {
  if (is.null(x$srcref)) {
    x$srcref <- srcref
  }
  x
}

as.testable.logical <- function(x, message, ..., srcref = NULL, info = NULL) {
  type <- if (x) "success" else "failure"
  testable(type, "Predicate is falsifiable\n")
}

as.testable.error <- function(x, ..., srcref = NULL) {
  error <- x$message

  msg <- gsub("Error.*?: ", "", as.character(error))

  # Need to remove trailing newline from error message to be consistent
  # with other messages
  msg <- gsub("\n$", "", msg)

  testable("error", msg, srcref)
}

as.testable.warning <- function(x, ..., srcref = NULL) {
  msg <- x$message
  testable("warning", msg, srcref)
}

testable_type <- function(exp) {
  stopifnot(is.testable(exp))
  gsub("^testable_", "", class(exp)[[1]])
}

testable_success <- function(exp) {
  testable_type(exp) == "success"
}

testable_failure <- function(exp) {
  testable_type(exp) == "failure"
}

testable_error <- function(exp) {
  testable_type(exp) == "error"
}

testable_warning <- function(exp) {
  testable_type(exp) == "warning"
}

testable_broken <- function(exp) {
  testable_failure(exp) || testable_error(exp)
}
testable_ok <- function(exp) {
  testable_type(exp) %in% c("success", "warning")
}

#' @rdname testable
#' @param x object to test for class membership
is.testable <- function(x) inherits(x, "testable")

print.testable <- function(x, ...) cat(format(x), "\n")

format.testable_success <- function(x, ...) {
  "Success"
}

format.testable_error <- function(x, ...) {
  paste("Exception encountered in property:", x$message, sep = "\n")
}

format.testable <- function(x, ...) {
  x$message
}
