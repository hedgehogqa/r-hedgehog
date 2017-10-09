#' Hedgehog property test
#'
#' Check a property holds for all generated values.
#'
#' The generator used can be defined flexibly, in that
#' one can pass in a list of generators, or even nest
#' generators and constant values deeply into the gen
#' argument and the whole construct will be treated
#' as a generator.
#'
#' @param generator a generator or list of generators
#'   (potentially nested) to use for value testing.
#' @param property a function which takes a value from
#'   from the generator and tests some predicated against
#'   it.
#' @param tests the number of tests to run
#' @param size.limit the max size used for the generators
#' @param shrink.limit the maximum number of shrinks to
#'   run when shrinking a value to find the smallest
#'   counterexample.
#' @param discard.limit the maximum number of discards to
#'   permit when running the property.
#' @param curry whether to curry the arguments passed
#'   to the property, and use do.call to use the list
#'   generated as individual arguments.
#'   When curry is on, the function arity should be the
#'   same as the length of the generated list.
#'   Defaults to \code{T} if the input is a list.
#'
#' @importFrom utils capture.output
#' @importFrom testthat succeed
#' @importFrom testthat fail
#'
#' @examples
#' test_that( "Reverse and concatenate symmetry",
#'   forall( list( as = gen.c( gen.element(1:100) )
#'               , bs = gen.c( gen.element(1:100) ))
#'         , function( as, bs )
#'             expect_identical ( rev(c(as, bs)), c(rev(bs), rev(as)))
#'   )
#' )
#'
#' # False example showing minimum shrink:
#' \dontrun{
#' test_that( "Reverse is identity",
#'   forall ( gen.c( gen.element(1:100)), function(x) { expect_identical ( rev(x), c(x) ) } )
#' )
#' }
#' # Falsifiable after 1 tests, and 5 shrinks
#' # Predicate is falsifiable
#'
#' # Counterexample:
#' # [1] 1 2
#'
#' @export
forall <- function( generator
                  , property
                  , tests = getOption("hedgehog.tests", 100)
                  , size.limit = getOption("hedgehog.size", 50)
                  , shrink.limit = getOption("hedgehog.shrinks", 100)
                  , discard.limit = getOption("hedgehog.discards", 100)
                  , curry = identical(class(generator), "list")) {

  # Starting size parameter
  size     <- 1
  # Counters for the forall loop
  discards <- 0
  test     <- 0

  # R doesn't have good tail call optimisation, hence the while loop.
  while ( test < tests ) {
    # Check if we have discarded too many trials
    if (discards >= discard.limit)
      return (fail( message = "discard limit reached"))

    # Run the test with error handling
    tree   <- gen.run( generator, size )
    run    <- run.prop( property, tree$root, curry )

    # Check for a discard. If we're discarding, we won't increment the test
    # counter, but do increment the discard counter and restart the loop.
    if ( run$discard ) {
      discards <- discards + 1
      next
    }

    # Increment the test counter.
    test <- test + 1

    if ( !run$ok ) {
      # The test didn't pass. Find the smallest
      # counterexample we can ( by shrinking ).
      counterexample <- find.smallest( tree, property, curry, shrink.limit, 0, discards, discard.limit )

      # Rerun the smallest counterexample to grab the specific error
      test_error     <- run.prop( property, counterexample$smallest, curry )$test_error

      # Make a nice error report for the user.
      report_        <- capture.output(print(
                          report( test, counterexample$shrinks, test_error, counterexample$smallest )
                        ))
      # Exit the loop with failure, this will be picked up
      # by testthat and displayed nicely.
      return (fail( message = paste(report_, collapse = "\n") ))
    }

    # Increment the size or reset it if we've
    # reached the size limit
    if (size < size.limit) {
      size <- size + 1
    } else {
      size <- 1
    }
  }

  succeed(message = paste("Passed after", tests, "tests\n"))
}

#' Discard a test case
#' @export
discard <- function() {
  cond <- structure(list(), class = c("discard", "condition"))
  stop(cond)
}

# Search through the trees to find the smallest value we can
# which still fails the test.
#
# @param tree the tree to search through for the smallest
#   value which fails the test.
# @param property the property which is failing
# @param curry whether to pass only one
#   to the property, or use do.call to use the list
#   generated as individual arguments.
# @param shrink.limit the limit to how far we will try and shrink
# @param shrinks the current number of shrinks
find.smallest <- function ( tree, property, curry, shrink.limit, shrinks, discards, discard.limit ) {

  # The smallest value so far.
  point    <- list ( smallest = tree$root, shrinks = shrinks )

  # If we've reached the shrink or discard limit return.
  if (shrinks >= shrink.limit || discards >= discard.limit)
    return( point )

  # We're looking further, so force the lazy tree's branches.
  children <- tree$children()

  # Search the branches of the tree.
  # This is a recursive depth first search, which assumes that no
  # branch in a child will fail if the root doesn't as well.
  smaller <- Find ( function( child ) {
    # Run the child.
    trial <- run.prop( property, child$root, curry )
    # If it's a discard it's not a failing case.
    # But we don't want to run too many, so increase
    # a discard counter.
    if ( trial$discard ) {
      discards <- discards + 1
      F
    } else {
      !( trial$ok )
    }
  }, children )

  # If there was nothing found, the the root must be the smallest
  # for this tree; otherwise, recurse into the child found.
  if (is.null(smaller)) {
    point
  } else {
    find.smallest( smaller, property, curry, shrink.limit, shrinks + 1, discards, discard.limit )
  }
}
