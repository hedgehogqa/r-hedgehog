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
#' @param size the max size used for the generators
#' @param shrink.limit the maximum number of shrinks to
#'   run when shrinking a value to find the smallest
#'   counterexample.
#' @param curry whether to curry the arguments passed
#'   to the property, and use do.call to use the list
#'   generated as individual arguments.
#'
#' @importFrom utils capture.output
#' @importFrom testthat fail
#' @importFrom testthat succeed
#'
#' @examples
#' test_that( "Reverse of reverse is identity",
#'   forall( list( as = gen.c( gen.sample(1:100) )
#'               , bs = gen.c( gen.sample(1:100) ))
#'         , function( as, bs )
#'             expect_identical ( rev(c(as, bs)), c(rev(bs), rev(as)))
#'   )
#' )
#' # TRUE
#'
#' # False example showing minimum shrink:
#' \dontrun{
#' test_that( "Reverse is identity",
#'   forall ( gen.c( gen.sample(1:100)), function(x) { expect_identical ( rev(x), c(x) ) } )
#' )
#' }
#' # Falsifiable after 1 tests, and 5 shrinks
#' # Predicate is falsifiable
#'
#' # Counterexample:
#' # [1] 1 2
#'
#' @export
forall <- function ( generator, property, tests = 100, size = 10, shrink.limit = 1000, curry = identical(class(generator), "list")) {
  # R doesn't have good tail call optimisation, hence, loops.
  for (i in 1:tests) {
    tree   <- gen.run ( generator, size )
    value  <- tree$root

    # Run the test
    if ( !run.prop(property, value, curry)$ok ) {
      # The test didn't pass. Find the smallest
      # counterexample we can ( by shrinking ).
      counterexample <- find.smallest( tree, property, curry, shrink.limit, 0 )

      # Print the message which comes with the counterexample.
      test_error     <- run.prop( property, counterexample$smallest, curry )$test_error

      # Print a nice message for the user.
      rep            <- capture.output( print (
                          report ( i, counterexample$shrinks, test_error, counterexample$smallest ))
                        )
      # Exit the loop with failure, this will be picked up
      # by testthat and displayed nicely.
      return (fail( message = paste(rep, collapse = "\n") ))
    }
  }

  succeed(message = paste("Passed after", tests, "tests\n"))
}

discard <- function(message = NULL) {
  cond <- structure(list(message = message), class = c("discard", "condition"))
  stop(cond)
}

# Search through the trees to find the smallest value we can
# which still fails the test.
#
# @param tree the tree to search through for the smallest
#   value which fails the test.
# @param property the property which is failing
# @param single.argument whether to pass only one
#   to the property, or use do.call to use the list
#   generated as individual arguments.
# @param shrink.limit the limit to how far we will try and shrink
# @param shrinks the current number of shrinks
find.smallest <- function ( tree, property, single.argument, shrink.limit, shrinks ) {

  # The smallest value so far.
  point    <- list ( smallest = tree$root, shrinks = shrinks )

  # If we've reached the shrink counter return.
  if (shrinks >= shrink.limit)
    return( point )

  # We're looking further, so force the lazy tree's branches.
  children <- tree$children()

  # Search the branches of the tree.
  # This is a recursive depth first search, which assumes that no
  # branch in a child will fail if the root doesn't as well.
  smaller <- Find ( function( child ) {
    !(run.prop( property, child$root, single.argument)$ok)
  }, children )

  # If there was nothing found, the the root must be the smallest
  # for this tree; otherwise, recurse into the child found.
  if (is.null(smaller)) {
    point
  } else {
    find.smallest( smaller, property, single.argument, shrink.limit, shrinks + 1 )
  }
}
