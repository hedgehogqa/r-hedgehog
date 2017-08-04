utils::globalVariables(c("hedgehog.internal"))

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
#' @importFrom progress progress_bar
#'
#' @examples
#' forall( list( as = gen.c( gen.sample(1:100) )
#'             , bs = gen.c( gen.sample(1:100) ))
#'       , function( as, bs )
#'           identical ( rev(c(as, bs)), c(rev(bs), rev(as)))
#' )
#' # TRUE
#'
#' # False example showing minimum shrink:
#' forall ( gen.c( gen.sample(1:100)), function(x) { identical ( rev(x), x ) } )
#' # Falsifiable after 1 tests, and 5 shrinks
#' # Predicate is falsifiable
#'
#' # Counterexample:
#' # [1] 1 2
#'
#' @export
forall <- function ( generator, property, tests = 100, size = 10, shrink.limit = 1000, curry = identical(class(generator), "list")) {

  # Start a progress bar to track how far we have come.
  pb <- progress_bar$new(
          format = "  Running tests [:bar] :current of :total",
          total = tests, clear = FALSE, width= 60 )

  # R doesn't have good tail call optimisation, hence, loops.
  for (i in 1:tests) {
    trees  <- unfoldgenerator( generator, size )
    tree   <- tree.traverse ( trees )
    value  <- tree$root
    pb$tick()

    # Run the test
    if ( ! testable_success( run.prop(property, value, curry))) {
      # The test didn't pass. Find the smallest
      # counterexample we can ( by shrinking ).
      counterexample <- find.smallest( tree, property, curry, shrink.limit, 0 )

      # Print the message which comes with the counterexample.
      message <- run.prop( property, counterexample$smallest, curry )

      # Print a nice message for the user.
      print ( report ( i, counterexample$shrinks, message, counterexample$smallest ) )

      # If we're inside a runner, update the error tally.
      if (exists("hedgehog.internal")) {
        hedgehog.internal$failures <<- hedgehog.internal$failures + 1
      }

      # Exit the loop with failure
      return(invisible(F))
    }
  }
  cat( paste("Passed after", tests, "tests\n") )

  # If we're inside a runner, update the success tally.
  if (exists("hedgehog.internal")) {
    hedgehog.internal$successes <<- hedgehog.internal$successes + 1
  }
  invisible(T)
}

#' Turn a generator into a tree and a list of generators
#' into a list of trees.
#' Non-generator and list values are passed along
#' as is.
#' Generators can use the random number generator when
#' creating their trees.
#'
#' @param generator the generator ( or list of generators )
#' @param size the size parameter to use
unfoldgenerator <- function ( generator , size ) {
  if (inherits( generator,"gen")) {
    # A generator can be run and turned into
    # a tree
    generator$unGen(size)
  } else if ( is.list(generator) ) {
    # Lists can contain a generator.
    lapply ( generator, function(g) unfoldgenerator(g, size) )
  } else {
    # Static values are passed through as is
    generator
  }
}

#' Search through the trees to find the smallest value we can
#' which still fails the test.
#'
#' @param tree the tree to search through for the smallest
#'   value which fails the test.
#' @param property the property which is failing
#' @param single.argument whether to pass only one
#'   to the property, or use do.call to use the list
#'   generated as individual arguments.
#' @param shrink.limit the limit to how far we will try and shrink
#' @param shrinks the current number of shrinks
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
    !testable_success( run.prop( property, child$root, single.argument ))
  }, children )

  # If there was nothing found, the the root must be the smallest
  # for this tree; otherwise, recurse into the child found.
  if (is.null(smaller)) {
    point
  } else {
    find.smallest( smaller, property, single.argument, shrink.limit, shrinks + 1 )
  }
}

# Turn the arguments into a list for our function.
# If the class is *just* list, then we will allow
# all named or unnamed arguments to be passed to
# the property.
argument.list <- function(arguments) {
 if (identical(class(arguments), "list"))
    arguments else list( arguments )
}

# Run a property (with error handling), and turn it
# into a testable.
# @param property the property to test
# @param arguments the generated arguments to the property.
# @param single.argument whether to pass only one
#   to the property, or use do.call to use the list
#   generated as individual arguments.
run.prop <- function ( property, arguments, curry ) {
  arguments <- if ( curry ) arguments else list ( arguments )
  tryCatch( as.testable ( do.call( property, arguments) ),
    warning = function(w) as.testable(w),
    error = function(e) as.testable(e))
}
