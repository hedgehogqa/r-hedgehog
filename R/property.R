
#' Hedgehog property test
#'
#' Check a property holds for all generated values.
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
#'
#' @examples
#' forall (
#'   list ( a = vec( gen.sample(1:100))
#'        , b = vec(gen.sample(1:100))
#'        )
#'   , function(x) {
#'       identical ( c(rev(x$b), rev(x$a)), rev( c(x$a, x$b )))
#'     }
#' )
#' # TRUE
#'
#' # False example showing minimum shrink:
#' forall ( vec( gen.sample(1:100)), function(x) { identical ( rev(x), x ) } )
#' # Falsifiable after 1 tests, and 5 shrinks
#' # Predicate is falsifiable
#'
#' # Counterexample:
#' # [1] 1 2
#'
#' @export
forall <- function ( generator, property, tests = 100, size = 5, shrink.limit = 1000) {

  # R doesn't have good tail call optimisation, hence, loops.
  for (i in 1:tests) {
    trees  <- unfoldgenerator( generator, size )
    tree   <- tree.traverse ( trees )
    value  <- tree$root

    # Run the test
    if ( ! testable_success( run.prop(property, value ))) {
      # The test didn't pass. Find the smallest
      # counterexample we can ( by shrinking ).
      counterexample <- find.smallest( tree, property, shrink.limit, 0 )

      # Print a nice message for the user.
      # This could be moved into a fully fledged
      # report type.
      cat ( paste("Falsifiable after", i, "tests, and",  counterexample$shrinks, "shrinks\n") )

      # Print the message which comes with the counterexample.
      print ( run.prop(property, counterexample$smallest ) )

      # Show the counterexamples
      cat ( "Counterexample:\n")
      print ( counterexample$smallest )

      # If we're inside a runner, update the error tally.
      if (exists("hedgehog.summary")) {
        hedgehog.summary$failures <<- hedgehog.summary$failures + 1
      }

      # Exit the loop with failure
      return(invisible(F))
    }
  }
  cat( paste("Passed after", tests, "tests\n") )

  # If we're inside a runner, update the success tally.
  if (exists("hedgehog.summary")) {
    hedgehog.summary$successes <<- hedgehog.summary$successes + 1
  }
  return(T)
}

#' Turn a generator into a tree and a list of generators
#' into a list of trees.
#' Non-generator and list values are passed along
#' as is.
#' Generators can use the random number generator when
#' creating their trees.
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

#' Recursively turn lists of trees into a single tree.
tree.traverse <- function ( trees ) {
  if (inherits( trees,"tree")) {
    # We have a single tree.
    # This is all we need so we can return it.
    trees
  } else if ( is.list( trees ) ) {
    # Lists can contain trees.
    # Collect information about the list (its attributes).
    info    <- attributes(trees)

    # Recursively call tree.traverse on the list so all
    # values in the list are now actually trees.
    lowered <- lapply ( trees, tree.traverse )

    # Reduce the list of trees into a single tree by folding
    # over it with a bind (this is essentially a foldM).
    # with an list accumulating in the fold.
    #
    # /Note/ This may change to allow *applicative* shrinking.
    merged  <- Reduce ( function (acc, t) {
      tree.bind (
        function(a) {
          tree.bind (
            function(as)
              tree( unlist( list ( a, list(as) ) , recursive = F))
            , t
          )
        }
      , acc )
    }, lowered, tree (list()) )

    # The original list had structure to it (class, attributes...).
    # We have made a tree containing lists of the same shape, but
    # they don't currently have the attributes of the original.
    # We can made the "structure" the same by mapping the list's
    # attributes to all values in the tree.
    tree.map( function(m) { attributes(m) <- info; m }, merged )
  } else {
    # The value is not a list or a tree.
    # We can embed it into a pure tree (one having no shrinks).
    # and return it.
    tree ( trees )
  }
}

#' Search through the trees to find the smallest value we can
#' which still fails the test.
find.smallest <- function ( tree, property, shrink.limit, shrinks ) {

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
  smaller <- Find ( function( child ) { ! testable_success( run.prop( property, child$root )) }, children )

  # If there was nothing found, the the root must be the smallest
  # for this tree; otherwise, recurse into the child found.
  if (is.null(smaller)) {
    point
  } else {
    find.smallest( smaller, property, shrink.limit, shrinks + 1 )
  }
}

#' Run a property (with error handling), and turn it
#' into a testable.
run.prop <- function ( property, arguments ) {
  tryCatch( as.testable ( property(arguments) ),
    warning = function(w) as.testable(w),
    error = function(e) as.testable(e))
}
