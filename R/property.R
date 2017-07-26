
#' Check a property holds for all generated values.
#'
#' True example:
#' > forall ( list (a = vec( gen.sample(1:100)), b = vec(gen.sample(1:100))), function(x) { identical ( c(rev(x$b), rev(x$a)), rev( c(x$a, x$b ))) } )
#' [1] TRUE
#'
#' False example showing minimum shrink:
#' > forall ( vec( gen.sample(1:100)), function(x) { identical ( rev(x), x ) } )
#' Falsifiable after 1 tests, and 5 shrinks
#' Predicate is falsifiable
#'
#' Counterexample:
#' [1] 1 2
forall <- function ( generator, property, tests = 100, size = 5, shrink.limit = 1000) {
  # R doesn't have good tail call optimisation.
  # Hence, loops.
  for (i in 1:tests) {
    trees  <- unfoldgenerator( generator, size )
    tree   <- tree.traverse ( trees )
    value  <- tree$root

    # Run the test
    if ( ! testable_success( run.prop(property, value ))) {
      # The test didn't pass. Find the smallest
      # counterexample we can ( by shrinking ).
      counterexample <- find.smallest( tree, property, shrink.limit, 0 )

      cat ( paste("Falsifiable after", i, "tests, and",  counterexample$shrinks, "shrinks\n") )

      # Print the message which comes with the counterexample.
      print ( run.prop(property, counterexample$smallest ) )

      # Show the counterexamples
      cat ( "Counterexample:\n")
      print ( counterexample$smallest )

      # Exit the loop with failure
      return(invisible(F))
    }
  }
  cat( paste("Passed after", tests, "tests\n") )
  return(T)
}

#' Turn a generator into a tree
#' and a list of generators into a list of trees.
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
    # values in the list are actually trees.
    lowered <- lapply ( trees, tree.traverse )

    # Reduce the list of trees into a single tree by folding
    # over it with a bind (this is essentially a foldM).
    # with an list accumulating in the fold.
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
