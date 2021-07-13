
#' Lazy rose trees
#'
#' A rose tree is a type of multibranch tree.
#' This is hedgehog's internal implementation
#' of a lazy rose tree.
#'
#' In general, one should not be required to
#' use any of the functions from this module
#' as the combinators in the gen module
#' should be expressive enough (if they're
#' not raise an issue).
#'
#' @param root
#'   the root of the rose tree
#' @param children_
#'   a list of children for the tree.
#' @param f
#'   a function for mapping, binding, or applying
#' @param x
#'   a tree to map or bind over
#' @param y
#'   a tree to map or bind over
#' @param a
#'   a value to unfold from
#' @param shrink
#'   a shrinking function
#' @param trees
#'   a tree, or list or structure potentially
#'   containing trees to turn into a tree of
#'   said structure.
#'
#' @name tree
NULL

#' @rdname tree
tree <- function( root, children_ = list() ) {
  eval.children <- NULL
  structure(
    list(
      root = root,
      children = function() {
        if (is.null(eval.children))
          eval.children <<- force(children_)
        eval.children
      }
    ), class = "hedgehog.internal.tree")
}

#' @rdname tree
tree.map <- function ( f, x ) {
  tree.check ( x )
  y <- f ( x$root )
  tree (
    root      = y
  , children_ = lapply( x$children(), function( xx ) tree.map(f,xx) )
  )
}

#' @rdname tree
tree.bind <- function ( f, x ) {
  tree.check ( x )
  y <- f ( x$root )
  tree.check ( y )
  tree (
    root      = y$root
  , children_ =
      unlist(
        list(
          lapply( x$children(), function(xx) tree.bind(f, xx) )
          , y$children()
        ), recursive = F
      )
  )
}

#' @rdname tree
tree.liftA2 <- function ( f, x, y ) {
  tree.check ( x )
  tree.check ( y )
  z <- f ( x$root, y$root )
  tree (
    root      = z
  , children_ =
      unlist(
        list(
          lapply( x$children(), function(xx) tree.liftA2(f, xx, y) )
        , lapply( y$children(), function(yy) tree.liftA2(f, x, yy) )
        ), recursive = F
      )
  )
}

#' @rdname tree
tree.expand <- function ( shrink, x ) {
  tree.check ( x )
  node         <- x$root
  children     <- x$children
  tree( root = node,
        children_ =
          unlist(
            list (
              lapply( children(), function( child ) tree.expand( shrink, child ) )
            , tree.unfoldForest( shrink, node )
            ), recursive = F
          )
       )
}

#' @rdname tree
tree.unfold <- function ( shrink, a ) {
  tree (
    root     = a
  , children_ = tree.unfoldForest( shrink, a )
  )
}

#' @rdname tree
tree.unfoldForest <- function ( shrink, a ) {
  lapply ( shrink ( a ), function( y ) { tree.unfold ( shrink, y )  } )
}

#' @rdname tree
tree.sequence <- function ( trees ) {
  if (inherits( trees,"hedgehog.internal.tree" )) {
    # We have a single tree.
    # This is all we need so we can return it.
    trees
  } else if ( is.list( trees ) ) {
    # Lists can contain trees.
    # Collect information about the list (its attributes).
    info    <- attributes(trees)

    # Recursively call tree.traverse on the list so all
    # values in the list are now actually trees.
    lowered <- lapply ( trees, tree.sequence )

    # Reduce the list of trees into a single tree by folding
    # over it with a liftA2 with an list accumulating in the
    # fold.
    #
    # /Note/ This is *independent* (or applicative) shrinking,
    # so shrinks can alternate between the positions in the
    # list.
    merged  <- Reduce (
        function (acc, t) tree.liftA2 ( snoc, acc, t )
      , lowered
      , tree(list())
      )

    # The original list had structure to it (attributes...).
    # We have made a tree containing lists of the same shape, but
    # they don't currently have the attributes of the original.
    # We can make the "structure" the same by mapping the list's
    # attributes to all values in the tree.
    tree.map( function(m) { attributes(m) <- info; m }, merged )
  } else {
    # The value is not a list or a tree.
    # We can embed it into a pure tree (one having no shrinks).
    # and return it.
    tree ( trees )
  }
}

#' Creating trees of lists
#'
#' Build a tree of a list, potentially
#' keeping hold of an internal state.
#'
#' @param num
#'   the length of the list in the tree
#' @param ma
#'   a function which (randomly) creates
#'   new tree to add to the list
#' @param s
#'   a state used when replicating to
#'   keep track of.
#' @param ...
#'   extra arguments to pass to the tree
#'   generating function
#'
#' @name tree.replicate
NULL

#' @rdname tree.replicate
tree.replicate <- function (num, ma) {
  trees <- replicate(num, ma(), simplify = FALSE)
  tree.interleave(trees)
}


# All ways a list can be split
#
# > splits(c(1,2,3]))
# > ==
# > [ ([], 1, [2, 3])
#   , ([1], 2, [3])
#   , ([1, 2], 3, [])
#   ]
#
tree.splits <- function(xs) {
  total <- length(xs)
  lapply(seq_along(xs), function(x) {
    list(
      inits = xs[seq(from = 1,     length.out = x - 1)]
    , focus = xs[[x]]
    , tails = xs[seq(from = x + 1, length.out = total - x)]
    )
  })
}

tree.shrink_one <- function(trees) {
  unlist(
    lapply(tree.splits(trees), function(split) {
      lapply(split$focus$children(), function(focus) {
        tree.interleave(c(split$inits, list(focus), split$tails))
      })
    })
  , recursive = FALSE)
}

tree.interleave <- function (trees) {
  tree(
    root = lapply(trees, function(tree) { tree$root })
  , children_ = tree.shrink_one(trees)
  )
}


#' @rdname tree.replicate
tree.replicateS <- function ( num, ma, s, ...) {
  if ( num <= 0) {
    tree ( list() )
  } else {
    tree.bind (
      function(a) {
        tree.map (
            function(as) cons(a[[2]], as )
          , tree.replicateS ( num - 1, ma, a[[1]], ... )
        )
      }
    , do.call(ma, cons(s, list(...))) )
  }
}

tree.check <- function ( x ) {
  if (!inherits( x, "hedgehog.internal.tree" ))
    stop(paste0('`',x,'`, of class `', class(x),'`, is not a tree'))
}
