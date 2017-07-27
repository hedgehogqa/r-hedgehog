
#' A lazy, purely functional rose tree
tree <- function( root, children_ = list() ) {
  eval.children <- NULL
  structure(
    list(
      root = root,
      children = function() {
        if (is.null(eval.children)) eval.children <<- force(children_)
        eval.children
      }
    ), class = "tree")
}

#' Map a function over values of the tree
tree.map <- function ( f, x ) {
  y <- f ( x$root )
  tree (
    root     = y
  , children = lapply( x$children(), function( xx ) tree.map(f,xx) )
  )
}

#' Bind a function over values of the tree
tree.bind <- function ( f, x ) {
  y <- f ( x$root )
  tree (
    root     = y$root
  , children = unlist(
                list(
                  lapply( x$children(), function(xx) tree.bind(f, xx) )
                  , y$children()
                ), recursive = F
               )
  )
}

#' Expand a tree with a shrinking function
tree.expand <- function ( shrink, t ) {
  node         <- t$root
  children     <- t$children
  tree ( root = node,
         children = unlist(
                      list (
                        lapply( children(), function( child ) tree.expand( shrink, child ) )
                      , tree.unfoldForest( shrink, node )
                      ), recursive = F
                    )
       )
}

#' Generate a tree while there are values in the shrink.
tree.unfold <- function ( shrink, a ) {
  tree (
    root     = a
  , children = tree.unfoldForest( shrink, a )
  )
}

#' Generate a set of tree while there are values in the shrink.
tree.unfoldForest <- function ( shrink, x ) {
  lapply ( shrink ( x ), function( y ) { tree.unfold ( shrink, y )  } )
}

#' Create a tree of a vector from a generator for a value.
tree.replicateM <- function ( num, ma ) {
  if ( num <= 0) {
    tree ( list() )
  } else {
    tree.bind (
      function(a) {
        tree.bind (
          function(as)
            tree( unlist( list( list(a), as ), recursive = F))
          , tree.replicateM ( num - 1, ma )
        )
      }
    , ma() )
  }
}
