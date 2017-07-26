#' Constructor for a generator.
#'
#' We're lazy and are using R's global seed.
#' A gen essentially a function which takes
#' a size parameter and returns a lazy tree.
gen <- function ( x ) {
  structure ( list ( unGen = x ), class = "gen" )
}

#' Sample from a generator.
gen.example <- function ( g, size = 5 ) {
  tree <- g$unGen(size)
  tree$root
}

#' Generate a structure
#'
#' If you can create an object with `structure`,
#' you should be able to generate an object with
#' this function from a generator or list of
#' generators.
#'
#' Examples:
#' > g <- gen.structure( vec.of(6, integral (1, 30)), dim = 3:2)
#' > gen.example ( g )
#'      [,1] [,2]
#' [1,]   21   11
#' [2,]    2   10
#' [3,]    5   27
#'
#' > g <- gen.structure( list ( vec.of(4, integral(2, 10)), vec.of(4, integral(2,10))), names = c("a","b"), class = "data.frame", row.names = c("1", "2", "3", "4" ))
#' > gen.example ( g )
#'   a b
#' 1 2 7
#' 2 5 5
#' 3 8 9
#' 4 2 9
gen.structure <- function ( x, ... ) {
  gen ( function( size ) {
    trees  <- unfoldgenerator(x, size)
    tree   <- tree.traverse ( trees )
    tree.map( function(m) { attributes(m) <- list(...); m }, tree )
  })
}

#' Use one generator as a precondition for another.
#'
#' This is a monadic bind with arguments reversed.
#'
#' Example:
#' > g <- gen.with( chooseOf(2, 100), function(x) vec.of( x, chooseOf(1,10)))
#' > gen.sample ( g )
#' [1] 8 6 2 7 5 4 2 2 4 6 4 6 6 3 6 7 8 5 4 6
gen.with <- function ( g, f ) {
  gen ( function ( size ) {
    tree <- g$unGen(size)
    tree.bind ( function(x) { f(x)$unGen(size) }, tree )
  })
}

#' Monadic bind for a generator.
gen.bind <- function ( f, g ) {
  gen.with( g, f )
}

#' Helper for making a gen with a size parameter.
#' Pass a function which takes an int and returns
#' a gen.
#'
#' >>> sized ( function(e) elements(c(1:e)) )
sized <- function ( f ) {
  gen ( function ( size ) { f(size)$unGen( size ) } )
}

#' Create a gen from a vector
#' Semantically similar to, and implemented with,
#' sample(x, 1)
element <- function ( x ) {
  gen ( function ( size ) { tree ( sample(x, 1) )})
}

#' Generate an integer with shrinking,
#' from and to the specified values
integral <- function ( from, to ) {
  shrink( towards(from), gen ( function( size ) { tree ( round(runif( 1, from, to)))  }) )
}

#' Generate a float with shrinking,
#' from and to the specified values
floating <- function ( from, to ) {
  shrink( towards(from), gen ( function( size ) { tree ( runif( 1, from, to))  }) )
}

#' Helper to create a generator with a
#' shrink function.
#'
#' shrinker : a -> list(a)
shrink <- function ( shrinker, g ) {
  if (!inherits(g,"gen"))
    stop("shrink function takes a 'gen'");

  gen ( function( size )
    tree.expand ( shrinker, g$unGen(size) )
  )
}

#' Generate a vector of primitive values with
#' fixed length
vec.of <- function ( x, g ) {
  if (!inherits(g, "gen"))
    stop("vec.of function takes a 'gen'");

  gen ( function (size )
    tree.replicateM ( x, function() { g$unGen(size) } )
  )
}

#' Generate a vector of primitive values, with
#' length bounded by the size parameter.
vec <- function ( gen ) {
  if (!inherits(gen,"gen"))
    stop("vec function takes a 'gen'");

  gen ( function ( size ) {
    tree.bind( function ( num ) {
      tree.replicateM ( num, function() { gen$unGen(size) })
    }, integral(1, size)$unGen(size) )
  })
}
