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
#' > g <- gen.structure( vec.of(6, gen.sample(1:30)), dim = 3:2)
#' > gen.example ( g )
#'      [,1] [,2]
#' [1,]   21   11
#' [2,]    2   10
#' [3,]    5   27
#'
#' > g <- gen.structure( list ( vec.of(4, gen.sample(2:10)), vec.of(4, gen.sample(2:10))), names = c("a","b"), class = "data.frame", row.names = c("1", "2", "3", "4" ))
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
#' > g <- gen.with( gen.sample(2:100), function(x) vec.of( x, gen.sample(1:10)))
#' > gen.example ( g )
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

#' Use a value as an unshrinking generator.
gen.pure <- function ( x ) {
  gen ( function ( size ) tree( x ) )
}

#' Helper for making a gen with a size parameter.
#' Pass a function which takes an int and returns
#' a gen.
#'
#' >>> sized ( function(e) elements(c(1:e)) )
gen.sized <- function ( f ) {
  gen ( function ( size ) { f(size)$unGen( size ) } )
}

#' Create a gen from a vector
#' Semantically similar to sample(x, 1)
#'
#' This generator shrinks to the first
#' value.
gen.sample <- function ( x ) {
  gen.with ( gen.sample.int(length(x)), function(i) gen.pure( x[i] ))
}

#' Create a gen from a vector
#' Semantically similar to sample.int(n, 1)
#'
#' This generator shrinks to 1.
gen.sample.int <- function ( n ) {
  gen.shrink( towards(1), gen ( function( size ) { tree ( sample.int(n, 1) ) }))
}

#' Generate a float between the from
#' and to the values specified.
#'
#' Shrinks towards the `from` value.
gen.runif <- function ( from, to ) {
  gen.shrink( towards(from), gen ( function( size ) { tree ( runif( 1, from, to))  }) )
}

#' Generate a float with a gamma distribution
#'
#' Shrinks towards the median value.
gen.rgamma <- function ( shape, rate = 1, scale = 1/rate) {
  gen.shrink( towards(qgamma(0.5, shape, rate )), gen ( function( size ) { tree ( rgamma( 1, shape, rate )) }) )
}

#' Generate a float with a gamma distribution
#'
#' Shrinks towards the median value.
gen.rbeta <- function ( shape1, shape2, ncp = 0 ) {
  gen.shrink( towards(qbeta(0.5, shape1, shape2, ncp )), gen ( function( size ) { tree ( rbeta( 1, shape1, shape2, ncp )) }) )
}

#' Helper to create a generator with a
#' shrink function.
#'
#' shrinker takes an 'a and returns a vector of 'a.
gen.shrink <- function ( shrinker, g ) {
  if (!inherits(g,"gen"))
    stop("gen.shrink function takes a 'gen'");

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
    }, gen.sample(1:size)$unGen(size) )
  })
}
