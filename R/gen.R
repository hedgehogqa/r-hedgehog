#' Constructor for a generator.
#'
#' We're using R's global random number gen.
#'
#' A gen is essentially a function which takes
#' a size parameter and returns a lazy tree.
gen <- function ( x ) {
  structure ( list ( unGen = x ), class = "gen" )
}

#' Sample from a generator.
#' @export
#' @param g A generator
#' @param size The sized example to view
gen.example <- function ( g, size = 5 ) {
  tree <- g$unGen(size)
  tree$root
}

#' Print information about a generator.
#' @param g A generator
print.gen <- function ( g ) {
  cat( "Hedgehog generator:\n" )
  cat( "A generator is a function which produces random trees\n" )
  cat( "using a size parameter to scale it.\n\n" )
  cat( "Example:\n" )
  print ( gen.example ( g ))
}

#' Generate a structure
#'
#' If you can create an object with `structure`,
#' you should be able to generate an object with
#' this function from a generator or list of
#' generators.
#'
#' @export
#' @param x an object generator which will have various attributes attached to it.
#' @param ... attributes, specified in ‘tag = value’ form, which will be
#'   attached to generated data.
#'
#' @examples
#' g <- gen.structure( vec.of(6, gen.sample(1:30)), dim = 3:2)
#' gen.example ( g )
#'
#' # To create a data frame for testing.
#' g <- gen.structure (
#'        list ( vec.of(4
#'             , gen.sample(2:10))
#'             , vec.of(4, gen.sample(2:10))
#'             )
#'        , names = c("a","b")
#'        , class = "data.frame"
#'        , row.names = c("1", "2", "3", "4" ))
#' gen.example ( g )
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
#' @export
#' @param g a generator whose results will be used by f
#' @param f a function from a value to a generator.
#'
#' @examples
#' g <- gen.with( gen.sample(2:100), function(x) vec.of( x, gen.sample(1:10)))
#' gen.example ( g )
#' # [1] 8 6 2 7 5 4 2 2 4 6 4 6 6 3 6 7 8 5 4 6
gen.with <- function ( g, f ) {
  gen ( function ( size ) {
    tree <- g$unGen(size)
    tree.bind ( function(x) { f(x)$unGen(size) }, tree )
  })
}

#' Use one generator as a precondition for another.
#'
#' This is a monadic bind with arguments reversed.
#'
#' @export
#' @param f a function from a value to a generator.
#' @param g a generator whose results will be used by f
#'
#' @examples
#' g <- gen.bind( function(x) vec.of( x, gen.sample(1:10)), gen.sample(2:100))
#' gen.example ( g )
#' # [1] 8 6 2 7 5 4 2 2 4 6 4 6 6 3 6 7 8 5 4 6
gen.bind <- function ( f, g ) {
  gen.with( g, f )
}

#' Use a value as an unshrinking generator.
#' @param x a pure value which will now be embedded into
#    a generator.
#' @export
gen.pure <- function ( x ) {
  gen ( function ( size ) tree( x ) )
}

#' Map a function to the results of a generator
#' @param f a function which will be applied to
#'   values created by the generator.
#' @export
gen.map <- function ( f, g ) {
  gen ( function ( size ) {
    tree <- g$unGen(size)
    tree.map ( f, tree )
  })
}

#' Helper for making a gen with a size parameter.
#' Pass a function which takes an int and returns
#' a gen.
#'
#' @export
#' @param f the function, taking a size and returning
#'   a generator
#' @examples
#' gen.sized ( function(e) gen.sample(1:e) )
gen.sized <- function ( f ) {
  gen ( function ( size ) { f(size)$unGen( size ) } )
}

#' Create a gen from a vector
#' Semantically similar to sample(x, 1)
#'
#' This generator shrinks to the first
#' value.
#'
#' @export
#'
#' @param a list or vector to sample from.
#'
#' @examples
#' gen.sample(1:10)   # a number
#' gen.sample(c(T,F)) # a boolean
gen.sample <- function ( x ) {
  gen.with ( gen.sample.int(length(x)), function(i) gen.pure( x[i] ))
}

#' Create a gen from a vector
#' Semantically similar to sample.int(n, 1)
#'
#' This generator shrinks to 1.
#'
#' @export
#'
#' @param a number which is the maximum integer
#'   sampled from.
#'
#' @examples
#' gen.sample.int(10) # a number up to 10
gen.sample.int <- function ( n ) {
  gen.shrink( towards(1), gen ( function( size ) { tree ( sample.int(n, 1) ) }))
}

#' Generate a float between the from
#' and to the values specified.
#'
#' Shrinks towards the `from` value, or
#' if shrink.median is on, the middle.
#'
#' @export
#'
#' @param from same as from in runif
#' @param to same as to in runif
#' @param shrink.median whether to shrink
#'   to the middle of the distribution
#'   instead of the low end.
#'
#' @examples
#' gen.unif(0, 1) # a float between 0 and 1
gen.unif <- function ( from, to, shrink.median = F ) {
  gen.shrink(
    towards(qunif(ifelse ( shrink.median, 0.5, 0 ), from, to))
  , gen ( function( size ) { tree ( runif( 1, from, to)) })
  )
}

#' Generate a float with a gamma distribution
#'
#' Shrinks towards the median value.
#'
#' @export
#'
#' @param shape same as shape in rgamma
#' @param rate same as rate in rgamma
#' @param scale same as scale in rgamma
gen.gamma <- function ( shape, rate = 1, scale = 1/rate) {
  gen.shrink (
    towards(qgamma(0.5, shape, rate ))
  , gen ( function( size ) { tree ( rgamma( 1, shape, rate )) })
  )
}

#' Generate a float with a gamma distribution
#'
#' Shrinks towards the median value.
#'
#' @export
#'
#' @param shape1 same as shape1 in rbeta
#' @param shape2 same as shape2 in rbeta
gen.beta <- function ( shape1, shape2, ncp = 0 ) {
  gen.shrink (
    towards(qbeta(0.5, shape1, shape2, ncp ))
  , gen ( function( size ) { tree ( rbeta( 1, shape1, shape2, ncp )) })
  )
}

#' Helper to create a generator with a
#' shrink function.
#'
#' shrinker takes an 'a and returns a vector of 'a.
#'
#' @export
#'
#' @param shrinker a function takes an 'a and
#'   returning a vector of 'a.
#' @param g a generator we wish to add shrinking
#'   to
gen.shrink <- function ( shrinker, g ) {
  if (!inherits(g,"gen"))
    stop("gen.shrink function takes a 'gen'");

  gen ( function( size )
    tree.expand ( shrinker, g$unGen(size) )
  )
}

#' Generate a vector of primitive values
#' from a generator
#' @export
vec <- function ( g ) {
  gen.map ( unlist, gen.list(g) )
}

#' Generate a vector of primitive values
#' from a generator.
#' @export
vec.of <- function ( x, g ) {
  gen.map ( unlist, gen.list.of(x, g) )
}

#' Generate a list of values with fixed length
#' @export
gen.list.of <- function ( number, generator ) {
  if (!inherits(generator, "gen"))
    stop("vec.of function takes a 'gen'");

  gen ( function ( size )
    tree.replicateM ( number, function() { generator$unGen(size) } )
  )
}

#' Generate a list of values, with
#' length bounded by the size parameter.
#' @export
gen.list <- function ( gen ) {
  if (!inherits(gen,"gen"))
    stop("vec function takes a 'gen'");

  gen ( function ( size ) {
    tree.bind( function ( num ) {
      tree.replicateM ( num, function() { gen$unGen(size) })
    }, gen.sample(1:size)$unGen(size) )
  })
}
