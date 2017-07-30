#' Constructor for a generator.
#'
#' We're using R's global random number gen.
#'
#' A gen is essentially a function which takes
#' a size parameter and returns a lazy tree.
gen <- function ( x ) {
  structure ( list ( unGen = x ), class = "gen" )
}

#' Generator Monad
#'
#' Hedgehog generators are functors and monads, allowing
#' one to map over them and use their results to create
#' more complex generators.
#'
#' A generator can use R's random seed when constructing
#' its value, but all shrinks should be deterministic.
#'
#' Generator's are functors and monads, meaning that one
#' can take the output of a generator and create a new
#' generator depending on the result.
#'
#' @param f a function from a value to a generator.
#' @param m a function to apply to values in the generator
#' @param g a generator to map or bind over
#' @param x a value to use as a generator
#'
#' @examples
#' # To create a matrix
#' gen.map( function(x) { matrix(x, ncol=3) }, vec.of(6, gen.sample(1:30)) )
#'
#' # Generating a vector whose length is defined by a generator
#' g <- gen.with( gen.sample(2:100), function(x) vec.of( x, gen.sample(1:10)))
#' gen.example ( g )
#' # [1] 8 6 2 7 5 4 2 2 4 6 4 6 6 3 6 7 8 5 4 6
#'
#' # Same as above, as @bind@ is @with@ with arguments flipped.
#' g <- gen.bind( function(x) vec.of( x, gen.sample(1:10)), gen.sample(2:100))
#' gen.example ( g )
#' # [1] 8 6 2 7 5 4 2 2 4 6 4 6 6 3 6 7 8 5 4 6
#' @name gen-monad
NULL

#' @rdname gen-monad
#' @export
gen.with <- function ( g, f ) {
  gen ( function ( size ) {
    trees  <- unfoldgenerator ( g, size )
    tree   <- tree.traverse ( trees )
    tree.bind ( function(x) { f(x)$unGen(size) }, tree )
  })
}

#' @rdname gen-monad
#' @export
gen.bind <- function ( f, g ) {
  gen.with( g, f )
}

#' @rdname gen-monad
#' @export
gen.pure <- function ( x ) {
  gen ( function ( size ) tree ( x ) )
}

#' @rdname gen-monad
#' @export
gen.map <- function ( m, g ) {
  gen ( function ( size ) {
    trees  <- unfoldgenerator ( g, size )
    tree   <- tree.traverse ( trees )
    tree.map ( m, tree )
  })
}

#' Sample from a generator.
#' @export
#' @param g A generator
#' @param size The sized example to view
gen.example <- function ( g, size = 5 ) {
  trees  <- unfoldgenerator ( g, size )
  tree   <- tree.traverse ( trees )
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
#' gen.structure accepts the same forms of data
#' as forall, and is flexible, in that any list
#' of generators is considered to be a generator.
#'
#' @export
#' @param x an object generator which will have various attributes attached to it.
#' @param ... attributes, specified in ‘tag = value’ form, which will be
#'   attached to generated data.
#'
#' @examples
#' To create a matrix
#' gen.structure( vec.of(6, gen.sample(1:30)), dim = 3:2)
#'
#' # To create a data frame for testing.
#' gen.structure (
#'   list ( vec.of(4, gen.sample(2:10))
#'        , vec.of(4, gen.sample(2:10))
#'        , c('a', 'b', 'c', 'd')
#'        )
#'   , names = c("a","b", "constant")
#'   , class = "data.frame"
#'   , row.names = c("1", "2", "3", "4" ))
gen.structure <- function ( x, ... ) {
  gen ( function( size ) {
    trees  <- unfoldgenerator ( x, size )
    tree   <- tree.traverse ( trees )
    tree.map( function(m) { attributes(m) <- list(...); m }, tree )
  })
}

#' Sized generator creation
#'
#' Helper for making a gen with a size parameter.
#' Pass a function which takes an int and returns
#' a gen.
#'
#' @export
#' @param f the function, taking a size and
#'   returning a generator
#'
#' @examples
#' gen.sized ( function(e) gen.sample(1:e) )
gen.sized <- function ( f ) {
  gen ( function ( size ) {
    trees  <- unfoldgenerator ( f(size), size )
    tree.traverse ( trees )
  })
}

#' Random Sample Generation
#'
#' These methods are semantically related to
#' sample and sample.int.
#'
#' These generators shrinks to the first
#' value and 1 respectively.
#'
#' @param x a list or vector to sample from.
#' @param n number which is the maximum integer
#'   sampled from.
#'
#' @examples
#' gen.sample(1:10)   # a number
#' gen.sample(c(T,F)) # a boolean
#' gen.sample.int(10) # a number up to 10
#'
#' @name gen-sample
NULL

#' @rdname gen-sample
#' @export
gen.sample <- function ( x ) {
  gen.map ( function(i) {
    if (is.list(x)) {
      x[[i]]
    } else {
      x[i]
    }
  }, gen.sample.int(length(x)))
}

#' @rdname gen-sample
#' @export
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
#' @importFrom stats runif qunif
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
#' @importFrom stats rgamma qgamma
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
#' @importFrom stats rbeta qbeta
#'
#' @param shape1 same as shape1 in rbeta
#' @param shape2 same as shape2 in rbeta
#' @param ncp same as ncp in rbeta
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
  gen ( function( size )
    tree.expand ( shrinker, g$unGen(size) )
  )
}

#' Generate a vector of primitive values
#' from a generator
#'
#' @export
#'
#' @param number length of vector to generate
#' @param g a generator used for vector elements
vec <- function ( g ) {
  gen.map ( unlist, gen.list(g) )
}

#' Generate a vector of primitive values
#' from a generator.
#'
#' @export
#'
#' @param number length of vector to generate
#' @param g a generator used for vector elements
vec.of <- function ( x, g ) {
  gen.map ( unlist, gen.list.of(x, g) )
}

#' Generate a list of values with fixed length
#'
#' @export
#'
#' @param number length of list to generate
#' @param g a generator used for list elements
gen.list.of <- function ( number, generator ) {
  gen ( function ( size )
    tree.replicateM ( number, function() {
      trees  <- unfoldgenerator (generator, size)
      tree.traverse( trees )
    })
  )
}

#' Generate a list of values, with
#' length bounded by the size parameter.
#'
#' @export
#'
#' @param g a generator used for list elements
gen.list <- function ( generator ) {
  gen ( function ( size ) {
    tree.bind( function ( num ) {
      tree.replicateM ( num, function() {
        trees  <- unfoldgenerator (generator, size)
        tree.traverse( trees )
      })
    }, gen.sample(1:size)$unGen(size) )
  })
}
