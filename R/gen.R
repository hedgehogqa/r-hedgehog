#' Generators
#'
#' A Hedgehog generator is a function, which, using R's
#' random seed, will build a lazy rose tree given a size
#' parameter, which represent a value to test, as well
#' as possible shrinks to try in the event of a failure.
#' Usually, one should compose the provided generators
#' instead of dealing with the gen contructor themselves.
#'
#' Hedgehog generators are functors and monads, allowing
#' one to map over them and use their results to create
#' more complex generators.
#'
#' A generator can use R's random seed when constructing
#' its value, but all shrinks should be deterministic.
#'
#' In general, functions which accept a generator can also
#' be provided with a list of generators nested arbitrarily.
#'
#'
#' @param f
#'   a function from a value to new generator, used to
#'   build new generators monadically from a generator's
#'   output
#' @param m
#'   a function to apply to values produced the generator
#' @param g
#'   a generator to map or bind over
#' @param x
#'   a value to use as a generator
#' @param t
#'   a function producing a tree from a size parameter
#'
#' @examples
#' # To create a matrix
#' gen.map( function(x) { matrix(x, ncol=3) }, gen.c.of(6, gen.sample(1:30)) )
#'
#' # Generating a vector whose length is defined by a generator
#' g <- gen.with( gen.sample(2:100), function(x) gen.c.of( x, gen.sample(1:10)))
#' gen.example ( g )
#' # [1] 8 6 2 7 5 4 2 2 4 6 4 6 6 3 6 7 8 5 4 6
#'
#' # Same as above, as @bind@ is @with@ with arguments flipped.
#' g <- gen.bind( function(x) gen.c.of( x, gen.sample(1:10)), gen.sample(2:100))
#' gen.example ( g )
#' # [1] 8 6 2 7 5 4 2 2 4 6 4 6 6 3 6 7 8 5 4 6
#' @name gen-monad
NULL

#' @rdname gen-monad
gen <- function ( t )
  structure ( list ( unGen = t ), class = "gen" )

#' @rdname gen-monad
#' @export
gen.with <- function ( g, f )
  gen ( function ( size ) {
    trees     <- unfoldgenerator ( g, size )
    tree      <- tree.traverse ( trees )
    tree.bind ( function(x) {
      new <- unfoldgenerator (f(x), size)
      tree.traverse ( new )
    }, tree )
  })

#' @rdname gen-monad
#' @export
gen.bind <- function ( f, g )
  gen.with( g, f )

#' @rdname gen-monad
#' @export
gen.pure <- function ( x )
  gen ( function ( size ) tree ( x ) )

#' @rdname gen-monad
#' @export
gen.map <- function ( m, g )
  gen ( function ( size ) {
    trees  <- unfoldgenerator ( g, size )
    tree   <- tree.traverse ( trees )
    tree.map ( m, tree )
  })

#' Sample from a generator.
#' @export
#' @param g A generator
#' @param size The sized example to view
gen.example <- function ( g, size = 5 ) {
  trees  <- unfoldgenerator ( g, size )
  tree   <- tree.traverse ( trees )
  tree
}

#' Print information about a generator.
#' @param g A generator
print.gen <- function ( g ) {
  example <- gen.example ( g )
  cat( "Hedgehog generator:\n" )
  cat( "A generator is a function which produces random trees\n" )
  cat( "using a size parameter to scale it.\n\n" )
  cat( "Example:\n" )
  print ( example$root )
  cat( "Shrinks:\n" )
  lapply ( example$children(), function ( c ) print( c$root ))
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
#' @param ... attributes, specified in 'tag = value' form, which will be
#'   attached to generated data.
#'
#' @examples
#' # To create a matrix
#' gen.structure( gen.c.of(6, gen.sample(1:30)), dim = 3:2)
#'
#' # To create a data frame for testing.
#' gen.structure (
#'   list ( gen.c.of(4, gen.sample(2:10))
#'        , gen.c.of(4, gen.sample(2:10))
#'        , c('a', 'b', 'c', 'd')
#'        )
#'   , names = c("a","b", "constant")
#'   , class = "data.frame"
#'   , row.names = c("1", "2", "3", "4" ))
gen.structure <- function ( x, ... )
  gen.map (
    function(m) { attributes(m) <- list(...); m }
  , x
  )

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
gen.sized <- function ( f )
  gen ( function ( size ) {
    trees  <- unfoldgenerator ( f(size), size )
    tree.traverse ( trees )
  })

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
gen.sample <- function ( x )
  gen.map (
    function(i) {
      if (is.list(x)) {
        x[[i]]
      } else {
        x[i]
      }
    }
  , gen.sample.int(length(x))
  )


#' @rdname gen-sample
#' @export
gen.sample.int <- function ( n )
  gen.shrink (
    shrink.towards(1)
  , gen ( function( size ) { tree ( sample.int(n, 1) ) })
  )

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
gen.unif <- function ( from, to, shrink.median = T )
  gen.shrink(
    shrink.towards(qunif(ifelse ( shrink.median, 0.5, 0 ), from, to))
  , gen ( function( size ) { tree ( runif( 1, from, to)) })
  )

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
gen.gamma <- function ( shape, rate = 1, scale = 1/rate)
  gen.shrink (
    shrink.towards(qgamma(0.5, shape, rate ))
  , gen ( function( size ) { tree ( rgamma( 1, shape, rate )) })
  )

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
gen.beta <- function ( shape1, shape2, ncp = 0 )
  gen.shrink (
    shrink.towards(qbeta(0.5, shape1, shape2, ncp ))
  , gen ( function( size ) { tree ( rbeta( 1, shape1, shape2, ncp )) })
  )

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
gen.shrink <- function ( shrinker, g )
  gen ( function( size )
    tree.expand ( shrinker
                , tree.traverse(unfoldgenerator(g, size)))
  )


#' Stop a generator from shrinking
#'
#' @export
#'
#' @param g a generator we wish to remove shrinking
#'   from
gen.no.shrink <- function ( g )
  gen ( function( size ) {
    ts <- unfoldgenerator(g, size)
    t  <- tree.traverse( ts )
    tree ( t$root )
  })


#' Generate a vector of primitive values
#' from a generator
#'
#' @export
#'
#' @param generator a generator used for vector elements
#' @param from minimum length of the list of
#'   elements
#' @param to maximum length of the list of
#'   elements ( defaults to size if NULL )
gen.c <- function ( generator, from = 1, to = NULL  ) {
  gen.map ( unlist, gen.list(generator, from, to) )
}

#' Generate a vector of primitive values
#' from a generator.
#'
#' @export
#'
#' @param number length of vector to generate
#' @param generator a generator used for vector elements
gen.c.of <- function ( number, generator ) {
  gen.map ( unlist, gen.list.of( number, generator ))
}

#' Generate a list of values with fixed length
#'
#' @export
#'
#' @param number length of list to generate
#' @param generator a generator used for list elements
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
#' @param generator a generator used for list elements
#' @param from minimum length of the list of
#'   elements
#' @param to maximum length of the list of
#'   elements ( defaults to size if NULL )
gen.list <- function ( generator, from = 1, to = NULL )
  gen.sized ( function ( size ) {
    if (is.null(to))
      to <- size
    gen.with (
      gen.sample(from:to)
    , function ( num ) {
        shrinker <- function ( as )
          Filter( function(ls) length(ls) >= from, shrink.list(as))

        gen.shrink ( shrinker,
          gen.list.of( num, generator )
        )
      })
    })

#' Generator Conditioning
#'
#' Require a generator's output to hold some property
#'
#' This function will fail with stop if the discard
#' limit is passed.
#'
#' In general, one should try to avoid using this
#' function, and instead create appropriate values for
#' one's tests by construction.
#'
#' @export
#'
#' @param requires a predicate which must hold before
#'   the generated value is used.
#' @param generator a generator used for list elements
#' @param discard.limit the maximum number of predicate
#'   failures on a single example before this generator
#'   fails. This should be at most 100, as beyond this
#'   level, R may detect the generator to be an infinite
#'   loop and kill it.
gen.ensure <- function ( requires, generator, discard.limit = 100 ) {
  if ( discard.limit <= 0 )
    stop ( "Discard limit reached in ensure" )
  gen.with (generator,
    function(v) {
      if (requires(v)) {
        gen.pure(v)
      } else {
        gen.ensure ( requires, generator, discard.limit = discard.limit - 1)
      }
    })
}
