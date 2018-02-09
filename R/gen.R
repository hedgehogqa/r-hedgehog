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
#' Generators which are created from impure values (i.e., have
#' randomness), can be created with \code{\link{gen.impure}},
#' which takes a function from \code{size} to a value. When
#' using this the function will not shrink, so it is best
#' composed with \code{\link{gen.shrink}}.
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
#'   a function producing a tree from a size parameter, usually
#'   an R function producing random values is used.
#' @param fg
#'   a function producing a single value from a size parameter
#'
#' @examples
#' # To create a matrix
#' gen.with( gen.c.of(6, gen.element(1:30)), function(x) { matrix(x, ncol=3) })
#'
#' # or equivalently
#' gen.map( function(x) { matrix(x, ncol=3)}, gen.c.of(6, gen.element(1:30)) )
#'
#' # To create a generator from a normal R random function
#' # (this generator does not shrink).
#' g <- gen.impure ( function(size) sample(1:10) )
#' gen.example ( g )
#' # [1]  5  6  3  4  8 10  2  7  9  1
#'
#' # Generating a vector whose length is defined by a generator
#' g <- gen.and_then( gen.element(2:100), function(x) gen.c.of( x, gen.element(1:10)))
#' gen.example ( g )
#' # [1] 8 6 2 7 5 4 2 2 4 6 4 6 6 3 6 7 8 5 4 6
#'
#' # Same as above, as @bind@ is @and_then@ with arguments flipped.
#' g <- gen.bind( function(x) gen.c.of( x, gen.element(1:10)), gen.element(2:100))
#' gen.example ( g )
#' # [1] 8 6 2 7 5 4 2 2 4 6 4 6 6 3 6 7 8 5 4 6
#' @name gen-monad
NULL

#' @rdname gen-monad
gen <- function(t) {
    structure(list(unGen = t), class = "hedgehog.internal.gen")
}

#' Run a generator
#'
#' Samples from a generator or list of generators
#' producing a (single) lazy rose tree.
#'
#' This is different to calling generarator$unGen(size)
#' in that it also works on (nested) lists of generators
#' and pure values.
#'
#' @export
#' @param generator A generator
#' @param size The size parameter passed to the
#'   generation functions
gen.run <- function(generator, size) {
    trees <- unfoldgenerator(generator, size)
    tree  <- tree.sequence(trees)
    tree
}

#' @rdname gen-monad
#' @export
gen.and_then <- function(g, f) {
    gen(function(size) {
        tree <- gen.run(g, size)
        tree.bind(function(x) {
            gen.run(f(x), size)
        }, tree)
    })
}

#' @rdname gen-monad
#' @export
gen.bind <- function(f, g) {
    gen.and_then(g, f)
}

#' @rdname gen-monad
#' @export
gen.pure <- function(x) {
    gen(function(size) tree(x))
}

#' @rdname gen-monad
#' @export
gen.impure <- function(fg) {
    gen(function(size) tree(fg(size)))
}

#' @rdname gen-monad
#' @export
gen.with <- function(g, m) {
    gen(function(size) {
        tree <- gen.run(g, size)
        tree.map(m, tree)
    })
}

#' @rdname gen-monad
#' @export
gen.map <- function(m, g) {
    gen.with(g, m)
}

#' Sample from a generator.
#' @export
#' @param g A generator
#' @param size The sized example to view
gen.example <- function(g, size = 5) {
    gen.run(g, size)
}

#' @export
print.hedgehog.internal.gen <- function(x, ...) {
    example <- gen.example(x)
    cat("Hedgehog generator:\n")
    cat("Example:\n")
    print(example$root)
    cat("Initial shrinks:\n")
    lapply(example$children(), function(c) print(c$root))
}

#' Generate a structure
#'
#' If you can create an object with \code{structure},
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
#' gen.structure( gen.c.of(6, gen.element(1:30)), dim = 3:2)
#'
#' # To create a data frame for testing.
#' gen.structure (
#'   list ( gen.c.of(4, gen.element(2:10))
#'        , gen.c.of(4, gen.element(2:10))
#'        , c('a', 'b', 'c', 'd')
#'        )
#'   , names = c('a','b', 'constant')
#'   , class = 'data.frame'
#'   , row.names = c('1', '2', '3', '4' ))
gen.structure <- function(x, ...) {
    gen.map(function(m) {
        attributes(m) <- list(...)
        m
    }, x)
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
#' gen.sized ( function(e) gen.element(1:e) )
gen.sized <- function(f) {
    gen(function(size) {
        tree <- gen.run(f(size), size)
        tree
    })
}

#' Random Sample Generation
#'
#' Generators which sample from a list or produce random
#' integer samples. Both single sample, with \code{gen.element};
#' and multi-sample, with \code{gen.sample} and \code{gen.subsequence}
#' are supported; while \code{gen.choice} is used to choose from
#' generators instead of examples.
#'
#' These generators implement shrinking.
#'
#' @param x a list or vector to sample an element from.
#' @param ... generators to sample from
#' @param n the number which is the maximum integer
#'   sampled from.
#' @param replace Should sampling be with replacement?
#' @param size a non-negative integer or a generator of
#'   one, giving the number of items to choose.
#' @param prob a vector of probability weights for
#'   obtaining the elements of the vector being
#'   sampled.
#'
#' @examples
#' gen.element(1:10)   # a number
#' gen.element(c(TRUE,FALSE)) # a boolean
#' gen.int(10) # a number up to 10
#' gen.choice(gen.element(1:10), gen.element(letters))
#' gen.choice(NaN, Inf, gen.unif(-10, 10), prob = c(1,1,10))
#'
#' @return \code{gen.element} returns an item from the list
#'   or vector; \code{gen.int}, an integer up to the value
#'   n; \code{gen.choice}, a value from one of given selected
#'   generators; \code{gen.subsequence} an ordered subsequence
#'   from the input sequence; and \code{gen.sample} a list or
#'   vector (depending on the input) of the inputs.
#'
#'   For \code{gen.element} and \code{gen.choice}, shrinking
#'   will move towards the first item; \code{gen.int} will
#'   shrink to 1; \code{gen.subsequence} will shrink the list
#'   towards being empty; and \code{gen.sample} will shrink
#'   towards the original list order.
#'
#' @name gen-element
NULL

#' @rdname gen-element
#' @export
gen.element <- function(x, prob = NULL) {
    gen.map(function(i) {
        if (is.list(x)) {
            x[[i]]
        } else {
            x[i]
        }
    }, gen.int(length(x), prob = prob))
}

#' @rdname gen-element
#' @export
gen.int <- function(n, prob = NULL) {
    gen.shrink(shrink.towards(1), gen.impure(function(size) {
        sample.int(n, 1, prob = prob)
    }))
}

#' @rdname gen-element
#' @export
gen.choice <- function(..., prob = NULL) {
    gens <- list(...)
    gen.bind(function(i) gens[[i]], gen.int(length(gens), prob = prob))
}

#' @rdname gen-element
#' @export
gen.subsequence <- function(x) {
    gen.and_then( gen.no.shrink( gen.int( length(x)) ), function(size_) {
      gen.shrink(shrink.list,
        gen.impure( function(g_size) {
          x[sort(sample.int(length(x), size_))]
        })
      )
    })
}

#' @rdname gen-element
#' @export
gen.sample <- function(x, size, replace = FALSE, prob = NULL) {
    # If size isn't specified, then we'll use the length
    # This is the sample behaviour of sample
    arg.size <- if (missing(size)) length(x) else size

    # Monadic generator here so we can permit the size
    # argument to be a generator.
    gen.and_then(arg.size, function(size_) {
      gen.map(function(inds) x[inds],
        gen.sample.int(length(x), size_, replace = replace, prob = prob )
      )
    })
}

#' @rdname gen-element
#' @export
#' @importFrom utils combn
gen.sample.int <- function(n, size, replace = FALSE, prob = NULL) {
    # This needs a clean up

    # If size isn't specified, then we'll use the length
    # This is the sample behaviour of sample
    arg.size <- if (missing(size)) n else size

    # Helper function which partially sorts the indicies
    # selected by the main function.
    # This is a bit of an inefficient way of shrinking.
    reorder <- function(xs) {
      unique(c(reorder.halves(xs), reorder.bubble(xs)))
    }

    # Reorder function which shrinks the list, leaving a progressively
    # larger unsorted tail.
    reorder.halves <- function(xs) {
        # Halves to shrink, reversed, as we want to try the sorted variant
        # first.
        halves <- rev(as.list(length(xs) - c(shrink.halves(length(xs)), 0)))
        # For each length, sort the first half, and leave the second half
        # unsorted
        trials <- lapply(halves, function(h) c(sort(xs[c(1:h)]), xs[-c(1:h)]))
        # So we don't loop infinitely, ensure that we actually
        # reordered the list
        Filter( function(ys) !identical( xs, ys ), trials)
    }

    # Reorder function which swaps the positions of two elements.
    # Can be a bit slow, so has an upper limit to the size of
    # lists to which it is applied.
    reorder.bubble <- function(xs) {
      # Don't try if it's too big.
      if (length(xs) > 30)
        return(xs[c()])

      # Generate all possible pairs
      mat <- combn(seq_along(xs), 2)
      tst <- lapply(as.list(1:ncol(mat)),
        function(col) mat[,col]
      )

      # If the first index is bigger than the second, we can
      # swap them to get a shrink.
      pos <- lapply(Filter(function(y) xs[y[1]] > xs[y[2]], tst), sort)
      lapply(pos, function(s) { t <- xs; t[s] <- t[rev(s)]; t })
    }

    # Monadic generator here so we can permit the size
    # argument to be a generator.
    gen.and_then(arg.size, function(size_) {
      gen.shrink(reorder,
        gen.impure(function(g_size) {
          sample.int(n, size_, replace = replace, prob = prob)
        })
      )
    })
}
#' Generate a float between the from
#' and to the values specified.
#'
#' Shrinks towards the \code{from} value, or
#' if \code{shrink.median} is on, the middle.
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
gen.unif <- function(from, to, shrink.median = T) {
    gen.shrink(
        shrink.towards(qunif(ifelse(shrink.median, 0.5, 0), from, to))
    ,   gen.impure(function(size) runif(1, from, to))
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
gen.gamma <- function(shape, rate = 1, scale = 1/rate) {
    gen.shrink(
        shrink.towards(qgamma(0.5, shape, rate))
    ,   gen.impure(function(size) rgamma(1, shape, rate))
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
gen.beta <- function(shape1, shape2, ncp = 0) {
    gen.shrink(
        shrink.towards(qbeta(0.5, shape1, shape2, ncp))
    ,   gen.impure(function(size) rbeta(1, shape1, shape2, ncp))
    )
}

#' Generate a date between the from and to
#' dates specified.
#'
#' Shrinks towards the \code{from} value.
#'
#' @export
#' @importFrom stats runif qunif
#'
#' @param from a \code{Date} value
#' @param to a \code{Date} value
#'
#' @examples
#' gen.date()
#' gen.date( from = as.Date("1939-09-01"), to = as.Date("1945-09-02"))
gen.date <- function(from = as.Date("1900-01-01"), to = as.Date("3000-01-01")) {
    gen.element( seq(from, to, by="day") )
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
gen.shrink <- function(shrinker, g) {
    gen(function(size) tree.expand(shrinker, gen.run(g, size)))
}

#' Stop a generator from shrinking
#'
#' @export
#'
#' @param g a generator we wish to remove shrinking
#'   from
gen.no.shrink <- function(g) {
    gen(function(size) {
        t <- gen.run(g, size)
        tree(t$root)
    })
}

#' Generate a vector of values from a generator
#'
#' @export
#'
#' @param generator a generator used for vector elements
#' @param from minimum length of the list of
#'   elements
#' @param to maximum length of the list of
#'   elements ( defaults to size if NULL )
gen.c <- function(generator, from = 1, to = NULL) {
    gen.map(function(xs) do.call(c,xs), gen.list(generator, from, to))
}

#' Generate a vector of primitive values
#' from a generator.
#'
#' @export
#'
#' @param number length of vector to generate
#' @param generator a generator used for vector elements
gen.c.of <- function(number, generator) {
    gen.map(function(xs) do.call(c,xs), gen.list.of(number, generator))
}

#' Generate a list of values with fixed length
#'
#' @export
#'
#' @param number length of list to generate
#' @param generator a generator used for list elements
gen.list.of <- function(number, generator) {
    gen(function(size) tree.replicate(number, function() {
        gen.run(generator, size)
    }))
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
gen.list <- function(generator, from = 1, to = NULL) {
    gen.sized(function(size) {
        if (is.null(to)) {
            to <- size
        }
        gen.and_then(gen.element(from:to), function(num) {
            shrinker <- function(as) {
                Filter(function(ls) length(ls) >= from, shrink.list(as))
            }
            gen.shrink(shrinker, gen.list.of(num, generator))
        })
    })
}

# Turn a generator into a tree and a list of generators
# into a list of trees.
#
# Non-generator and list values are passed along as is.
# Generators can use the random number generator when
# creating their trees.
#
# @param generator the generator ( or list of generators )
# @param size the size parameter to use
unfoldgenerator <- function(generator, size) {
    if (inherits(generator, "hedgehog.internal.gen")) {
        # A generator can be run and turned into a tree
        generator$unGen(size)
    } else if (is.list(generator)) {
        # Lists can contain a generator.
        lapply(generator, function(g) unfoldgenerator(g, size))
    } else {
        # Static values are passed through as is
        generator
    }
}
