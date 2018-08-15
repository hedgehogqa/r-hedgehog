library(hedgehog)

context("Hedgehog self-check")

test_that("forall is a true expectation", {
  expect_failure (
    forall (F, function(x) expect_true(x))
  )
  expect_success (
    forall (T, function(x) expect_true(x))
  )
})

test_that("forall inception expectation", {
  forall (F, function(x)
    expect_failure ( expect_true(x) )
  )
  forall (T, function(x)
    expect_success ( expect_true(x) )
  )
})

test_that("Discarding 20% will reach test limit before discard limit and succeed", {
  forall (gen.int(10), function(x) if (x > 8) discard() else expect_true(T))
})

test_that("Discarding 80% will reach discard limit and fail", {
  expect_failure (
    forall (gen.int(10), function(x) if (x > 2) discard() else expect_true(T))
  )
})

test_that("The same generator really makes different values", {
  g <- gen.int(10)
  expect_failure (
    forall (g, function(x) expect_true(x < 5))
  )
  expect_failure (
    forall (g, function(x) expect_true(x > 5))
  )
})

test_that("forall fails if no expectations are given", {
  expect_failure (
    forall (NULL, function(x) {})
  )
})

test_that("error handling inside foralls", {
  expect_success (
    forall (NULL, function(x) {
      warning("unheeded warning")
      succeed("ok")
    })
  )
  expect_success (
    forall (NULL, function(x) {
      message("unheeded message")
      succeed("ok")
    })
  )
  expect_failure (
    forall (NULL, function(x) {
      stop("Ouch")
      succeed("ok")
    })
  )
})

test_that("forall permits multiple expectations", {
  expect_failure (
    forall (NULL, function(x) {
      expect_true(T)
      expect_true(F)
    })
  )
  expect_success (
    forall (NULL, function(x) {
      expect_true(T)
      expect_true(T)
    })
  )
})

test_that("generator compositions work", {
  g  <- gen.int(100)
  gx <- generate(for (i in g) {
    gen.c(3, of = i)
  })

  forall (gx, function(x) {
    expect_equal(unique(x), 3)
  })
})

#####################
# Generator testing #
#####################
context("Hedgehog Generator testing")

test_that("gen.pure returns the value", {
  forall (gen.pure(10), function(x) {
    expect_equal(x, 10)
  })
})

test_that("gen.sized respects the size parameter", {
  sgen <- gen.sized(function(s) gen.int(s))
  forall (sgen, function(x) {
    expect_true(x <= 10)
  }, size.limit = 10)

  expect_failure(
    forall (sgen, function(x) {
      expect_true(x <= 10)
    }, size.limit = 20)
  )
})

test_that("gen.int only contains good values", {
  forall (gen.int(10), function(x) {
    expect_true(x <= 10 && x >= 0)
  })
})

test_that("gen.sample.int returns subsets", {
  forall(generate(for (i in gen.int(10)) gen.sample.int(10, i)), function(x) {
    expect_true(all(x %in% 1:10))
  })
})

test_that("gen.element only contains good values", {
  forall (gen.element(5:10), function(x) {
    expect_true(x <= 10 && x >= 5)
  })
})

test_that("gen.element produces single values from a list", {
  forall (gen.element(1:10), function(x) {
    expect_true(x <= 10 && x >= 1)
  })
})

test_that("gen.subsequence produces subsequences", {
  forall (gen.subsequence(c()), function(x) {
    expect_identical(x, c())
  })
  forall (gen.subsequence(1:10), function(x) {
    expect_identical(x, sort(x))
    expect_identical(x, intersect(x, 1:10))
  })
})

test_that("generators shrink soundly", {
  forall (
      list( u = gen.unif( 0,10 )
          , g = gen.gamma( shape = 2 )
          , b = gen.beta( 1,2 )
          )
    , function(u,g,b) expect_true(T)
  )
  expect_failure(
    forall (
        list( u = gen.unif( 0,10 )
            , g = gen.gamma( shape = 2 )
            , b = gen.beta( 1,2 )
            )
      , function(u,g,b) expect_true(F)
    )
  )
})

test_that("can mix pure and generative in a list",
  forall (list( lower = 2, higher = gen.element(5:10)), function(lower, higher) {
    expect_less_than(lower, higher)
  })
)

test_that("can build data frames with structure", {
  g <- gen.structure (
       list ( gen.c( of = 4, gen.element(2:10))
            , gen.c( of = 4, gen.element(2:10))
            , c('a', 'b', 'c', 'd')
            )
       , names = c("a","b", "constant")
       , class = "data.frame"
       , row.names = c("1", "2", "3", "4" ))
  forall( g, function(x) expect_equal(nrow(x), 4))
})

test_that("can build data frames with data.map", {
  g <- gen.map ( as.data.frame,
       list ( as = gen.c( of = 4, gen.element(2:10))
            , bs = gen.c( of = 4, gen.element(2:10))
            , cs = c('a', 'b', 'c', 'd')
            ))
  forall( g, function(x) expect_equal(nrow(x), 4))
})
