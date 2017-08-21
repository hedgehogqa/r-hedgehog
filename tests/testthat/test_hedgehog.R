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
  forall (gen.sample.int(10), function(x) if (x > 8) discard() else expect_true(T))
})

test_that("Discarding 80% will reach discard limit and fail", {
  expect_failure (
    forall (gen.sample.int(10), function(x) if (x > 2) discard() else expect_true(T))
  )
})

test_that("forall fails if no expectations are given", {
  expect_failure (
    forall (NULL, function(x) NULL)
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

#####################
# Generator testing #
#####################
context("Hedgehog Generator testing")

test_that("gen.sample.int only contains good values", {
  forall (gen.sample.int(10), function(x) {
    expect_true(x <= 10 && x >= 0)
  })
})

test_that("gen.sample only contains good values", {
  forall (gen.sample(5:10), function(x) {
    expect_true(x <= 10 && x >= 5)
  })
})

test_that("gen.sample produces single values from a list", {
  forall (gen.sample(1:10), function(x) {
    expect_true(x <= 10 && x >= 1)
  })
})

test_that("all generators shrink soundly", {
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
  forall (list( lower = 2, higher = gen.sample(5:10)), function(lower, higher) {
    expect_less_than(lower, higher)
  })
)

test_that("can build data frames with structure", {
  g <- gen.structure (
       list ( gen.c.of(4, gen.sample(2:10))
            , gen.c.of(4, gen.sample(2:10))
            , c('a', 'b', 'c', 'd')
            )
       , names = c("a","b", "constant")
       , class = "data.frame"
       , row.names = c("1", "2", "3", "4" ))
  forall( g, function(x) expect_equal(nrow(x), 4))
})

test_that("can build data frames with data.map", {
  g <- gen.map ( as.data.frame,
       list ( as = gen.c.of(4, gen.sample(2:10))
            , bs = gen.c.of(4, gen.sample(2:10))
            , cs = c('a', 'b', 'c', 'd')
            ))
  forall( g, function(x) expect_equal(nrow(x), 4))
})
