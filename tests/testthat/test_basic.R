library(hedgehog)

test_that("reverse . reverse = identity", {
  forall ( gen.c( gen.sample(1:100)) , function(x) {
    expect_identical ( rev(rev(x)), x )
  })
})

test_that("Two list, reverse and cat", {
  forall ( list (as = gen.c( gen.sample(1:100)), bs = gen.c(gen.sample(1:100))), function(as, bs) {
    expect_identical ( c(rev(bs), rev(as)), rev( c(as, bs )))
  })
})

test_that("Constants work as generators", {
  forall (expect_true(T), function(x) x)
})

test_that("gen.sample.int only contains good values", {
  forall (gen.sample.int(10), function(x) {
    expect_true(x <= 10 && x >= 0)
  })
})

test_that("gen.sample only contains good values", {
  forall (gen.sample(1:10), function(x) {
    expect_true(x <= 10 && x >= 1)
  })
})

test_that("gen.sample produces single values from a list", {
  forall (gen.sample(1:10), function(x) {
    expect_true(x <= 10 && x >= 1)
  })
})

test_that("mixed generators work well", {
  forall (list( lower = 2, higher = gen.sample(5:10)), function(lower, higher) {
    expect_less_than(lower, higher)
  })
})

test_that("multiple generators work well", {
  forall (list( lower = gen.sample(1:4), higher = gen.sample(5:10)), function(lower, higher) {
    expect_less_than(lower, higher)
  })
})

test_that("data.frame", {
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
