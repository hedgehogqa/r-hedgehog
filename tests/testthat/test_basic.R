library(hedgehog)

context("Basic Usage")

test_that("reverse . reverse = identity", {
  forall ( gen.c( gen.element(1:100)), function(x) {
    expect_identical ( rev(rev(x)), x )
  })
})

test_that("reverse != identity", {
  expect_failure(
    forall ( gen.c( gen.element(1:100)) , function(x) {
      expect_identical ( rev(x), x )
    })
  )
})

test_that("reverse is associative", {
  forall ( list (as = gen.c( gen.element(1:100)), bs = gen.c(gen.element(1:100))), function(as, bs) {
    expect_identical ( c(rev(bs), rev(as)), rev( c(as, bs )))
  })
})
