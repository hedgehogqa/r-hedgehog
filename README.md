Hedgehog
========

<img src="vignettes/hedgehog-logo.png" width="307" align="right"/>

> Hedgehog will eat all your bugs.

[Hedgehog](http://hedgehog.qa/) is a modern property based testing
system in the spirit of QuickCheck, originally written in Haskell,
but now also available in R. One of the key benefits of Hedgehog is
integrated shrinking of counterexamples, which allows one to quickly
find the cause of bugs, given salient examples when incorrect
behaviour occurs.

Features
========

- Expressive property based testing.
- Integrated shrinking, shrinks obey invariants by construction.
- Generators can be combined to build complex and interesting
  structures.
- Abstract state machine testing.
- Full compatibility with [testthat][testthat] makes it easy to
  add property based testing, without disrupting your work flow.

Example
=======

To get a quick look of how Hedgehog feels, here's an example
showing some of the properties a function which reverses a vector
should have. We'll be testing the `rev` function from
`package:base`.


```r
test_that( "Reverse of reverse is identity",
  forall( gen.c( gen.element(1:100) ), function(xs) expect_equal(rev(rev(xs)), xs))
)
```

Documentation
=============

Hedgehog has extensive documentation available at its
[homepage][homepage] as well vingettes and function documentation
within the R package

  [testthat]: https://github.com/hadley/testthat
  [homepage]: https://hedgehog.qa/r-hedgehog
