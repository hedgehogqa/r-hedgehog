#' Property based testing in R
#'
#' Hedgehog is a modern property based testing
#' system in the spirit of QuickCheck, originally written in Haskell,
#' but now also available in R.
#'
#' @details Software testing is critical when we want to distribute our
#' work, but unit testing only covers examples we have thought of.
#'
#' With hedgehog (integrated into testthat), we can instead test properties
#' which our programs and functions should have, and allow automatic
#' generation of tests, which cover more that we could imagine.
#'
#' One of the key benefits of Hedgehog is
#' integrated shrinking of counterexamples, which allows one to quickly
#' find the cause of bugs, given salient examples when incorrect
#' behaviour occurs.
#'
#' @docType package
#' @name hedgehog
#' @references Campbell, H (2017). hedgehog: Property based testing in R
#' \strong{The R Journal} under submission.
#'
#' \url{https://github.com/hedgehogqa/r-hedgehog}
#'
#' @examples
#' library(hedgehog)
#' test_that( "Reverse and concatenate symmetry",
#'   forall( list( as = gen.c( gen.sample(1:100) )
#'               , bs = gen.c( gen.sample(1:100) ))
#'         , function( as, bs )
#'             expect_identical ( rev(c(as, bs)), c(rev(bs), rev(as)))
#'   )
#' )
NULL