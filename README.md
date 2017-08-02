hedgehog
========

<img src="https://github.com/hedgehogqa/haskell-hedgehog/raw/master/img/hedgehog-logo.png" width="307" align="right"/>

> Hedgehog will eat all your bugs.

Hedgehog is a modern property based testing system, originally written
in Haskell, but now also ported to R. One of the key benefits of
hedgehog is integrated shrinking of counterexamples, which allows one to
quickly find the cause of bugs.

Features
========

-   Integrated shrinking, shrinks obey invariants by construction.
-   Abstract state machine testing.
-   Generators can be combined to build complex and interesting
    structures

Example
=======

To get a quick feel for how hedgehog feels, here's a quick example
showing some of the properties a reversing function should have. We'll
be testing the `rev` function from included in package:base.

    forall( gen.c( gen.sample(1:100) ), function(xs) identical ( rev(rev(xs)), xs))

    ## Passed after 100 tests

The property above tests that if I reverse a vector twice, the result
should be the same as the vector that I began with.

We use the term forall (which comes from predicate logic) to say that we
want the property to be true no matter what the input to the `rev`
function is. The first argument to forall is function to generate random
values (the generator); while the second is the property we wish to
test.

The property above doesn't actually completely specify that the `rev`
function is accurate though, as one could replace rev with the identity
function and still observe this result. We will therefore write one more
property to completely test this function.

    forall( list( as = gen.c( gen.sample(1:100) )
                , bs = gen.c( gen.sample(1:100) ))
          , function(as,bs) identical ( rev(c(as, bs)), c(rev(bs), rev(as)))
    )

    ## Passed after 100 tests

Cool. Notice that the property function now accepts two arguments: `as`
and `bs`. A list of generators in hedgehog is treated as a generator of
lists, and shrinks like one. We do however do our best to make sure that
properties can be specified naturally if the generator is specified in
this way. Now let's look at an assertion which isn't true so we can see
what a counterexamples looks like

    forall( gen.c( gen.sample(1:100) ), function(xs) identical ( rev(xs), xs))

    ##
    ## Falsifiable after 1 tests, and 3 shrinks
    ## Predicate is falsifiable
    ##
    ## Counterexample:
    ## [1] 1 2

Here, the counterexample is shrunk from an original test value.
