hedgehog
========

> Hedgehog will eat all your bugs.

<img src="https://github.com/hedgehogqa/r-hedgehog/raw/master/img/hedgehog-logo.png" width="307" align="right"/>

[Hedgehog](http://hedgehog.qa/) is a modern property-based testing
system in R, in the spirit of QuickCheck. Hedgehog uses integrated
shrinking, so shrinks obey the invariants of generated values by
construction.

Features
--------

- Available as an easy to use R package with minimal dependencies.
- Integrated shrinking, shrinks obey invariants by construction.
- Predicate logic allows easy testing of functional code.
- Abstract state machine testing (like QuviQ's quickcheck) allows
  testing of complex object oriented and effectful systems.

Example
-------

Once imported hedgehog exports all one needs in order to write
expressive property tests.

```R
library( hedgehog )
```

Once you have loaded the package, one can write a simple property

```R
property("Reverse of Reverse is Identity",
  forall ( vec( gen.sample(1:100)) , function(x) {
    identical ( rev(rev(x)), x )
  })
)
```
Which will run the test and return `FALSE` if there are any errors.
```
--- Reverse of Reverse is Identity ---
Passed after 100 tests
```
