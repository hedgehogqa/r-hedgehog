library(hedgehog)

context("hedgehog")

property("Constants work as generators", {
  forall (T, function(x) x)
})

property("Constants work as generators", {
  forall (F, function(x) x)
})
