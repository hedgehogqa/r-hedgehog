library(hedgehog)

property("Constants work as generators", {
  forall (T, function(x) x)
})

property("gen.sample.int only contains good values", {
  forall (gen.sample.int(10), function(x) {
    x <= 8 && x >= 0
  })
})

property("gen.sample only contains good values", {
  forall (gen.sample(1:10), function(x) {
    x <= 10 && x >= 1
  })
})

property("mixed generators work well", {
  forall (list( lower = 2, higher = gen.sample(5:10)), function(x) {
    x$lower < x$higher
  })
})

property("multiple generators work well", {
  forall (list( lower = gen.sample(1:4), higher = gen.sample(5:10)), function(x) {
    x$lower < x$higher
  })
})

property("Two lists can be reversed and concatenated", {
  forall ( list (a = vec( gen.sample(1:100)), b = vec(gen.sample(1:100))), function(x) {
    identical ( c(rev(x$b), rev(x$a)), rev( c(x$a, x$b )))
  })
})

property("reverse . reverse = identity", {
  forall ( vec( gen.sample(1:100)) , function(x) {
    identical ( rev(rev(x)), x )
  })
})
