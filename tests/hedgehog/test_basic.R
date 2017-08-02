library(hedgehog)

property("reverse . reverse = identity", {
  forall ( gen.c( gen.sample(1:100)) , function(x) {
    identical ( rev(rev(x)), x )
  })
})

property("Two list, reverse and cat", {
  forall ( list (as = gen.c( gen.sample(1:100)), bs = gen.c(gen.sample(1:100))), function(as, bs) {
    identical ( c(rev(bs), rev(as)), rev( c(as, bs )))
  })
})

property("Constants work as generators", {
  forall (T, function(x) x)
})

property("gen.sample.int only contains good values", {
  forall (gen.sample.int(10), function(x) {
    x <= 10 && x >= 0
  })
})

property("gen.sample only contains good values", {
  forall (gen.sample(1:10), function(x) {
    x <= 10 && x >= 1
  })
})

property("gen.sample produces single values from a list", {
  forall (gen.sample(1:10), function(x) {
    x <= 10 && x >= 1
  })
})

property("mixed generators work well", {
  forall (list( lower = 2, higher = gen.sample(5:10)), function(lower, higher) {
    lower < higher
  })
})

property("multiple generators work well", {
  forall (list( lower = gen.sample(1:4), higher = gen.sample(5:10)), function(lower, higher) {
    lower < higher
  })
})

property("data.frame", {
  g <- gen.structure (
       list ( gen.c.of(4, gen.sample(2:10))
            , gen.c.of(4, gen.sample(2:10))
            , c('a', 'b', 'c', 'd')
            )
       , names = c("a","b", "constant")
       , class = "data.frame"
       , row.names = c("1", "2", "3", "4" ))
  forall( g, function(x) nrow(x) == 4)
})
