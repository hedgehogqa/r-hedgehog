

#' Shrink an integral number by edging towards a destination.
#'
#'  >>> towards (0) (100)
#'  [0,50,75,88,94,97,99]
#'
#'  >>> towards(500)(1000)
#'  [500,750,875,938,969,985,993,997,999]
#'
#'  >>> towards (-50) (-26)
#'  [-50,-38,-32,-29,-27]
#'
#'  /Note we always try the destination first, as that is the optimal shrink./
towards <- function ( destination ) {
  function ( x ) {
    if ( destination == x ) {
      c()
    } else {
      # Halve the operands before subtracting them so they don't overflow.
      diff <- x - destination
      c(destination, x - halves (diff))
    }
  }
}

#' Shrink a number by dividing it into halves.
#'
#'  >>> halves(45)
#'  22 11  5  2  1
halves <- function ( x ) {
  if (abs(x) < 2) {
    c()
  } else {
    c( x %/% 2, halves ( x %/% 2) )
  }
}

#' Shrink a list by edging towards the empty list.
#'
#' >>> list [1,2,3]
#' [[],[2,3],[1,3],[1,2]]
#'
#' >>> list "abcd"
#' ["","cd","ab","bcd","acd","abd","abc"]
#'
#' /Note we always try the empty list first, as that is the optimal shrink./
#' list :: [a] -> [[a]]
#' list xs =
#'  concatMap
#'    (\k -> removes k xs)
#'    (halves $ length xs)
shrink.list <- function ( xs ) {
  len     <- length ( xs )
  remnums <- halves ( len )
  new     <- lapply ( c(len, remnums), function (rn) {
    removes( rn, xs )
  })
  unlist( new, recursive = F )
}

#' Produce all permutations of removing 'k' elements from a list.
removes <- function ( num, xs ) {
  lapply ( as.list( 1:(length(xs) - num + 1) ), function (s) {
    xs[-c(s:(s+num-1))]
  })
}
