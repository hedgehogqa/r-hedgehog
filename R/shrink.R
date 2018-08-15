

#' Shrink an integral number by edging towards a destination.
#'
#' Note we always try the destination first, as that is the optimal shrink.
#'
#' @export
#' @param destination
#'   the value we want to shrink towards.
#'
#' @examples
#' shrink.towards (0) (100)
#' # [0,50,75,88,94,97,99]
#'
#' shrink.towards(500)(1000)
#' # [500,750,875,938,969,985,993,997,999]
#'
#' shrink.towards (-50) (-26)
#' # [-50,-38,-32,-29,-27]
shrink.towards <- function(destination) {
    function(x) {
        if (destination == x) {
            c()
        } else {
            # Halve the operands before subtracting them so they don't overflow.
            diff <- x - destination
            c(destination, x - shrink.halves(diff))
        }
    }
}

#' Shrink a number by dividing it into halves.
#'
#' @export
#' @param x number to produce halves of
#'
#' @examples
#' shrink.towards(45)
#' # 22 11  5  2  1
shrink.halves <- function(x) {
    if (abs(x) < 2) {
        c()
    } else {
        c(x%/%2, shrink.halves(x%/%2))
    }
}

#' Shrink a list by edging towards the empty list.
#'
#' @export
#' @param xs
#'   the list to shrink
shrink.list <- function(xs) {
    len     <- length(xs)
    remnums <- shrink.halves(len)
    new <- lapply(c(len, remnums), function(rn) {
        shrink.removes(rn, xs)
    })
    unlist(new, recursive = F)
}

#' Produce permutations of removing num elements from a list.
#'
#' @export
#' @param num
#'   the number of values to drop
#' @param xs
#'   the list to shrink
shrink.removes <- function(num, xs) {
    lapply(as.list(1:(length(xs) - num + 1)), function(s) {
        xs[-c(s:(s + num - 1))]
    })
}
