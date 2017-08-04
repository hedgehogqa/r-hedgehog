library(R6)
library(testthat)

###############################
# State machine testing demo  #
###############################

# Definition of our New command.
# Creates a reference which we
# can write into
new  <- command ( "New",
    generator = function( state ) list()
  , execute   = function() grefs$newRef()
  , update    = function( state, output )
      snoc( state, list(pid = output, val = 0))
  )

# Definition of our Read command.
# Reads the value of the reference.
read <- command ( "Read",
    generator = function( state ) {
      if ( length(state) == 0 )
        return(NULL)
      list(
        pid = gen.map(function(i) i$pid, gen.sample( state ))
      )}
  , require = function( state, pid )
      !is.null ( Find( function( proc ) { proc$pid == pid } , state ) )
  , execute = function( pid ) grefs$readRef(pid)
  , ensure  = function( state, output, pid ) {
      expected <- Find( function( proc ) { proc$pid == pid } , state )$val
      expect_equal( expected, output)
    }
  )

# Definition of our Write command
# Writes a new value to a reference.
write <- command ( "Write",
    generator = function( state ) {
      if ( length(state) == 0 )
        return(NULL)
      list (
        pid = gen.map( function(i) i$pid, gen.sample( state ))
      , val = gen.sample.int(10)
      )}
  , require = function( state, pid, val )
      !is.null ( Find( function( proc ) { proc$pid == pid } , state ) )
  , execute = function( pid, val ) grefs$writeRef( pid, val )
  , update  = function( state, output, pid, val )
      lapply( state, function(proc)
        if (proc$pid == pid) list(pid = proc$pid, val = val) else proc
      )
  )

# One can also not use the helper function "command"
# and write the function as a list.
inc <- command ( title = "Inc",
    generator = function( state ) {
      if ( length(state) == 0 )
        return(NULL)
      list (
        pid = gen.map( function(i) i$pid, gen.sample( state ))
      )}

  , require = function( state, pid )
      !is.null ( Find( function( proc ) { proc$pid == pid } , state ) )

  , execute = function( pid ) {
        val <- grefs$readRef(pid)
        grefs$writeRef( pid, val + 1 )
      }
  , update = function( state, output, pid )
      lapply( state, function(proc)
        if (proc$pid == pid) list(pid = proc$pid, val = proc$val + 1) else proc
      )
)


# Initial state
# Our state is just the list of references
# and their expected values.
initialstate <- list()

###################################
# Object oriented code under test #
###################################

refs <- R6Class("Refs",
  public = list(
    newRef = function() {
      private$num <- private$num + 1
      private$refs[[private$num]] <- 0
      return ( private$num )
    }
    , readRef = function(i) {
      return ( private$refs[[i]] )
    }
    , writeRef = function(i, a) {
      private$refs[[i]] <- a
      NULL
    }
    , reset = function() {
      private$num = 0
      private$refs = list()
      NULL
    }
    )
  , private = list( num = 0 , refs = list() )
)
grefs <- refs$new()

#######################
# Property Definition #
#######################
snoc <- function (xs, x) {
  unlist ( list ( xs, list( x)) , recursive = F )
}

test_that( "Actions hold",
  forall( gen.actions ( initialstate, list(new, read, write, inc) ), function( actions ) {
    grefs$reset()
    executeSequential( initialstate, actions )
  })
)
