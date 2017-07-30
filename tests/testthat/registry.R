
library(R6)

# Testing of object oriented code with
# a pure state.
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
    , dump = function() {
      private$refs
    }
    )
  , private = list( num = 0 , refs = list() )
)

grefs <- refs$new()

# Initial state
initialstate <- list()

# Not really needed, but a neat way of saying what
# our functions are.
ticket <- function(type, ...) {
  type <- match.arg(type, c("new", "read", "write", "inc"))
  structure( list(...), class = c( paste0("ticket_", type), "ticket"))
}

# Definition of our New command
new  <- command (
    generator = function( state ) gen.pure(ticket("new"))
  , execute   = function( input ) grefs$newRef()
  , update    = function( state, input, output )
      snoc( state, list(pid = output, val = 0))
  )

# Definition of our Read command
read <- command (
    generator = function( state ) {
      if ( length(state) == 0 )
        return(NULL)
      gen.map(
        function(i) ticket("read", i$pid), gen.sample( state )
      )}
  , require = function( state, input )
      !is.null ( Find( function( proc ) { proc$pid == input } , state ) )
  , execute = function( input ) grefs$readRef(input)
  , ensure  = function( state, input, output )
      Find( function( proc ) { proc$pid == input } , state )$val == output
  )


# Definition of our Read command
write <- command (
    generator = function( state ) {
      if ( length(state) == 0 )
        return(NULL)
      list (
        pid = gen.map( function(i) i$pid, gen.sample( state ))
      , val = gen.sample.int(10)
      )}
  , require = function( state, input )
      !is.null ( Find( function( proc ) { proc$pid == input$pid } , state ) )
  , execute = function( pid, val ) grefs$writeRef( pid, val )
  , update  = function( state, input, output )
      lapply( state, function(proc) if (proc$pid == input$pid) list(pid = proc$pid, val = input$val))
  )

# Run the sequence, ensuring all post conditions hold
forall( gen.actions ( initialstate, list(new, read, write) ), function( actions ) {
    grefs$reset()
    executeSequential( initialstate, actions)
  })
