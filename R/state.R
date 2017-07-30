
#' A symbolic value.
#' Really, this is just an integer, which we use
#' as a name for a value which will exist later
#' in the computation
symbolic <- function(x) {
  structure (x, class = c(class(x), "symbolic"))
}

#' A concrete value resulting from a real computations
concrete <- function(x) {
  structure (x, class = c(class(x), "concrete"))
}

#' Build a commmand
#'
#' @export
#'
#' @param gen A generator which provides random arguments
#'   for a command, given the current (symbolic) state.
#'   If nothing can be done with the current state, one
#'   should preclude the situation with a requires and
#'   return NULL. Otherwise, it should be a list of
#'   arguments (the empty list is ok for functions which
#'   take no arguments).
#' @param execute A function from the concrete input,
#'   which executes the true function and returns
#'   concrete output.
#' @param require A function from the current (symbolic)
#'   state to a bool, indicating if action is currently
#'   applicable
#' @param update A function from state to state, which is
#'   polymorphic over symbolic and concrete inputs and
#'   outputs ( as it is used in both action generation and
#'   command execution ).
#' @param ensure A post-condition for a command that must be
#'   verified for the command to be considered a success.
command  <- function( generator
                    , execute
                    , require = function(state, input) T
                    , update  = function(state, input, output) state
                    , ensure  = function(state, input, output) T
                    ) {
  gen_     <- match.fun ( generator )
  execute_ <- match.fun ( execute )
  require_ <- match.fun ( require )
  update_  <- match.fun ( update )
  ensure_  <- match.fun ( ensure )
  structure (
    list (
        gen     = gen_
      , execute = execute_
      , require = require_
      , update  = update_
      , ensure  = ensure_
    )
    , class = "command"
  )
}

#' Reify Structures
#'
#' Convert a symbolic structure to a concrete one,
#' using the provided environment.
#'
#' @param x A structure holding symbolic variables,
#'   which will be replaced with concrete values
#'   from the environment.
#' @param environment a map from symbolic to concrete
#'   values.
reify <- function( x, env ) {
  if ( inherits( x, "symbolic") ) {
    # We have a single symbolic variable
    # Find it in the environment.
    env[[x]]
  } else if ( is.list( x ) ) {
    # We have a list which may contain
    # symbolic values.
    # Traverse over it, returning the
    # concrete version.
    lapply ( x , function(x_) reify(x_, env) )
  } else {
    # Let constants be.
    x
  }
}

#' Generate an action from a set of commands
gen.action <- function ( commands ) { function ( state, counter ) {
  possible <- Filter ( function(command) {
    !is.null ( command$gen(state) )
  }, commands)
  gen.with ( gen.sample( possible ), function (command) {
    # The (symbolic) input for the command.
    # Essentially this says which values it
    # will read from.
    gen.map ( function(input) {
      # Check the requires condition make sense.
      # These requires functions are needed to
      # ensure we have a good shrink.
      if (! command$require( state, input) )
        stop ( "Command generation arguments voilate requirements" )

      # Get a variable name we'll use for the output
      # We just use sequential values, as we'll add
      # them to the environment list here.
      output   <- symbolic ( counter )

      # Build a new state which we can work with
      state_   <- command$update ( state, input, output )

      # Build the action which can be run.
      action_  <- structure(list (
          input   = input
        , output  = output
        , execute = command$execute
        , require = command$require
        , update  = command$update
        , ensure  = command$ensure
      ), class = "action")

      # Return the new symbolic state and the action to run.
      list ( state = list(state_, counter + 1), action = action_ )
    }, command$gen( state ))
  })
}}

# Returns the actions and it's updates to the state
# only if they're currently valid.
check.valid  <- function ( ok, state, action ) {
  if (action$require ( state, action$input )) {
    state_   <- action$update ( state, action$input, action$output )
    list ( ok = snoc(ok, action), state = state_ )
  } else {
    list ( ok = ok, state = state )
  }
}

# After shrinking we may have an inconsistent state.
# Run the state, ensuring the requirements are still
# good.
drop.invalid <- function ( actions, initial.state ) {
  Reduce ( function( acc, action ) {
    check.valid( acc$ok, acc$state, action )
  }, actions, init = list(ok = list(), initial.state ))$ok
}

#' Generate a list of possible actions.
gen.actions <- function ( initial.state, commands ) {
  gen.map (
    function ( actions ) drop.invalid( actions, initial.state )
  , gen.shrink ( shrink.list,
      gen ( function ( size ) {
        tree.bind( function ( num ) {
          tree.replicateS ( num, function(x) { gen.action(commands)(x[[1]], x[[2]])$unGen(size) }, list(initial.state, 1))
        }, gen.sample(1:size)$unGen(size) )
      })
    )
  )
}

#' Execute an action in an environment.
#'
#' Executes the action in an environment, ensuring
#' all postconditions are met.
execute <- function ( state, env, action ) {
  input  <- reify( action$input, env )
  output <- do.call( action$execute, as.list(input) )
  state_ <- action$update ( state, input, output )
  env[[action$output]] <- output

  list( state = state_
      , environment = env
      , result = action$ensure(state_,input,output)
      )
}

#' Execute a set of commands sequentially, ensuring
#' that all postconditions hold.
executeSequential <- function ( initial.state, actions ) {
  final <- Reduce ( function( acc, action ) {
    if ( acc$result ) {
      execute( acc$state, acc$environment, action )
    } else acc
  }, init = list( state = initial.state, environment = list(), result = T)
   , actions )
  final$result
}
