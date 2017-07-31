
#' A symbolic value.
#'
#' These values are the outputs of a computation
#' during the calculations' construction, and
#' allow a value to use the results of a previous
#' function.
#'
#' Really, this is just an integer, which we use
#' as a name for a value which will exist later
#' in the computation
symbolic <- function(x) {
  structure (x, class = c(class(x), "symbolic"))
}

print.symbolic <- function ( var ) {
  cat ( paste( var, "(symbolic)"  ))
}

# Pretty printer for an action (this is what will)
# be shown during shrinking.
print.action <- function ( action ) {
  cat ( action$title, "\n" )
  cat ( "inputs:\n" )
  print ( action$input )
  cat ( paste ( "output variable:", action$output, "\n" ))
}

#' Build a commmand
#'
#' @export
#'
#' @param gen A generator which provides random arguments
#'   for the command, given the current (symbolic) state.
#'   If nothing can be done with the current state, one
#'   should preclude the situation with a requires and
#'   return NULL. Otherwise, it should be a list of
#'   arguments (the empty list is ok for functions which
#'   take no arguments).
#' @param execute A function from the concrete input,
#'   which executes the true function and returns
#'   concrete output.
#'   Function takes the (possibly named) arguments given
#'   by the generator.
#' @param require A function from the current (symbolic)
#'   state to a bool, indicating if action is currently
#'   applicable.
#'   Function also takes the (possibly named) arguments
#'   given by the generator (this is mostly used in
#'   shrinking, to ensure after a shrink it's still ok).
#' @param update A function from state to state, which is
#'   polymorphic over symbolic and concrete inputs and
#'   outputs (as it is used in both action generation and
#'   command execution).
#'   It's critical that one doesn't "inspect" the values
#'   going into the state for this function.
#' @param ensure A post-condition for a command that must be
#'   verified for the command to be considered a success.
command  <- function( title
                    , generator
                    , execute
                    , require = function(state, ...) T
                    , update  = function(state, output, ...) state
                    , ensure  = function(state, output, ...) T
                    ) {
  gen_     <- match.fun ( generator )
  execute_ <- match.fun ( execute )
  require_ <- match.fun ( require )
  update_  <- match.fun ( update )
  ensure_  <- match.fun ( ensure )
  structure (
    list (
        title   = title
      , gen     = gen_
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
      require_ <- partial( command$require, state = state )
      if (! do.call( require_, as.list( input ) ) )
        stop ( "Command generation arguments voilate requirements" )

      # Get a variable name we'll use for the output
      # We just use sequential values, as we'll add
      # them to the environment list here.
      output   <- symbolic ( counter )

      # Build a new state which we can work with
      update_  <- partial( command$update, state = state, output = output)
      state_   <- do.call( update_ , as.list( input ))

      # Build the action which can be run.
      action_  <- structure(list (
          title   = command$title
        , input   = input
        , output  = output
        , execute = command$execute
        , require = command$require
        , update  = command$update
        , ensure  = command$ensure
      ), class = "action")

      # Return the new symbolic state and the action to run.
      list ( acc = list(state = state_, counter = counter + 1), action = action_ )
    }, command$gen( state ))
  })
}}

# Returns the actions and it's updates to the state
# only if they're currently valid.
check.valid  <- function ( ok, state, action ) {
  require_ <- partial( action$require, state = state )
  if (do.call( require_, as.list( action$input ) )) {
    update_ <- partial( action$update, state = state, output = action$output)
    state_  <- do.call( update_ , as.list( action$input ))
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
          tree.replicateS ( num, function(x) { gen.action(commands)(x$state, x$counter)$unGen(size) }, list(state = initial.state, counter = 1))
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
  input   <- reify( action$input, env )
  output  <- do.call( action$execute, as.list(input) )
  update_ <- partial( action$update, state = state, output = output)
  state_  <- do.call( update_ , as.list( input ))
  env[[action$output]] <- output

  ensure_ <- partial( action$ensure, state = state, output = output)
  result_ <- do.call( ensure_ , as.list( input ))

  list( state = state_
      , environment = env
      , result = result_
      )
}

#' Execute a set of commands sequentially, ensuring
#' that all postconditions hold.
executeSequential <- function ( initial.state, actions ) {
  final <- Reduce ( function( acc, action ) {
    if ( testable_success( as.testable (acc$result ))) {
      execute( acc$state, acc$environment, action )
    } else acc
  }, init = list( state = initial.state, environment = list(), result = T)
   , actions )
  final$result
}
