% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/state.R
\name{gen.actions}
\alias{gen.actions}
\title{Generate a list of possible actions.}
\usage{
gen.actions(initial.state, commands)
}
\arguments{
\item{initial.state}{the starting state to
build from which is appropriate for this
state machine generator.}

\item{commands}{the list of commands which
we can select choose from. Only commands
appropriate for the state will actually be
selected.}
}
\value{
a list of actions to run during testing
}
\description{
Generate a list of possible actions.
}
