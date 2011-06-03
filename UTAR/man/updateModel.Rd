\name{updateModel}

\alias{updateModel}

\title{Update UTA model}

\description{Updates UTA model by integrating a new preference statement}

\usage{
	out <- updateModel( M, nextpair, U )
	}

\arguments{
	\item{M}{UTA model, correctly initialized}
	\item{nextpair}{The nextpair, obtained from a selection procedure (concatenation of two evaluation profiles)}
	\item{U}{The decision maker's value function, in order to obtain his judgement on the next pair}
}

\value{Updated model}

\examples{}