\name{nextpair_visionary}

\alias{nextpair_visionary}

\title{Visionary learning strategy}

\description{Selects the next pair in the set of possible questions using the visionary method (oracle).}

\usage{
	out <- nextpair_visionary( L, M, U )
	}

\arguments{
	\item{L}{Set of possible questions/pairs, where each row corresponds to a pair of alternatives and is the concatenation of their evaluation profiles.}
	\item{M}{A model properly initialized and enriched with the other functions of UTAR}
	\item{U}{The decision maker's value function, in order to obtain his judgement on the next pair}
	}

\value{
	A list structured as follows:
	\item{pos}{Row number of the selected pair in L}
	\item{pair}{Selected pair, i.e. concatenation of the evaluation profiles}
	}

\examples{}