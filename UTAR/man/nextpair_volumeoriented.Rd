\name{nextpair_volumeoriented}

\alias{nextpair_volumeoriented}

\title{Volume-oriented earning strategy}

\description{Selects the next pair in the set of possible questions using the Volume-oriented method.}

\usage{
	out <- nextpair_volumeoriented( L, M )
	}

\arguments{
	\item{L}{Set of possible questions/pairs, where each row corresponds to a pair of alternatives and is the concatenation of their evaluation profiles.}
	\item{M}{A model properly initialized and enriched with the other functions of UTAR}
	}

\value{
	A list structured as follows:
	\item{pos}{Row number of the selected pair in L}
	\item{pair}{Selected pair, i.e. concatenation of the evaluation profiles}
	}

\examples{}