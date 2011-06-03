\name{nextpair_random}

\alias{nextpair_random}

\title{Random selection method}

\description{Randomly selects the next pair in the set of possible questions}

\usage{
	out <- nextpair_random( L )
	}

\arguments{
	\item{L}{Set of possible questions/pairs, where each row corresponds to a pair of alternatives and is the concatenation of their evaluation profiles.}
	}

\value{
	A list structured as follows:
	\item{pos}{Row number of the selected pair in L}
	\item{pair}{Selected pair, i.e. concatenation of the evaluation profiles}
	}

\examples{}