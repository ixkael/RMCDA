\name{computeranking}

\alias{computeranking}

\title{Computes a ranking}

\description{Computes a ranking from a value function applied to a performance table.}

\usage{
	out <- computeranking( perfTable, gmatrix, umatrix )
	}

\arguments{
	\item{data}{Classical performanceTable according to \code{XMCDA} standard, that may be saved under \code{XML}. Matrix containing evaluations of alternatives on different criteria}
	\item{gmatrix}{A matrix containing the abscissa of the value functions, with the proper number of breakpoints and criteria}
	\item{umatrix}{A matrix containing the ordinates of the value functions, with the proper number of breakpoints and criteria}
	}

\value{
	A ranking, under the form of a two-column matrix
	}

\examples{}