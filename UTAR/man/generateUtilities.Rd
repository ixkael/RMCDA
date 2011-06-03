\name{generateUtilities}

\alias{generateUtilities}

\title{Generate random value functions}

\description{Generate a random set of marginal value functions from the number of criteria and the number of segments in each function.}

\usage{
	out <- generateUtilities( nbcrit, nbnodes )
	}

\arguments{
	\item{nbcrit}{The number of criteria}
	\item{nbnodes}{The number of segments in each value function}
	}

\value{
	A list structured as follows:
	\item{gmatrix}{A matrix containing the abscissa of the value functions, with the proper number of breakpoints and criteria}
	\item{umatrix}{A matrix containing the ordinates of the value functions, with the proper number of breakpoints and criteria}
	}

\examples{}