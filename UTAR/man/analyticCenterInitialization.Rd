\name{analyticCenterInitialization}
\alias{analyticCenterInitialization}
\title{
	Initialize the analytic center. This method is included into the analyticCenter function.
	}
\description{}
\usage{
	analyticCenterInitialization(x,A,b)
	}
\arguments{
	\item{x}{a solution in the constraint polyhedron (for example using lpSolve)}
	\item{A}{Matrix : inequality matrix}
	\item{b}{Vector : inequality right-member}
	}
\value{
	\item{x}{The solution (vectorized value function)}
	\item{flag}{Success or failure tag}
	}
\examples{}