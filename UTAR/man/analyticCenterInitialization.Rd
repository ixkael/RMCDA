\name{analyticCenterInitialization}

\alias{analyticCenterInitialization}

\title{Initializes the analytic center}
	
\description{
	Initializes the analytic center. This method is included into the \code{analyticCenter()} function.
	}

\usage{
	xout <- analyticCenterInitialization(x,A,b)
	}
	
\arguments{
	\item{x}{a solution in the constraint polyhedron (for example using lpSolve)}
	\item{A}{Matrix : inequality matrix}
	\item{b}{Vector : inequality right-member}
	}
	
\value{
	A list structured as follows:
	\item{x}{The solution (vectorized value function)}
	\item{flag}{Success or failure tag}
	}
	
\examples{}