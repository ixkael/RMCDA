\name{analyticCenter}

\alias{analyticCenter}

\title{Computes the analytic center}

\description{
	Computes the analytic center of a polyhedron defined with equality and inequality constraints. It requires a first solution (x) in this polyhedron, which may be obained using simple linear programming techniques, such as the lpSolve package.
	}
	
\usage{
	xout <- analyticCenter(x,A,b,Aeq)
	}
	
\arguments{
	\item{x}{a solution in the constraint polyhedron (for example using lpSolve)}
	\item{A}{Matrix : inequality matrix}
	\item{b}{Vector : inequality right-member}
	\item{Aeq}{Matrix : equality matrix}
	}
	
\value{
	A list structured as follows:
	\item{x}{The solution (vectorized value function)}
	\item{validation}{Success or failure tag}
	\item{LOG}{Log information}
	}
	
\examples{}