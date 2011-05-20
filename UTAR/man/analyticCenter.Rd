\name{analyticCenter}

\alias{analyticCenter}

\title{Analytic center computation}

\description{
	This method computes the analytic center of the constraint polyhedron. It requires a first solution (x) in this polyhedron, that can be obained using simple linear programming techniques, such as the lpSolve package. The A-Aeq matrices are the inequality-equality matrices
	}
	
\usage{
	analyticCenter(x,A,b,Aeq)
	}
	
\arguments{
	\item{x}{a solution in the constraint polyhedron (for example using lpSolve)}
	\item{A}{Matrix : inequality matrix}
	\item{b}{Vector : inequality right-member}
	\item{Aeq}{Matrix : equality matrix}
	}
	
\value{
	\item{x}{The solution (vectorized value function)}
	\item{validation}{Success or failure tag}
	\item{LOG}{Log information}
	}
	
\examples{}