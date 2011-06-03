\name{utamp}

\alias{utamp}

\title{Computes "Utamp" post-optimality analysis}

\description{Utamp is a post-optimality method taking a set of constraints and finding the compatible value functions that maximize the gap between two alternatives (where one is declared to be preferred to the other)}

\usage{
	out <- utamp(A,b,Aeq,beq,segs)
	}

\arguments{
	\item{x}{a solution in the constraint polyhedron (for example using lpSolve)}
	\item{A}{Matrix : inequality constraints matrix}
	\item{b}{Vector : inequality constraints right-member}
	\item{Aeq}{Matrix : equality constraints matrix}
	\item{segs}{Vector : number of segments in each value function}
	}
	
\value{
	A list structured as follows:
	\item{x}{The solution (vectorized value function)}
	\item{validation}{Success or failure flag}
	\item{LOG}{Log information}
	}
	
\examples{}