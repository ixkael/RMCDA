\name{utamp}

\alias{utamp}

\title{Computes "Utamp" post-optimality analysis}

\description{Utamp is a post-optimality method taking a set of constraints and finding the compatible value functions that maximize the gap between two alternatives (where one is declared to be preferred to the other)}

\usage{
	utamp(A,b,Aeq,beq,segs)
	}

\arguments{
	\item{x}{a solution in the constraint polyhedron (for example using lpSolve)}
	\item{A}{Matrix : inequality matrix}
	\item{b}{Vector : inequality right-member}
	\item{Aeq}{Matrix : equality matrix}
	\item{segs}{Vector : number of segments in each value function}
	}
	
\value{
	\item{x}{The solution (vectorized value function)}
	\item{validation}{Success or failure tag}
	\item{LOG}{Log information}
	}
	
\examples{}