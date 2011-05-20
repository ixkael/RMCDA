\name{meanValue}

\alias{meanValue}

\title{Computes the "mean value" post-optimality method}

\description{MeanValue is a post-optimality method taking a set of constraints and finding the compatible value functions that corresponds to a (geometric) central point in the constraints polyhedron}

\usage{
	meanValue(A,b,Aeq,beq,segs)
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