\name{meanValue}

\alias{meanValue}

\title{Computes the UTASTAR post-optimality method}

\description{UTASTAR is a post-optimality method taking a set of constraints and finding the compatible value functions that corresponds to a (geometric) central point in the constraints polyhedron.}

\usage{
	out <- meanValue(A,b,Aeq,beq,segs)
	}

\arguments{
	\item{A}{Matrix : inequality constraints matrix}
	\item{b}{Vector : inequality constraints right-member}
	\item{A}{Matrix : equality constraints matrix}
	\item{b}{Vector : equality constraints right-member}
	\item{segs}{Vector : number of segments in each value function}
	}
	
\value{
	A list structured as follows:
	\item{x}{The solution (vectorized value function)}
	\item{validation}{Success or failure tag}
	\item{LOG}{Log information}
	}
	
\examples{}