\name{computePostOptimalityAnalysis}

\alias{computePostOptimalityAnalysis}

\title{Computes post-optimality analysis}

\description{Computes the post-optimality analysis from a given solution (interior point of the constraints polyhedron) for UTA models. The three available methods are Analytic center, UTASTAR, and UTAMP.}

\usage{
	xout <- computePostOptimalityAnalysis( data )
	}

\arguments{
	\item{data}{A list containing equality and inequatily matrices ( Aeq and A ) and vectors ( beq and b ) as well as segs information and choosen method ("ac", "mv" or "md")}
	}

\value{
	A list structured as follows:
	\item{x}{The solution (vectorized value function)}
	\item{validation}{Success or failure tag}
	\item{LOG}{Log information}
	}

\examples{}