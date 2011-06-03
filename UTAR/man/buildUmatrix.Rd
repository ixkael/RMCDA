\name{buildUmatrix}

\alias{buildUmatrix}

\title{Value functions reconstruction}

\description{
	Constructs the Umatrix (ordinates of the value functions) from the vector-solution of UTA or ACUTA.
	}

\usage{
	umatrix <- buildUmatrix(x,segs)
	}

\arguments{
	\item{x}{Solution in the constraint polyhedron, arising from UTA or ACUTA}
	\item{segs}{Vector : number of segments in each value function}
	}

\value{
	\item{umatrix}{A matrix containing the ordinates of the value functions, with the proper number of breakpoints and criteria}
	}

\examples{}