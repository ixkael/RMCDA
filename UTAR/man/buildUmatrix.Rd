\name{buildUmatrix}


\alias{buildUmatrix}

\title{Value functions reconstruction}

\description{
	Constructs the Umatrix (value functions ordinates) given the vector solution from UTA or ACUTA.
	}

\usage{
	buildUmatrix(u,segs)
	}

\arguments{
	\item{x}{Solution in the constraint polyhedron, arising from UTA or ACUTA}
	\item{segs}{Vector : number of segments in each value function}
	}

\value{
	A matrix containing the ordinates of the value functions, with the proper number of breakpoints and dimensions
	}

\examples{}