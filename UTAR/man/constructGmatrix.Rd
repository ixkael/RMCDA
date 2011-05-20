\name{constructGmatrix}

\alias{constructGmatrix}

\title{Constructs G matrices}

\description{
	Constructs the Gmatrix (value functions abscissa) given the vector solution from UTA or ACUTA.
	}

\usage{
	constructGmatrix(u,segs)
	}

\arguments{
	\item{x}{Solution in the constraint polyhedron, arising from UTA or ACUTA}
	\item{segs}{Vector : number of segments in each value function}
	}

\value{
	A matrix containing the abscissa of the value functions, with the proper number of breakpoints and dimensions
	}

\examples{}