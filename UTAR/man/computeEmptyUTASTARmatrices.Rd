\name{computeEmptyUTASTARmatrices}

\alias{computeEmptyUTASTARmatrices}

\title{Computes empty UTASTAR matrices}

\description{Computes the linear programming system corresponding the an empty UTA model}

\usage{
	out <- computeEmptyUTASTARmatrices( prefDirections )
	}

\arguments{
	\item{prefDirections}{Maximization direction and number of segments in each dimension/criterion}
	}

\value{
	A list structured as follows:
	\item{A}{Matrix : inequality matrix}
	\item{b}{Vector : inequality right-member}
	\item{Aeq}{Matrix : equality matrix}
	\item{beq}{Vector : inequality right-member}
	\item{validation}{Validation flag}
	}

\examples{}