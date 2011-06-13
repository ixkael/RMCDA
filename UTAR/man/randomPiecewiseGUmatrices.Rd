\name{randomPiecewiseGUmatrices}

\alias{randomPiecewiseGUmatrices}

\title{Generate random piecewise value functions}

\description{Generates random piecewise value functions from given performance table and model parameters}

\usage{
	out <- randomPiecewiseGUmatrices( prefDirs, performanceTable )
	}

\arguments{
	\item{prefDirs}{Vector or matrix containing the directions and the criteria names} 
	\item{performanceTable}{Classical performanceTable according to MCDA standard}
	}

\value{
	A list structured as follows:
	\item{gmatrix}{A matrix containing the abscissa of the value functions, with the proper number of breakpoints and criteria}
	\item{umatrix}{A matrix containing the ordinates of the value functions, with the proper number of breakpoints and criteria}
	}

\examples{}