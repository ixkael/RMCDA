\name{computeNormalizedPerformanceTable}

\alias{computeNormalizedPerformanceTable}

\title{Computes normalized performance table}

\description{
	Normalizes a performanceTable using given value functions.
	}

\usage{
	normalizedTable <- computeNormalizedPerformanceTable(performanceTable,gmatrix,umatrix)
	}

\arguments{
	\item{data}{Classical performanceTable according to MCDA standards. Matrix containing evaluations of alternatives on different criteria}
	\item{gmatrix}{Matrix containing the abcsissa of the piecewize value functions}
	\item{umatrix}{Matrix containing the ordinates of the piecewize value functions}
	}

\value{
	\item{normalizedTable}{Normalised performanceTable. Matrix containing evaluations of alternatives on different criteria}
	}

\examples{}