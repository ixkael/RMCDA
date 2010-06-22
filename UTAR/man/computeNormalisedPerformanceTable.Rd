\name{computeNormalisedPerformanceTable}

\alias{computeNormalisedPerformanceTable}

\title{Computes normalised performance table}

\description{Based on value (utility) functions, evaluate and normalise a performanceTable.}

\usage{
	normalisedTable <- computeNormalisedPerformanceTable(performanceTable,gmatrix,umatrix)
	}

\arguments{
	\item{data}{Classical performanceTable according to XMCDA standard. Matrix containing evaluations of alternatives on different criteria.}
	\item{gmatrix}{Matrix containing values of criteria on which we have value (utility) functions. Breakpoints of piecewize functions.}
	\item{umatrix}{Matrix containing the values of the piecewize utility functions previously initiated in gmatrix.}
	}

\value{
	\item{performanceTable}{Classical but normalised performanceTable according to XMCDA standard. Matrix containing evaluations of alternatives on different criteria.}
	}

\examples{}