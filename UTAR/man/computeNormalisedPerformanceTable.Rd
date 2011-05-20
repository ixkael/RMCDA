\name{computeNormalisedPerformanceTable}

\alias{computeNormalisedPerformanceTable}

\title{Computes normalised performance table}

\description{Based on value (utility) functions, evaluate and normalize a performanceTable}

\usage{
	normalisedTable <- computeNormalisedPerformanceTable(performanceTable,gmatrix,umatrix)
	}

\arguments{
	\item{data}{Classical performanceTable according to XMCDA standard. Matrix containing evaluations of alternatives on different criteria}
	\item{gmatrix}{Matrix containing the abcsissa of the piecewize value functions}
	\item{umatrix}{Matrix containing the ordinates of the piecewize value functions}
	}

\value{
	\item{performanceTable}{Classical but normalised performanceTable according to XMCDA standard. Matrix containing evaluations of alternatives on different criteria}
	}

\examples{}