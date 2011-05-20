\name{computeUTASTAR}

\alias{computeUTASTAR}

\title{Computes UTASTAR disaggregation method}

\description{Meta-method to compute and solve the UTA linear programming problem}

\usage{
	computeUTASTAR( data )
	}

\arguments{
	\item{data}{Structures for the computeUTASTARmatrices and solveUTASTAR methods}
	}

\value{
	List containing the extracted content in appropriate format :
	\item{x}{Vector : constructed value function}
	\item{A}{Matrix : inequality matrix}
	\item{b}{Vector : inequality right-member}
	\item{Aeq}{Matrix : equality matrix}
	\item{beq}{Vector : inequality right-member}
	}

\examples{}