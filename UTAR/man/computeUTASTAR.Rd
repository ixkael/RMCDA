\name{computeUTASTAR}

\alias{computeUTASTAR}

\title{Computes UTASTAR disaggregation method}

\description{Meta-method to compute and solve the UTA linear programming problem.}

\usage{
	out <- computeUTASTAR( data )
	}

\arguments{
	\item{data}{Structures for the computeUTASTARmatrices and solveUTASTAR methods}
	}

\value{
	A list structured as follows:	
	\item{x}{Vector : constructed value function}
	\item{A}{Matrix : inequality constraints matrix}
	\item{b}{Vector : inequality constraints right-member}
	\item{Aeq}{Matrix : equality constraints matrix}
	\item{beq}{Vector : inequality constraints right-member}
	}

\examples{}