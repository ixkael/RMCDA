\name{computeUTASTARmatrices}

\alias{computeUTASTARmatrices}

\title{Computes UTASTAR constraints matrices}

\description{Given a preference information and model parameters, computes the equality and inequality matrices and vectors related to the polyhedron of admissible value functions of UTASTAR.}

\usage{
	out <- computeUTASTARmatrices( performanceTable , ranking , delta , prefPairs , indiffPairs , prefDirections , segmentation = FALSE  )
	}

\arguments{
	\item{performanceTable}{Classical performanceTable according to MCDA standard}
	\item{ranking}{Total order of a subset of alternatives}
	\item{delta}{Utility gap between two alternatives for which one was declared to be preferred to the other}
	\item{prefPairs}{Pairs of alternatives, involved in preference statements}
	\item{indiffPairs}{Pairs of alternatives, involved in indifference statements}
	\item{prefDirections}{Vector : number of segments in each value function}
	}

\value{
	A list structured as follows:
	\item{c}{Vector : objective constraints function}
	\item{A}{Matrix : inequality constraints matrix}
	\item{b}{Vector : inequality constraints right-member}
	\item{Aeq}{Matrix : equality constraints matrix}
	\item{beq}{Vector : inequality constraints right-member}
	}

\examples{}