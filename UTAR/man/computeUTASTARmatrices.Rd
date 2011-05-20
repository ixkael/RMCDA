\name{computeUTASTARmatrices}

\alias{computeUTASTARmatrices}

\title{Computes UTASTAR constraint matrices (UTA model)}

\description{Given a preference information and model parameters, computes the equality and inequality matrices and vectors related to the polyhedron of admissible value functions}

\usage{
	computeUTASTARmatrices( performanceTable , ranking , delta , prefPairs , indiffPairs , prefDirections , segmentation = FALSE  )
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
	\item{c}{Vector : objective function}
	\item{A}{Matrix : inequality matrix}
	\item{b}{Vector : inequality right-member}
	\item{Aeq}{Matrix : equality matrix}
	\item{beq}{Vector : inequality right-member}
	}

\examples{}