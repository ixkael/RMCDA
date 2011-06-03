\name{initializeModel}

\alias{initializeModel}

\title{Initializes an empty UTA model}

\description{Initializes an empty UTA model}

\usage{
	out <- initializeModel( A, alpha )
	}

\arguments{
	\item{A}{Initial set of alternatives}
	\item{alpha}{The number of segmens in each value function to construct}
	}

\value{
	The function returns a list structured as follows:
	\item{nalts}{The number of alternatives in A}
	\item{nbcrit}{The number of criteria}
	\item{nbnodes}{The number of segmens in each value function to construct}
	\item{delta}{Utility gap between two alternatives for which one was declared to be preferred to the other}
	\item{combins}{Pairwise possibilities for alternatives in A}
	\item{orientedSegment}{Maximization direction and number of segments in each dimension/criterion}
	\item{alternativesPreferences}{Empty set of pairwise comparisons of alternatives from A} 
	\item{alternativesIndifferences}{Empty set of pairwise comparisons of alternatives from A}
	\item{alternativesRanking}{Empty ranking of alternatives from A}
	\item{gmatrix}{A matrix containing the abscissa of the value functions, with the proper number of breakpoints and criteria}
	\item{initialPerformanceTable}{Simply the initial set A}
	\item{necess}{The necessary relation induced on A by the empty model}
	\item{A}{Matrix : inequality constraints matrix}
	\item{b}{Vector : inequality constraints right-member}
	\item{A}{Matrix : equality constraints matrix}
	\item{b}{Vector : equality constraints right-member}
	}

\examples{}