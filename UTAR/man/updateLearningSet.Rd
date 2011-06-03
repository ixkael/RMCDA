\name{updateLearningSet}

\alias{updateLearningSet}

\title{Updates learning set}

\description{Updates the set of possible questions by removing the previously asked question and the sure statements arising from the updated necessary relation on A.}

\usage{
	out <- updateLearningSet( L, M, pos )
	}

\arguments{
	\item{M}{A model properly initialized and enriched with other functions of UTAR}
	\item{L}{Set of possible questions/pairs, where each row corresponds to a pair of alternatives and is the concatenation of their evaluation profiles.}
	\item{pos}{The position of the selection question in L, in order to remove it}
	}

\value{The updated set of possible questions/pairs, where each row corresponds to a pair of alternatives and is the concatenation of their evaluation profiles. }

\examples{}