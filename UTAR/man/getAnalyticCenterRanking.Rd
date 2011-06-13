\name{getAnalyticCenterRanking}

\alias{getAnalyticCenterRanking}

\title{Computes analytic center ranking}

\description{Computes the ranking corresponding to the analytic center of the manipulated polyhedron.}

\usage{
	out <- getAnalyticCenterRanking( M )
	}

\arguments{
	\item{M}{A model properly initialized and enriched with other functions of UTAR}
	}

\value{
	a list structured as follows :
	\item{acranking}{The AC ranking, i.e. a two-column matrix containing alternatives and their rank}
	\item{uac}{The value function corresponding to the analytic center}
	}

\examples{}