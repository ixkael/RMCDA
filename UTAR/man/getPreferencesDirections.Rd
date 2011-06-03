\name{getPreferencesDirections}

\alias{getPreferencesDirections}

\title{Extracts Preferences Directions}

\description{Extracts the directions (min or max) of utility functions from a \code{XML}  tree, according to \code{XMCDA} format.}

\usage{
	out <- getPreferencesDirections( tree )
	}

\arguments{
	\item{tree}{An \code{XML} tree}
	}

\value{
	A list structured as follows:
	\item{status}{Validation tag ("OK" execution was successful)}
	\item{}{Vector or matrix containing the directions and the criteria names} 
	}

\examples{}