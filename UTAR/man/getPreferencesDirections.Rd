\name{getPreferencesDirections}

\alias{getPreferencesDirections}

\title{Get Preferences Directions}

\description{Extracts the directions (min or max) of utility functions}

\usage{
	sol <- getPreferencesDirections( tree )
	}

\arguments{
	\item{tree}{XML tree}
	}

\value{
	Returning a list :
	\item{status}{Validation tag ("OK" execution was successful)}
	\item{}{Vector or matrix containing the directions and the criteria names} 
	}

\examples{}