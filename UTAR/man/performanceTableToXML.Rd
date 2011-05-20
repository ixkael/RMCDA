\name{performanceTableToXML}

\alias{performanceTableToXML}

\title{Converts performanceTable into XMCDA tree}

\description{Create an XML tree (according to XMCDA standard) based on performanceTable given as an input.}

\usage{
	performanceTableToXML( performanceTable )
	}

\arguments{
	\item{data}{Classical performanceTable according to XMCDA standard, that may be saved under XML. Matrix containing evaluations of alternatives on different criteria.}
	}

\value{
	XML tree that might be saved using \code{saveXML()}
	}

\examples{}