\name{performanceTableToXML}

\alias{performanceTableToXML}

\title{Converts a performanceTable into an \code{XML} tree}

\description{Creates an XML tree containing a given performanceTable according to \code{XML-XMCDA} standard.}

\usage{
	out <- performanceTableToXML( performanceTable )
	}

\arguments{
	\item{data}{Classical performanceTable according to \code{XMCDA} standard, that may be saved under \code{XML}. Matrix containing evaluations of alternatives on different criteria.}
	}

\value{
	\code{XML} tree that might be saved using \code{saveXML()}
	}

\examples{}