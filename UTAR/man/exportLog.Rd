\name{exportLog}

\alias{exportLog}

\title{Exports log messages}

\description{Exports error or method messages according to XMCDA standard.}

\usage{ 
	exportLog( message, location, error=TRUE )
	}

\arguments{
	\item{message}{String containing the output message}
	\item{location}{Where you want the XML file to be saved}
	\item{error}{TRUE if it is an error message (tag "errorMessage"), and FALSE if it isn't (tag "logMessage").}
	}

\value{
	No output (except the XML file)
	}

\examples{}
