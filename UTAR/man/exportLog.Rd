\name{exportLog}

\alias{exportLog}

\title{Exports LOG messages}

\description{Exports log error or method messages according to XMCDA standard}

\usage{ 
	exportLog( message, location, error=TRUE )
	}

\arguments{
	\item{message}{String containing the output message}
	\item{location}{Where you want the XML file to be saved}
	\item{error}{If it is an error message, use the tag "errorMessage". If it isn't, use the tag "logMessage".}
	}

\value{
	No output (except the XML file)
	}

\examples{}
