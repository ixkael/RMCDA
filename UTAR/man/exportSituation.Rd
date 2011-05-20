\name{exportSituation}

\alias{exportSituation}

\title{Exports value functions to XMCDA file}

\description{Exports utility functions (created by function \code{saveUtilityFunctionUnderXML}) according to XMCDA standard}

\usage{ 
	exportLog( message, location, error=TRUE )
	}

\arguments{
	\item{solution}{List containing at least two matrices named "umatrix" and "gmatrix" that are the standard outputs for utility and value functions.}
	\item{location}{Where you want the XML file to be saved}
	}

\value{
	No output (except the XML file)
	}

\examples{}