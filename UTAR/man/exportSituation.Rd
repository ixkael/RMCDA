\name{exportSituation}

\alias{exportSituation}

\title{Exports value functions to XMCDA file}

\description{Exports the constructed value functions (created by function \code{saveUtilityFunctionUnderXML}) into XML file according to XMCDA standard.}

\usage{ 
	exportSituation( solution, outputsLocation )
	}

\arguments{
	\item{solution}{List containing at least two matrices named "umatrix" and "gmatrix" that are the standard outputs for utility and value functions.}
	\item{outputsLocation}{Where you want the XML file to be saved}
	}

\value{
	No output (except the XML file)
	}

\examples{}