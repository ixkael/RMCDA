\name{getValueFunctions}

\alias{getValueFunctions}

\title{Extracts value (utility) functions}

\description{Extracts utility functions from an XML parsed tree according to the \code{XMCDA} standard.}

\usage{ 
	sol <- getValueFunctions( tree , mcdaConcept = null ) 
	}
	
\arguments{
	
	\item{tree}{Object containing the \code{XMCDA} XML tree.}
	\item{mcdaConcept}{A string containing the specific \code{mcdaConcept} attribute which should be searched for.}
	
	}

\value{
	
	 The function returns a list structured as follows: 
     
	\item{gmatrix}{Matrix containing the}
	
	\item{umatrix}{}
  
  	\item{status}{Either OK or error status}
	
	}

\examples{}