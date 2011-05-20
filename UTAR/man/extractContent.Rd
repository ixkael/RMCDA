\name{extractContent}

\alias{extractContent}

\title{Extract Content}

\description{Extract and check content from an XML tree (generic function)}

\usage{ 
	content <- extractContent( xml, filename, aux=NULL )
	}

\arguments{
	\item{xml}{XML tree}
	\item{filename}{Filename}
	\item{aux}{aux file if necessary}
	}

\value{
	\item{content}{The appropriate result}
	\item{validation}{Validation tag (True if execution was successful)}
	\item{log}{if necessary, in case of error}
	}

\examples{}