\name{extractContent}

\alias{extractContent}

\title{Extracts Content}

\description{Extracts and checks content from an \code{XML} tree (generic function).}

\usage{ 
	out <- extractContent( xml, filename, aux=NULL )
	}

\arguments{
	\item{xml}{\code{XML} tree}
	\item{filename}{Filename}
	\item{aux}{Aux file if necessary}
	}

\value{
	A list structured as follows:
	\item{content}{The appropriate result}
	\item{validation}{Validation tag (True if execution was successful)}
	\item{log}{If necessary, in case of error}
	}

\examples{}